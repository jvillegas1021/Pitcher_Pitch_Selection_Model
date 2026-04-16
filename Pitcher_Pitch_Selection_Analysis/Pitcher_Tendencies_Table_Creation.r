library(DBI)
library(RPostgres)
library(tidyverse)
library(glue)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)

source("Pitcher_Pitch_Selection_Static_Data.r")
source("Pitch_Selection_Multipliers.r")
source("Pitcher_First_Pitch_Prediction.r")
source("Pitch_Prediction.r")
source("Scouting_Report.r")
source("Full_Pitcher_Pitch_Scouting_Report.r")



statcast_data <- read_csv("C:/Users/james.villegas/OneDrive - Rotork plc/Desktop/Baseball/statcast_25_26.csv", show_col_types = FALSE)

# clean data

statcast_data_cleaned <- statcast_data %>%
drop_na(
    pitch_type
    )

# re arrange 
statcast_data_ordered <- statcast_data_cleaned %>%
arrange(
        game_date,
        game_pk,
        at_bat_number,
        pitch_number
        )

# filter out <= 1000 pitches, not qualified pitchers

statcast_data_grouped <- statcast_data_ordered %>%
group_by(
    pitcher
    ) %>%
summarise(
    count = n()
    ) %>%
filter(
    count >= 1000
    ) %>%
select(
    pitcher
    )

# order data correctly to make sure in order

batch_size <- 50

pitcher_id_list <- statcast_data_grouped$pitcher

pitcher_batches <- split(pitcher_id_list, ceiling(seq_along(pitcher_id_list) / batch_size))

batch_index_list <- seq(1, length(names(pitcher_batches)))

for (batch in batch_index_list) {
    
    message("Working on Batch : ", batch)
    
    current_batch <- pitcher_batches[[batch]]
    
    for (pitcher in current_batch) {
    
        cache_file <- paste0("cache/pitcher_", pitcher, ".rds")
    
        if (file.exists(cache_file)) {
            message("Skipping cached pitcher: ", pitcher)
            next
        }
        
        message("Running report on: ", pitcher)
        
        pitchers_report <- run_pitcher_scouting_report(pitcher, statcast_data_ordered)
    
        saveRDS(pitchers_report, cache_file)
        }  
}


all_pitcher_report_list <- list()


for (batch in batch_index_list) {

    current_batch <- pitcher_batches[[batch]]

    message("Working on batch : ", batch)

    for (pitcher in current_batch) {

        cache_file <- paste0("cache/pitcher_", pitcher, ".rds")
        
        if (!file.exists(cache_file)) {
            next
        }
        message("Grabbing pitcher report for :", pitcher)
        
        pitcher_report <- readRDS(cache_file)
        
        all_pitcher_report_list[[length(all_pitcher_report_list) + 1]] <- pitcher_report
    }
}
