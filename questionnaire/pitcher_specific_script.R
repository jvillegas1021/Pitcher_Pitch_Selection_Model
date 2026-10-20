# Pitcher ID Specific Plots Script

library(tidyverse)
library(RColorBrewer)
library(scales)

source("data_process_functions.R")
source("data_plot_functions.R")
source("pitcher_specific_data_functions.R")
source("pitcher_specific_plot_functions.R")


# pull data
pitch_level_data <- read.csv("AnalyticsQuestionnairePitchData.csv")

# clean data
cleaned_pitch_level_data <- clean_pitch_level_data(pitch_level_data)

# pitcher ID 1 to evaluate
pitcher_id <- 1

# Data processing steps
pitcher_statcast_df <- cleaned_pitch_level_data %>% filter(PitcherId == pitcher_id)
pitcher_pitch_usage_df <- create_pitcher_pitch_usage_df(pitcher_statcast_df)
pitcher_pitch_characteristics_df <- create_pitcher_pitch_characteristics_df(pitcher_statcast_df)

########### Generate Plots #####################
pitch_visual_plots <- create_pitcher_pitch_visual_plots(pitcher_statcast_df)
pitch_usage_plots <- create_pitcher_usage_plots(pitcher_pitch_usage_df)
pitch_characteristics_plots <- create_pitcher_pitch_characteristics_plots(pitcher_pitch_characteristics_df)

########### Export Files / Plots to Files #####################

output_dir <- paste0("output/pitcher_", pitcher_id)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

pitcher_visual_combined <- patchwork::wrap_plots(pitch_visual_plots, ncol = 2) 
pitcher_usage_combined <- patchwork::wrap_plots(pitch_usage_plots, ncol = 2)
pitcher_characteristics_combined <- patchwork::wrap_plots(pitch_characteristics_plots, ncol = 2) 

ggplot2::ggsave(filename = file.path(output_dir, "pitch_visuals_combined.png"), 
                plot = pitcher_visual_combined, width = 14, height = 10, dpi = 300)

ggplot2::ggsave(filename = file.path(output_dir, "pitch_usage_combined.png"), 
                plot = pitcher_usage_combined, width = 14, height = 10, dpi = 300)

ggplot2::ggsave(filename = file.path(output_dir, "pitch_characteristics_combined.png"), 
                plot = pitcher_characteristics_combined, width = 14, height = 10, dpi = 300)


