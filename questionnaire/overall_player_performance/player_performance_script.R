library(tidyverse)

source("data_process_functions.R")
source("data_plot_functions.R")

# pull data
pitch_level_data <- read.csv("AnalyticsQuestionnairePitchData.csv")

# clean data
cleaned_pitch_level_data <- clean_pitch_level_data(pitch_level_data)

# process data for research
pitcher_performance_data <- summarise_pitch_level_data(cleaned_pitch_level_data, group_by_var = 'PitcherId')
batter_performance_data <- summarise_pitch_level_data(cleaned_pitch_level_data, group_by_var = 'BatterId')


# results

all_pitchers_data <- pitcher_performance_data[[1]]
all_pitchers_pitch_level_data <- pitcher_performance_data[[2]]


all_batters_data <- batter_performance_data[[1]]
all_batters_pitch_level_data <- batter_performance_data[[2]]
