library(tidyverse)
library(grid)
library(gridExtra)

source("functions/data_process_functions.R")
source("functions/data_plot_functions.R")

# pull data
pitch_level_data <- read.csv("AnalyticsQuestionnairePitchData.csv")

# clean data
cleaned_pitch_level_data <- clean_pitch_level_data(pitch_level_data)

# process data for research
pitcher_performance_data <- summarise_pitch_level_data(cleaned_pitch_level_data, group_by_var = 'PitcherId')
batter_performance_data <- summarise_pitch_level_data(cleaned_pitch_level_data, group_by_var = 'BatterId')

# all data separated
all_pitchers_df <- pitcher_performance_data[[1]]
all_pitchers_pitchtype_df <- pitcher_performance_data[[2]]
all_batters_df <- batter_performance_data[[1]]
all_batters_pitchtype_df <- batter_performance_data[[2]]

# data to csv
write.csv(all_pitchers_df, "All_Pitchers_Performance_Data.csv")
write.csv(all_pitchers_pitchtype_df, "All_Pitchers_Pitch_Performance_Data.csv")
write.csv(all_batters_df, "All_Batters_Performance_Data.csv")
write.csv(all_batters_pitchtype_df, "All_Batters_Pitch_Performance_Data.csv")

# summary to table
all_pitchers_summary <- create_pitcher_summary_table(all_pitchers_df)
all_pitchers_pitchtype_summary <- create_pitcher_pitchtype_summary_table(all_pitchers_pitchtype_df)
all_batters_summary <- create_batter_summary_table(all_batters_df)

# create PDFs for reports
# example pitcher summary
create_summary_pdf(all_pitchers_summary, "Pitcher Summary Table")

# plot data 
# use all_pitchers_pitchtype_df
pitch_general_usage_plot_function(all_pitchers_pitchtype_df)
pitch_release_to_finish_plot_function(all_pitchers_pitchtype_df)
pitcher_fastball_velocity_plot_function(all_pitchers_pitchtype_df)
pitcher_chase_rate_plot_function(all_pitchers_pitchtype_df)
pitcher_contact_rate_plot_function(all_pitchers_pitchtype_df)
all_pitchers_velo_and_spin_rate_plot_function(all_pitchers_pitchtype_df)
all_pitchers_whiff_rate_by_pitch_plot_function(all_pitchers_pitchtype_df)
# use cleaned_pitch_level_data
pitch_location_plot_function(cleaned_pitch_level_data)
pitch_movement_plot_function(cleaned_pitch_level_data)
