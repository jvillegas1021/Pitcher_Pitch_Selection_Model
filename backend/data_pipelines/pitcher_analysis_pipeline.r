run_pitcher_analysis <- function(pitcher_id) {
  
  pitcher_id <- as.character(pitcher_id)
  pitcher_scouting_report_df <- pull_pitcher_scouting_report_from_database(pitcher_id)
  
  # Data processing steps...
  pitcher_pitch_arsenal <- create_pitcher_pitch_arsenal(pitcher_scouting_report_df)
  pitcher_statcast_df <- pull_pitcher_statcast_data_from_database(pitcher_id, pitcher_pitch_arsenal)
  pitcher_pitch_usage_df <- create_pitcher_pitch_usage_df(pitcher_statcast_df)
  pitcher_pitch_characteristics_df <- create_pitcher_pitch_characteristics_df(pitcher_statcast_df)
  pitcher_pitch_performance_df <- create_pitcher_pitch_performance_df(pitcher_statcast_df)
  
  ########### Generate Plots #####################
  pitch_visual_plots          <- create_pitcher_pitch_visual_plots(pitcher_statcast_df)
  pitch_usage_plots           <- create_pitcher_usage_plots(pitcher_pitch_usage_df)
  pitch_characteristics_plots <- create_pitcher_pitch_characteristics_plots(pitcher_pitch_characteristics_df)
  #pitch_acceleration_plot     <- pitcher_radar_acceleration_plot(pitcher_pitch_characteristics_df)
  pitch_tendencies_plots      <- create_pitch_tendency_plots(pitcher_scouting_report_df)
  
  ########### Export Files / Plots to Files #####################
  
  output_dir <- paste0("output/pitcher_", pitcher_id)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  utils::write.csv(pitcher_pitch_performance_df, 
                   file = file.path(output_dir, "pitch_performance_table.csv"), 
                   row.names = FALSE)
  
  pitcher_visual_combined <- patchwork::wrap_plots(pitch_visual_plots, ncol = 2) 
  pitcher_usage_combined <- patchwork::wrap_plots(pitch_usage_plots, ncol = 2) 
  pitcher_characteristics_combined <- patchwork::wrap_plots(pitch_characteristics_plots, ncol = 2) 
  pitcher_tendencies_combined <- patchwork::wrap_plots(pitch_tendencies_plots, ncol = 2) 
  
  ggplot2::ggsave(filename = file.path(output_dir, "pitch_visuals_combined.png"), 
                  plot = pitcher_visual_combined, width = 14, height = 10, dpi = 300)
  
  ggplot2::ggsave(filename = file.path(output_dir, "pitch_usage_combined.png"), 
                  plot = pitcher_usage_combined, width = 14, height = 10, dpi = 300)
  
  ggplot2::ggsave(filename = file.path(output_dir, "pitch_characteristics_combined.png"), 
                  plot = pitcher_characteristics_combined, width = 14, height = 10, dpi = 300)
  
  ggplot2::ggsave(filename = file.path(output_dir, "pitch_tendencies_combined.png"), 
                  plot = pitcher_tendencies_combined, width = 14, height = 10, dpi = 300)
  
  
  return(invisible())
}