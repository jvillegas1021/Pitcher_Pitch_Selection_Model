run_pitcher_scouting_report_builder <- function() {
  
  statcast_data <- get_data_from_database("statcast_2023_2026")
  
  
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
  
  # Ensure the cache directory exists before running anything
  if (!dir.exists("cache")) {
    dir.create("cache", recursive = TRUE)
  }
  
  pitcher_id_list <- statcast_data_grouped$pitcher
  
  pitcher_batches <- split(pitcher_id_list, ceiling(seq_along(pitcher_id_list) / batch_size))
  
  batch_index_list <- seq_along(pitcher_batches)
  
  all_pitcher_report_list <- list()
  
  # Single Loop Pipeline: Handles checking, generating, caching, and collecting
  for (batch in batch_index_list) {
    
    message("Working on Batch : ", batch)
    
    current_batch <- pitcher_batches[[batch]]
    
    for (pitcher in current_batch) {
      
      cache_file <- paste0("cache/pitcher_", pitcher, ".rds")
      
      if (file.exists(cache_file)) {
        message("Grabbing cached pitcher report for: ", pitcher)
        pitcher_report <- readRDS(cache_file)
      } else {
        message("Running and caching report on: ", pitcher)
        
        # Wrap in tryCatch to prevent one broken player from crashing the whole run
        pitcher_report <- tryCatch({
          run_pitcher_scouting_report(pitcher, statcast_data_ordered)
        }, error = function(e) {
          message("❌ Error processing pitcher ", pitcher, ": ", e$message)
          return(NULL)
        })
        
        # Only save and append if the report succeeded
        if (!is.null(pitcher_report)) {
          saveRDS(pitcher_report, cache_file)
        }
      }
      
      # Append the report to our list if it exists
      if (!is.null(pitcher_report)) {
        all_pitcher_report_list[[length(all_pitcher_report_list) + 1]] <- pitcher_report
      }
    }  
  }
  
  all_pitcher_reports_df <- bind_rows(all_pitcher_report_list)
  
  pitch_names <- statcast_data %>%
    distinct(
      pitch_type,
      pitch_name
    )
  
  final_all_pitcher_reports_df <- all_pitcher_reports_df %>%
    left_join(
      pitch_names,
      by = "pitch_type"
    ) %>%
    left_join(
      pitch_names %>%
        rename(
          prev_pitch = pitch_type,
          prev_pitch_name = pitch_name
        ),
      by = c("prev_pitch" = "prev_pitch")
    ) %>%
    relocate(
      pitch_name, .before = probability
    ) %>%
    relocate(
      prev_pitch_name, .after = prev_pitch
    ) %>%
    rename(
      prev_pitch_type = prev_pitch
    )
  
  sql_table_name <- 'pitcher_scouting_reports'
  
  write_df_to_sql_replace(sql_table_name, final_all_pitcher_reports_df)
  
  return(invisible())
}