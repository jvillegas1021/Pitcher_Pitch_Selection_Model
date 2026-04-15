
run_pitcher_scouting_report <- function(pitcher_id, statcast_df) {
    
    pitcher_data <- statcast_df %>%
    filter(
        pitcher == pitcher_id
        )

    player_name <- pitcher_data$player_name[1]
    
    # scout report
    # grab all pitch types
    
    pitch_totals <- pitcher_data %>%
        count(pitch_type, name = "total_pitch_count") %>%
        mutate(total_usage_perc = total_pitch_count / sum(total_pitch_count))
    
    valid_pitches <- pitch_totals %>%
        filter(total_usage_perc >= 0.10) %>%
        pull(pitch_type)
    
    pitcher_data <- pitcher_data %>%
        filter(pitch_type %in% valid_pitches)
    
    pitch_types <- pitcher_data %>%
    distinct(pitch_type)
    
    pitcher_static_data <- pitcher_static_model(pitcher_data)
    
    ball_count_list <- seq(0,3)
    strike_count_list <- seq(0,2)
    tto_count_list <- seq(1,2)
    stance_list <- c('R', 'L')
    runners_on_list <- c(FALSE, TRUE)
    risp_list <- c(FALSE, TRUE)
    prev_result_list <- c('B', 'S')
    prev_pitch_list <- pitch_types$pitch_type
    
    situation_row_list <- list()
    
    for (tto in tto_count_list) {
        for (stance in stance_list) {
            for (runners in runners_on_list) {
                for (risp in risp_list) {
                    for (ball in ball_count_list) {
                        for (strike in strike_count_list) {
    
                            # FIRST-PITCH CASE
                            if (ball == 0 && strike == 0) {
    
                                situation_row <- build_situation_row(
                                    pitcher_data,
                                    pitcher_static_data,
                                    ball,
                                    strike,
                                    stance,
                                    runners,
                                    risp,
                                    tto,
                                    NA_character_,
                                    NA_character_
                                )
    
                                situation_row_list[[length(situation_row_list) + 1]] <- situation_row
                                next   # ← THIS IS THE KEY
                            }
    
                            # ALL OTHER COUNTS
                            for (prev_pitch in prev_pitch_list) {
                                for (prev_result in prev_result_list) {
    
                                    # Skip illegal states
                                    if (ball > 0 && strike == 0 && prev_result == "S") next
                                    if (ball == 0 && strike > 0 && prev_result == "B") next
    
                                    situation_row <- build_situation_row(
                                        pitcher_data,
                                        pitcher_static_data,
                                        ball,
                                        strike,
                                        stance,
                                        runners,
                                        risp,
                                        tto,
                                        prev_pitch,
                                        prev_result
                                    )
    
                                    situation_row_list[[length(situation_row_list) + 1]] <- situation_row
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    scouting_report <- bind_rows(situation_row_list)
    
    pitch_columns <- setdiff(names(scouting_report), c('balls', 'strikes', 'stance', 'runners_on', 'risp', 'tto', 'prev_pitch', 'prev_result'))
    
    scouting_report_cleaned <- scouting_report %>%
    filter(
        !if_all(all_of(pitch_columns), is.na)
        )
    
    scouting_report_cleaned <- scouting_report_cleaned %>%
        mutate(across(all_of(pitch_columns), ~replace_na(.x, 0)))
    
    scouting_report_cleaned <- scouting_report_cleaned %>%
    mutate(
        pitcher_id = player_id,
        pitcher_name = player_name,
        situation_id = row_number()
    )
    
    scouting_report_cleaned <- scouting_report_cleaned %>%
    relocate(
        pitcher_id, .before=balls
        ) %>%
    relocate(
        pitcher_name, .after=pitcher_id
        ) %>%
    relocate(
        situation_id, .after=pitcher_name
        )
    
    final_scouting_report <- scouting_report_cleaned %>%
    pivot_longer(
        pitch_columns,
        names_to = 'pitch_type',
        values_to = 'probability'
        )
    
    return (final_scouting_report)

    }