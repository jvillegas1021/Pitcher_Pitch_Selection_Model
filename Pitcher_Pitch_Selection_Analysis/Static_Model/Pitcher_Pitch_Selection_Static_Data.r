
pitcher_static_model <- function(pitcher_statcast_df) {
    
    pitcher_static_data <- list(
        general_usage = pitch_selection_general_usage(pitcher_statcast_df),
        previous_pitch_usage = previous_pitch_selection_prev_pitch_usage(pitcher_statcast_df),
        previous_result_usage = previous_pitch_selection_prev_result_usage(pitcher_statcast_df),
        count_usage = pitch_count_usage(pitcher_statcast_df),
        handedness_usage = pitch_handedness_usage(pitcher_statcast_df),
        tto_usage = pitch_tto_usage(pitcher_statcast_df),
        base_runners_on_usage = pitch_base_runners_on_usage(pitcher_statcast_df),
        base_runners_off_usage = pitch_base_runners_off_usage(pitcher_statcast_df),
        base_runners_scoring_position_usage = pitch_base_runners_scoring_position_usage(pitcher_statcast_df),
        base_runners_not_scoring_position_usage = pitch_base_runners_not_scoring_position_usage(pitcher_statcast_df)
        )
    
    return(pitcher_static_data)
    }


pitch_selection_general_usage <- function(pitcher_df) {
    
    pitch_general_usage_df <- pitcher_df %>%
    group_by(
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    mutate(
        pitch_perc_usage = total_pitch_count / sum(total_pitch_count)
        ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
        pitch_general_usage = pitch_perc_usage
        )
    return(pitch_general_usage_df)
    }

previous_pitch_selection_prev_pitch_usage <- function(pitcher_df) {
    
    prev_pitch_usage_df <- pitcher_df %>%
    arrange(
        game_date,
        game_pk,
        at_bat_number,
        pitch_number
        ) %>%
    group_by(
        game_date,
        game_pk,
        at_bat_number
        ) %>%
    mutate(
        previous_pitch_type = lag(pitch_type)
        ) %>%
    ungroup() %>%
    filter(
        !is.na(previous_pitch_type)
        ) %>%
    group_by(
        previous_pitch_type,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop_last'
        ) %>%
    mutate(
        pitch_perc_usage = total_pitch_count / sum(total_pitch_count)
    ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        previous_pitch_type
        ) %>%
    rename(
        prev_pitch_prev_pitch_usage = pitch_perc_usage
        )

    return(prev_pitch_usage_df)
    }

previous_pitch_selection_prev_result_usage <- function(pitcher_df) {

    prev_result_usage_df <- pitcher_df %>%
    
    arrange(
        game_date,
        game_pk,
        at_bat_number,
        pitch_number
        ) %>%
    group_by(
        game_date,
        game_pk,
        at_bat_number
        ) %>%
    mutate(
        previous_result = lag(type)
        ) %>%
        ungroup() %>%
    filter(
        !is.na(previous_result)
        ) %>%
    group_by(
        previous_result,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop_last'
        ) %>%
    mutate(
        pitch_perc_usage = total_pitch_count / sum(total_pitch_count)
    ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        previous_result
        ) %>%
    rename(
        prev_pitch_results_usage = pitch_perc_usage
        )
    
    return(prev_result_usage_df)
    }

previous_pitch_selection_df <- function(static_model) {
    
    prev_pitch_general_usage_df <- static_model$general_usage
    prev_pitch_usage_df <- static_model$previous_pitch_usage
    prev_results_usage_df <- static_model$previous_result_usage

    
    prev_pitch_general_and_prev_pitch_df <- prev_pitch_general_usage_df %>%
    left_join(
        prev_pitch_usage_df,
        by='pitch_type'
        ) %>%
    mutate(
        prev_pitch_multiplier = prev_pitch_prev_pitch_usage / pitch_general_usage
        )
    
    prev_pitch_general_and_prev_result_df <- prev_pitch_general_usage_df %>%
    left_join(
        prev_results_usage_df,
        by='pitch_type'
        ) %>%
    mutate(
        prev_result_multiplier = prev_pitch_results_usage / pitch_general_usage
        )

    return(
        list(
            previous_pitch_usage_df = prev_pitch_general_and_prev_pitch_df,
            previous_pitch_results_usage_df = prev_pitch_general_and_prev_result_df
            )
        )
    }


pitch_count_usage <- function(pitcher_df) {

    pitch_count_usage <- pitcher_df %>%
    filter(
        !(balls == 0 & strikes == 0) 
        ) %>%
    group_by(
        balls,
        strikes,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop_last'
        ) %>%
    mutate(
        pitch_count = paste0(balls, "-", strikes)
        ) %>%
    group_by(
        pitch_count
    ) %>%
    mutate(
        pitch_count_usage = total_pitch_count / sum(total_pitch_count)
        ) %>%
    ungroup()

    
    return(pitch_count_usage)
    }


pitch_handedness_usage <- function(pitcher_df) {
    pitch_handedness_usage_df <- pitcher_df %>%
    group_by(
        stand,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop_last'
        ) %>%
    mutate(
        pitch_perc_usage = total_pitch_count / sum(total_pitch_count)
        ) %>%
    select(
        -total_pitch_count
        ) %>%
    arrange(
        pitch_perc_usage
        ) %>%
    rename(
        pitch_handedness_usage = pitch_perc_usage
        )
    return(pitch_handedness_usage_df)
    }


pitch_base_runners_on_usage <- function(pitcher_df) {
    pitch_base_runners_on_usage_df <- pitcher_df %>%
    filter(
        (!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b))
        )%>%
    group_by(
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    mutate(
        pitch_perc_usage = total_pitch_count / sum(total_pitch_count)
        ) %>%
    select(
        -total_pitch_count
        ) %>%
    arrange(
        pitch_perc_usage
        ) %>%
    rename(
        runners_on_usage = pitch_perc_usage
        )
        
    return(pitch_base_runners_on_usage_df)
    }

pitch_base_runners_off_usage <- function(pitcher_df) {
    pitch_base_runners_off_usage_df <- pitcher_df %>%
    filter(
        (is.na(on_1b) & is.na(on_2b) & is.na(on_3b))
        )%>%
    group_by(
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    mutate(
        pitch_perc_usage = total_pitch_count / sum(total_pitch_count)
        ) %>%
    select(
        -total_pitch_count
        ) %>%
    arrange(
        pitch_perc_usage
        ) %>%
    rename(
        runners_off_usage = pitch_perc_usage
        )
        
    return(pitch_base_runners_off_usage_df)
    }

pitch_base_runners_scoring_position_usage <- function(pitcher_df) {
    pitch_base_runners_scoring_position_usage_df <- pitcher_df %>%
    filter(
        !is.na(on_2b) | !is.na(on_3b)
        ) %>%
    group_by(
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    mutate(
        pitch_perc_usage = total_pitch_count / sum(total_pitch_count)
        ) %>%
    select(
        -total_pitch_count
        ) %>%
    arrange(
        pitch_perc_usage
        ) %>%
    rename(
        runners_scoring_position_usage = pitch_perc_usage
        )
        
    return(pitch_base_runners_scoring_position_usage_df)
    }

pitch_base_runners_not_scoring_position_usage <- function(pitcher_df) {
    pitch_base_runners_not_scoring_position_usage_df <- pitcher_df %>%
    filter(
        !is.na(on_1b) & is.na(on_2b) & is.na(on_3b)
        ) %>%
    group_by(
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    mutate(
        pitch_perc_usage = total_pitch_count / sum(total_pitch_count)
        ) %>%
    select(
        -total_pitch_count
        ) %>%
    arrange(
        pitch_perc_usage
        ) %>%
    rename(
        runners_not_scoring_position_usage = pitch_perc_usage
        )
        
    return(pitch_base_runners_not_scoring_position_usage_df)
    }

pitch_tto_usage <- function(pitcher_df) {
    
    pitch_tto_usage_df <- pitcher_df %>%
    group_by(
        pitch_type,
        n_thruorder_pitcher
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    mutate(
        pitch_perc_usage = total_pitch_count / sum(total_pitch_count)
        ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
        pitch_tto_usage = pitch_perc_usage
        )
    return(pitch_tto_usage_df)
    }
