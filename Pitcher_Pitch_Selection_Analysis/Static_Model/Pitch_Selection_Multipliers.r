previous_pitch_selection_multiplier_df <- function(static_model, prev_pitch, prev_result) {

    prev_list <- previous_pitch_selection_df(static_model)
    
    prev_pitch_df <- prev_list$previous_pitch_usage_df %>%
    filter(
        previous_pitch_type == prev_pitch
        ) %>%
    select(
        pitch_type,
        prev_pitch_multiplier
        )
    
    prev_result_df <- prev_list$previous_pitch_results_usage_df %>%
    filter(
        previous_result == prev_result
        ) %>%
    select(
        pitch_type,
        prev_result_multiplier
        )
    
    previous_pitch_table <- static_model$general_usage %>%
    left_join(
        prev_pitch_df,
        by='pitch_type'
        ) %>%
    left_join(
        prev_result_df,
        by='pitch_type'
        ) %>%
    select(
        pitch_type,
        prev_pitch_multiplier,
        prev_result_multiplier
        )
    
    return(previous_pitch_table)
    }

pitch_count_selection_multiplier_df <- function(static_model, balls, strikes) {
    
    pitch_general_usage_df <- static_model$general_usage

    pitch_count_usage_df <- static_model$count_usage

    count_str <- paste0(balls, "-", strikes)

    pitch_count_usage_filtered_df <- pitch_count_usage_df %>%
    filter(
        pitch_count == count_str
        )
    
    pitch_count_selection_multiplier_df <- pitch_general_usage_df %>%
    left_join(
        pitch_count_usage_filtered_df,
        by='pitch_type'
        ) %>%
    mutate(
        pitch_count_multiplier = pitch_count_usage / pitch_general_usage
        ) %>%
    select(
        pitch_type,
        pitch_count_multiplier
        )

    
    return(pitch_count_selection_multiplier_df)
}

pitch_handedness_selection_multiplier_df <- function(static_model, stance) {

    pitch_handedness_general_usage_df <- static_model$general_usage

    pitch_handedness_usage_df <- static_model$handedness_usage

    pitch_handedness_usage_filtered_df <- pitch_handedness_usage_df %>%
    filter(
        stand == stance
        )

    pitch_handedness_selection_multiplier_df <- pitch_handedness_general_usage_df %>%
    left_join(
        pitch_handedness_usage_filtered_df,
        by='pitch_type'
        ) %>%
    mutate(
        pitch_handedness_multiplier = pitch_handedness_usage / pitch_general_usage
        ) %>%
    select(
        pitch_type,
        pitch_handedness_multiplier
        )
    
    return(pitch_handedness_selection_multiplier_df)
    }

    
pitch_base_runners_selection_multiplier_df <- function(static_model, base_runners = FALSE) {

    pitch_base_runners_general_usage_df <- static_model$general_usage

    pitch_base_runners_on_usage_df <- static_model$base_runners_on_usage

    pitch_base_runners_off_usage_df <- static_model$base_runners_off_usage

    pitch_base_runners_selection_multiplier_df <- pitch_base_runners_general_usage_df %>%
    left_join(
        pitch_base_runners_on_usage_df,
        by='pitch_type'
        ) %>%
    left_join(
        pitch_base_runners_off_usage_df,
        by='pitch_type'
        ) %>%
    mutate(
        base_runners_multiplier = if (base_runners) {
            runners_on_usage / pitch_general_usage
        } else {
            runners_off_usage / pitch_general_usage
        }
        ) %>%
    select(
        pitch_type,
        base_runners_multiplier
        )
    
    return(pitch_base_runners_selection_multiplier_df)
    }

pitch_tto_selection_multiplier_df <- function(static_model, tto) {

    pitch_tto_general_usage_df <- static_model$general_usage

    pitch_tto_usage_df <- static_model$tto_usage

    pitch_tto_usage_filtered_df <- pitch_tto_usage_df %>%
    filter(
        n_thruorder_pitcher == tto
        )
    
    pitch_tto_selection_multiplier_df <- pitch_tto_general_usage_df %>%
    left_join(
        pitch_tto_usage_filtered_df,
        by='pitch_type'
        ) %>%
    mutate(
        pitch_tto_multiplier = pitch_tto_usage / pitch_general_usage
        ) %>%
    select(
        pitch_type,
        pitch_tto_multiplier
        )
    
    return(pitch_tto_selection_multiplier_df)
    }

pitch_base_runners_scoring_position_selection_multiplier_df <- function(static_model, base_runners_scoring_position = FALSE) {

    pitch_base_runners_general_usage_df <- static_model$general_usage

    pitch_base_runners_scoring_position_usage_df <- static_model$base_runners_scoring_position_usage

    pitch_base_runners_not_scoring_position_usage_df <- static_model$base_runners_not_scoring_position_usage    

    pitch_base_runners_scoring_position_selection_multiplier_df <- pitch_base_runners_general_usage_df %>%
    left_join(
        pitch_base_runners_scoring_position_usage_df,
        by='pitch_type'
        ) %>%
    left_join(
        pitch_base_runners_not_scoring_position_usage_df,
        by='pitch_type'
        ) %>%
    mutate(
        base_runners_scoring_position_multiplier = if (base_runners_scoring_position) {
            runners_scoring_position_usage / pitch_general_usage
        } else {
            runners_not_scoring_position_usage / pitch_general_usage
        }
        ) %>%
    select(
        pitch_type,
        base_runners_scoring_position_multiplier
        )
    
    return(pitch_base_runners_scoring_position_selection_multiplier_df)
    }

