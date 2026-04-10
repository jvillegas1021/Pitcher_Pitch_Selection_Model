
predict_pitch_probabilities <- function(pitcher_df, static_model, balls = 0, strikes = 0, stance = 'R', runners_on = FALSE, runners_scoring_position = FALSE, tto = 1, prev_pitch = NA_character_, prev_result = NA_character_) {
    
    if (balls == 0 & strikes == 0) {

        pitch_prediction <- predict_first_pitch(pitcher_df, stance, tto, runners_on, risp)

        # Convert to named numeric vector
        probs <- setNames(pitch_prediction$probability, pitch_prediction$pitch_type)
        
        return(probs)

        
        } else {

        pitch_general_usage_selection_multiplier_df <- static_model$general_usage
        
        previous_pitch_selection_multiplier_df <- previous_pitch_selection_multiplier_df(static_model, prev_pitch, prev_result)
        
        pitch_count_selection_multiplier_df <- pitch_count_selection_multiplier_df(static_model, balls, strikes)

        pitch_handedness_selection_multiplier_df <- pitch_handedness_selection_multiplier_df(static_model, stance)

        pitch_base_runners_selection_multiplier_df <- pitch_base_runners_selection_multiplier_df(static_model, runners_on)

        pitch_base_runners_scoring_position_selection_multiplier_df <- pitch_base_runners_scoring_position_selection_multiplier_df(static_model, runners_scoring_position)
        
        pitch_tto_selection_multiplier_df <- pitch_tto_selection_multiplier_df(static_model, tto)


        pitch_prediction_df <- pitch_general_usage_selection_multiplier_df %>%
        left_join(
            previous_pitch_selection_multiplier_df,
            by='pitch_type'
            ) %>%
        left_join(
            pitch_count_selection_multiplier_df,
            by='pitch_type'
            ) %>%
        left_join(
            pitch_handedness_selection_multiplier_df,
            by='pitch_type'
            ) %>%
        left_join(
            pitch_base_runners_selection_multiplier_df,
            by='pitch_type'
            ) %>%
        left_join(
            pitch_base_runners_scoring_position_selection_multiplier_df,
            by='pitch_type'
            ) %>%
        left_join(
            pitch_tto_selection_multiplier_df,
            by='pitch_type'
            ) %>%
        mutate(
            raw_score = (pitch_general_usage * prev_pitch_multiplier * prev_result_multiplier * pitch_count_multiplier * pitch_handedness_multiplier * base_runners_multiplier * base_runners_scoring_position_multiplier * pitch_tto_multiplier),
            probability = round(raw_score / sum(raw_score, na.rm=TRUE) * 100, 2)
            ) %>%
        select(
            pitch_type,
            probability
            )

        probs <- setNames(pitch_prediction_df$probability, pitch_prediction_df$pitch_type)
        
        return(probs)

        }
    }

