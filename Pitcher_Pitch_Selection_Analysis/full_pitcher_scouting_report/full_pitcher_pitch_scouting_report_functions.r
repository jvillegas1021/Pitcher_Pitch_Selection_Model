
run_pitcher_scouting_report <- function(pitcher_id, statcast_df) {
    
    pitcher_data <- statcast_df %>%
    filter(
        pitcher == pitcher_id
        )

    player_name <- pitcher_data$player_name[1]
    player_id <- pitcher_id
    # scout report
    # grab all pitch types
    
    pitch_totals <- pitcher_data %>%
        count(pitch_type, name = "total_pitch_count") %>%
        mutate(total_usage_perc = total_pitch_count / sum(total_pitch_count))
    
    valid_pitches <- pitch_totals %>%
        filter(total_usage_perc >= 0.025) %>%
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
    leverage <- 'even'
    
    situation_row_list <- list()
    
    for (tto in tto_count_list) {
        for (stance in stance_list) {
            for (runners in runners_on_list) {
                for (risp in risp_list) {
                    # runners in scoring position must mean runners on
                    if (risp && !runners) {
                        #skip
                        next
                    }
                    
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
                                    leverage,
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
                                    # Set leverage
                                    if (ball == strike || (ball == 3 & strike == 2)) {
                                        leverage <- 'even'
                                        }
                                    else if (ball > strike && !(ball == 3 & strike == 2)) {
                                        leverage <- 'behind'
                                        }
                                    else {
                                        leverage <- 'ahead'
                                        }
                                        
                                    situation_row <- build_situation_row(
                                        pitcher_data,
                                        pitcher_static_data,
                                        ball,
                                        strike,
                                        stance,
                                        runners,
                                        risp,
                                        tto,
                                        leverage,
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
    
    pitch_columns <- setdiff(names(scouting_report), c('balls', 'strikes', 'stance', 'runners_on', 'risp', 'tto', 'count_leverage', 'prev_pitch', 'prev_result'))
    
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
        all_of(pitch_columns),
        names_to = 'pitch_type',
        values_to = 'probability'
        )
    
    return (final_scouting_report)

    }

build_situation_row <- function(pitcher_df, pitcher_static_model, balls, strikes, stance, runners_on, risp, tto, leverage, prev_pitch, prev_result) {
    
    # Run prediction
    probs <- predict_pitch_probabilities(
        pitcher_df,
        pitcher_static_model,
        balls,
        strikes,
        stance,
        runners_on,
        risp,
        tto,
        leverage,
        prev_pitch,
        prev_result
    )
    
    # Build row
    row <- data.frame(
        balls = balls,
        strikes = strikes,
        stance = stance,
        runners_on = runners_on,
        risp = risp,
        tto = tto,
        count_leverage = leverage,
        prev_pitch = prev_pitch,
        prev_result = prev_result,
        stringsAsFactors = FALSE
    )
    
    # Add pitch probability columns dynamically
    for (pitch in names(probs)) {
        row[[pitch]] <- probs[[pitch]]
    }
    
    return(row)
}



predict_pitch_probabilities <- function(pitcher_df, static_model, balls = 0, strikes = 0, stance = 'R', runners_on = FALSE, runners_scoring_position = FALSE, tto = 1, leverage = 'behind', prev_pitch = NA_character_, prev_result = NA_character_) {
    
    if (balls == 0 & strikes == 0) {

        pitch_prediction <- predict_first_pitch(pitcher_df, stance, tto, runners_on, runners_scoring_position)

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
        
        pitch_count_leverage_selection_multiplier_df <- pitch_count_leverage_selection_multiplier_df(static_model, leverage)
        
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
        left_join(
            pitch_count_leverage_selection_multiplier_df,
            by='pitch_type'
            ) %>%
        mutate(
            across(ends_with("_multiplier"), ~replace_na(.x, 1))
            ) %>%
        mutate(
            raw_score = (pitch_general_usage * prev_pitch_multiplier * prev_result_multiplier * pitch_count_multiplier * pitch_handedness_multiplier * base_runners_multiplier * base_runners_scoring_position_multiplier * pitch_tto_multiplier * pitch_leverage_multiplier),
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

pitch_count_leverage_selection_multiplier_df <- function(static_model, leverage) {

    pitch_count_leverage_general_usage_df <- static_model$general_usage

    if (leverage == 'behind') {
        pitch_leverage_usage_df <- static_model$behind_count_usage
        } 
    else if (leverage == 'even') {
        pitch_leverage_usage_df <- static_model$even_count_usage
        } 
    else {
        pitch_leverage_usage_df <- static_model$ahead_count_usage
        }

    pitch_count_leverage_selection_multiplier_df <- pitch_count_leverage_general_usage_df %>%
    left_join(
        pitch_leverage_usage_df,
        by='pitch_type'
        ) %>%
    mutate(
        pitch_leverage_multiplier = pitch_leverage_usage / pitch_general_usage
        ) %>%
    select(
        pitch_type,
        pitch_leverage_multiplier
        )
    
    return(pitch_count_leverage_selection_multiplier_df)
    }



predict_first_pitch <- function(pitcher_df, stance, tto, runners_on = FALSE, risp = FALSE) {

    first_pitch_df <- first_pitch_selection_df(pitcher_df)
    
    # 1. Determine stance column
    stance_col <- if (stance == 'R') "first_pitch_vs_rhb" else "first_pitch_vs_lhb"
    
    # 2. Determine TTO column
    tto_col <- dplyr::case_when(
    tto == 1 ~ "first_pitch_usage_first_time_thru",
    tto == 2 ~ "first_pitch_usage_second_time_thru",
    tto == 3 ~ "first_pitch_usage_third_time_thru"
    )
    
    # 3. Optional context columns
    runners_col <- if (runners_on) "first_pitch_usage_runners_on_base" else NULL
    risp_col    <- if (risp)      "first_pitch_usage_runners_in_scoring_position" else NULL
    
    # 4. Build column list safely
    cols <- c(
    "pitch_name",
    "pitch_type",
    "first_pitch_general",
    stance_col,
    tto_col
    )
    
    if (!is.null(runners_col)) cols <- c(cols, runners_col)
    if (!is.null(risp_col))    cols <- c(cols, risp_col)
    
    # 5. Select only needed columns
    first_pitch_table <- first_pitch_df %>%
    select(
        all_of(cols)
    )
    
    # 6. Compute multipliers SAFELY (no unquoting of NULL)
    # ----------------------------------------------------
    # stance multiplier
    first_pitch_table$stance_mult <- first_pitch_table[[stance_col]] / first_pitch_table$first_pitch_general
    
    # TTO multiplier
    first_pitch_table$tto_mult <- first_pitch_table[[tto_col]] / first_pitch_table$first_pitch_general
    
    # runners multiplier
    if (!is.null(runners_col)) {
    first_pitch_table$runners_mult <- first_pitch_table[[runners_col]] / first_pitch_table$first_pitch_general
    } else {
    first_pitch_table$runners_mult <- 1
    }
    
    # RISP multiplier
    if (!is.null(risp_col)) {
    first_pitch_table$risp_mult <- first_pitch_table[[risp_col]] / first_pitch_table$first_pitch_general
    } else {
    first_pitch_table$risp_mult <- 1
    }
    # ----------------------------------------------------
    
    # 7. Raw score + normalization
    first_pitch_results <- first_pitch_table %>%
    mutate(
       raw_pitch_score = first_pitch_general * stance_mult * tto_mult * runners_mult * risp_mult,
      probability = round(raw_pitch_score / sum(raw_pitch_score, na.rm = TRUE) * 100, 2)
    ) %>% 
    select(
        pitch_type,
        pitch_name,
        probability
        )
    
    return(first_pitch_results)
}


first_pitch_selection_df <- function(pitcher_df) {
    
    first_pitch_df <- pitcher_df %>%
    filter(
        pitch_number == 1
        )
    
    # 1 pitch usage
    first_pitch_usage <- first_pitch_usage_general(first_pitch_df)
    # 2 pitch usage vs RHB and LHB
    first_pitch_usage_lhb_and_rhb <- first_pitch_usage_vs_lhb_and_rhb(first_pitch_df)
    # 3 pitch usage first times thru the order 1, 2, 3
    first_pitch_usage_times_thru_order <- first_pitch_usage_times_thruorder(first_pitch_df)
    
    # 5 first pitch with runners on base
    first_pitch_usage_runners_on_base <- first_pitch_usage_runners_on_base(first_pitch_df)
    
    # 6 first pitch runners in scoring position
    first_pitch_usage_runners_in_scoring_position <- first_pitch_usage_runners_in_scoring_position(first_pitch_df)
    
    # 7 first pitch same for next at bat
    first_pitch_usage_following_at_bat <- first_pitch_tendency_next_at_bat(first_pitch_df)
    
    first_pitch_usage_df <- first_pitch_usage %>%
    full_join(
        first_pitch_usage_lhb_and_rhb,
        by=c('pitch_name', 'pitch_type'),
        na_matches='na'
        ) %>%
    full_join(
        first_pitch_usage_times_thru_order,
        by=c('pitch_name', 'pitch_type'),
        na_matches='na'
        ) %>%
    full_join(
        first_pitch_usage_runners_on_base,
        by=c('pitch_name', 'pitch_type'),
        na_matches='na'
        ) %>%
    full_join(
        first_pitch_usage_runners_in_scoring_position,
        by=c('pitch_name', 'pitch_type'),
        na_matches='na'
        )

    return(first_pitch_usage_df)

    }

first_pitch_usage_general <- function(pitcher_df) {
    pitch_usage_df <- pitcher_df %>%
    group_by(
        pitch_name,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    filter(
        total_pitch_count >= 10
        ) %>%
    mutate(
        pitch_perc_usage = round(total_pitch_count / nrow(pitcher_df) * 100, 2)
        ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
    first_pitch_general = pitch_perc_usage
    )

    return(pitch_usage_df)
    }


first_pitch_usage_vs_lhb_and_rhb <- function(pitcher_df) {
    pitch_usage_lhb_df <- pitcher_df %>%
    filter(
        stand =='L'
        ) %>%
    group_by(
        pitch_name,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    filter(
        total_pitch_count >= 10
        ) %>%
    mutate(
        pitch_perc_usage = round(total_pitch_count / sum(total_pitch_count) * 100, 2)
        ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
    first_pitch_vs_lhb = pitch_perc_usage
    )

    pitch_usage_rhb_df <- pitcher_df %>%
    filter(
        stand =='R'
        ) %>%
    group_by(
        pitch_name,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    filter(
        total_pitch_count >= 10
        ) %>%
    mutate(
        pitch_perc_usage = round(total_pitch_count / sum(total_pitch_count) * 100, 2)
        ) %>%
    select(
        -total_pitch_count
        ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
    first_pitch_vs_rhb = pitch_perc_usage
    )

    pitch_usage_lhb_and_rhb_df <- pitch_usage_lhb_df %>%
    full_join(
        pitch_usage_rhb_df,
        by=c('pitch_name', 'pitch_type'),
        na_matches='na'
        )
    
    return(pitch_usage_lhb_and_rhb_df)
}

first_pitch_usage_times_thruorder <- function(pitcher_df) {
    
    pitch_usage_first_time_thru <- pitcher_df %>%
    filter(
        n_thruorder_pitcher == 1
        ) %>%
    group_by(
        pitch_name,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    filter(
        total_pitch_count >= 10
        ) %>%
    mutate(
        pitch_perc_usage = round(total_pitch_count / sum(total_pitch_count) * 100, 2)
        ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
    first_pitch_usage_first_time_thru = pitch_perc_usage
    )

    pitch_usage_second_time_thru <- pitcher_df %>%
    filter(
        n_thruorder_pitcher == 2
        ) %>%
    group_by(
        pitch_name,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    filter(
        total_pitch_count >= 10
        ) %>%
    mutate(
        pitch_perc_usage = round(total_pitch_count / sum(total_pitch_count) * 100, 2)
        ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
    first_pitch_usage_second_time_thru = pitch_perc_usage
    )


    pitch_usage_third_time_thru <- pitcher_df %>%
    filter(
        n_thruorder_pitcher == 3
        ) %>%
    group_by(
        pitch_name,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    filter(
        total_pitch_count >= 10
        ) %>%
    mutate(
        pitch_perc_usage = round(total_pitch_count / sum(total_pitch_count) * 100, 2)
        ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
    first_pitch_usage_third_time_thru = pitch_perc_usage
    )

    pitch_usage_times_thruorder <- pitch_usage_first_time_thru %>%
    full_join(
        pitch_usage_second_time_thru,
        by=c('pitch_name', 'pitch_type'),
        na_matches='na'
        ) %>%
    full_join(
        pitch_usage_third_time_thru,
        pitch_usage_rhb_df,
        by=c('pitch_name', 'pitch_type'),
        na_matches='na'
        )

    return(pitch_usage_times_thruorder)
}

    
first_pitch_usage_runners_on_base <- function(pitcher_df) {
    # 5 first pitch with runners on base
    first_pitch_usage_runners_on_base <- pitcher_df %>%
    filter(
        !is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)
        ) %>%
     group_by(
        pitch_name,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    filter(
        total_pitch_count >= 10
        ) %>%
    mutate(
        pitch_perc_usage = round(total_pitch_count / sum(total_pitch_count) * 100, 2)
        ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
    first_pitch_usage_runners_on_base = pitch_perc_usage
    )

    return(first_pitch_usage_runners_on_base)
}

first_pitch_usage_runners_in_scoring_position <- function(pitcher_df) {
    first_pitch_usage_runners_in_scoring_position <- pitcher_df %>%
    filter(
        !is.na(on_2b) | !is.na(on_3b)
        ) %>%
     group_by(
        pitch_name,
        pitch_type
        ) %>%
    summarise(
        total_pitch_count = n(),
        .groups='drop'
        ) %>%
    filter(
        total_pitch_count >= 10
        ) %>%
    mutate(
        pitch_perc_usage = round(total_pitch_count / sum(total_pitch_count) * 100, 2)
        ) %>%
    select(
        -total_pitch_count
    ) %>%
    arrange(
        desc(pitch_perc_usage)
        ) %>%
    rename(
    first_pitch_usage_runners_in_scoring_position = pitch_perc_usage
    )

    return(first_pitch_usage_runners_in_scoring_position)
}

first_pitch_tendency_next_at_bat <- function(pitcher_df) {
    
    first_pitch_of_each_ab <- pitcher_df %>%
    select(
        pitch_type,
        pitch_name,
        game_pk,
        batter,
        events,
        at_bat_number,
        pitch_number,
        n_thruorder_pitcher
        ) %>%
    group_by(
        game_pk,
        batter,
        at_bat_number
        ) %>%
    slice_min(
        order_by = pitch_number, n = 1
    ) %>%
    arrange(
        at_bat_number
        )
    
    
    last_pitch_of_each_ab <- pitcher_df %>%
    select(
        pitch_type,
        pitch_name,
        game_pk,
        batter,
        events,
        at_bat_number,
        pitch_number,
        n_thruorder_pitcher
        ) %>%
    group_by(
        game_pk,
        batter,
        at_bat_number
        ) %>%
    slice_max(
        order_by = pitch_number, n = 1
    ) %>%
    arrange(
        at_bat_number
        )
    
    combined_df <- first_pitch_of_each_ab %>%
    left_join(
        last_pitch_of_each_ab,
        by=c('game_pk', 'batter', 'at_bat_number')
        )
    
    combined_df <- combined_df %>%
    group_by(
        game_pk,
        batter
        ) %>%
    filter(
        n() >= 2
        ) %>%
    arrange(
        game_pk,
        batter,
        at_bat_number
        )
    
    cleaned_ab_df <- combined_df %>%
    rename(
        first_at_bat_pitch_type = pitch_type.x,
        first_at_bat_pitch_name = pitch_name.x,
        event_of_ab = events.y
        ) %>%
    select(
        game_pk,
        batter,
        at_bat_number,
        first_at_bat_pitch_type,
        first_at_bat_pitch_name,
        event_of_ab
        )
    
    # final table
    final_df <- cleaned_ab_df %>%
      group_by(game_pk, batter) %>%
      arrange(at_bat_number) %>%
      mutate(
        next_pitch_type = lead(first_at_bat_pitch_type),
        next_pitch_name = lead(first_at_bat_pitch_name)
      ) %>%
      ungroup() %>%
      filter(
          !is.na(next_pitch_type)
          ) %>%
      arrange(
          game_pk,
          batter)

    hit_events <- c('single', 'double', 'triple', 'home_run')

    #final table hits only
    final_hit_df <- final_df %>%
    mutate(
        same_first_pitch = if_else(first_at_bat_pitch_type == next_pitch_type, 1, 0)
        ) %>%
    filter(
        event_of_ab %in% hit_events
        )

    
    
    return(final_df)
    }



pitcher_static_model <- function(pitcher_statcast_df) {
    
    pitcher_static_data <- list(
        general_usage = pitch_selection_general_usage(pitcher_statcast_df),
        previous_pitch_usage = previous_pitch_selection_prev_pitch_usage(pitcher_statcast_df),
        previous_result_usage = previous_pitch_selection_prev_result_usage(pitcher_statcast_df),
        count_usage = pitch_count_usage(pitcher_statcast_df),
        handedness_usage = pitch_handedness_usage(pitcher_statcast_df),
        tto_usage = pitch_tto_usage(pitcher_statcast_df),
        behind_count_usage = pitch_behind_count_usage(pitcher_statcast_df),
        even_count_usage = pitch_even_count_usage(pitcher_statcast_df),
        ahead_count_usage = pitch_ahead_count_usage(pitcher_statcast_df),
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

pitch_behind_count_usage <- function(pitcher_df) {
    
    pitch_behind_count_df <- pitcher_df %>%
    filter(balls > strikes,
           !(balls == 3 & strikes == 2)
          )%>%
    group_by(
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
        pitch_leverage_usage = pitch_perc_usage
        )
    return(pitch_behind_count_df)
    }

pitch_even_count_usage <- function(pitcher_df) {
    pitch_even_count_df <- pitcher_df %>%
    filter(balls == strikes | (balls == 3 & strikes == 2),
           !(balls == 0 & strikes == 0)
          )%>%
    group_by(
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
        pitch_leverage_usage = pitch_perc_usage
        )
    return (pitch_even_count_df)
    }

pitch_ahead_count_usage <- function(pitcher_df) {

    pitch_ahead_count_df <- pitcher_df %>%
    filter(balls < strikes
          )%>%
    group_by(
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
        pitch_leverage_usage = pitch_perc_usage
        )
    return(pitch_ahead_count_df)
    }