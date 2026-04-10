

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
    