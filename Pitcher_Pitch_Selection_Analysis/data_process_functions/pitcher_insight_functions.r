###########################INSIGHTS############################################################
create_lhb_and_rhb_insights <- function(pitcher_scounting_report_df)  {
    
    lhb_pitcher_scouting_report <- pitcher_scounting_report_df %>%
    filter(stance == 'L')

    rhb_pitcher_scouting_report <- pitcher_scounting_report_df %>%
    filter(stance == 'R')

    lhb_count_leverage_insights <- get_count_leverage_insights(lhb_pitcher_scouting_report)
    lhb_count_leverage_runners_on_insights <- get_count_leverage_runners_on_insights(lhb_pitcher_scouting_report)
    lhb_count_leverage_runners_scoring_position_insights <- get_count_leverage_runners_scoring_position_insights(lhb_pitcher_scouting_report)
    lhb_double_up_insights <- get_double_up_insights(lhb_pitcher_scouting_report)
    lhb_follow_up_insights <- get_follow_up_insights(lhb_pitcher_scouting_report)
    lhb_first_pitch_insights <- get_first_pitch_insights(lhb_pitcher_scouting_report)

    rhb_count_leverage_insights <- get_count_leverage_insights(rhb_pitcher_scouting_report)
    rhb_count_leverage_runners_on_insights <- get_count_leverage_runners_on_insights(rhb_pitcher_scouting_report)
    rhb_count_leverage_runners_scoring_position_insights <- get_count_leverage_runners_scoring_position_insights(rhb_pitcher_scouting_report)
    rhb_double_up_insights <- get_double_up_insights(rhb_pitcher_scouting_report)
    rhb_follow_up_insights <- get_follow_up_insights(rhb_pitcher_scouting_report)
    rhb_first_pitch_insights <- get_first_pitch_insights(rhb_pitcher_scouting_report)

    rhb_early_count_insights <- get_early_count_probabilites(rhb_pitcher_scouting_report)
    rhb_late_count_insights <- get_late_count_probabilites(rhb_pitcher_scouting_report)

    lhb_early_count_insights <- get_early_count_probabilites(lhb_pitcher_scouting_report)
    lhb_late_count_insights <- get_late_count_probabilites(lhb_pitcher_scouting_report)

    rhb_after_hard_stuff_insights <- get_after_hard_stuff_probabilities(rhb_pitcher_scouting_report)
    rhb_after_soft_stuff_insights <- get_after_soft_stuff_probabilities(rhb_pitcher_scouting_report)
    lhb_after_hard_stuff_insights <- get_after_hard_stuff_probabilities(lhb_pitcher_scouting_report)
    lhb_after_soft_stuff_insights <- get_after_soft_stuff_probabilities(lhb_pitcher_scouting_report)
    
    
    return(list(lhb_count_leverage = lhb_count_leverage_insights,
                lhb_runners_on = lhb_count_leverage_runners_on_insights,
                lhb_scoring_position = lhb_count_leverage_runners_scoring_position_insights,
                lhb_double_up = lhb_double_up_insights,
                lhb_follow_up = lhb_follow_up_insights,
                lhb_first_pitch = lhb_first_pitch_insights,
                rhb_count_leverage = rhb_count_leverage_insights,
                rhb_runners_on = rhb_count_leverage_runners_on_insights,
                rhb_scoring_position = rhb_count_leverage_runners_scoring_position_insights,
                rhb_double_up = rhb_double_up_insights,
                rhb_follow_up = rhb_follow_up_insights,
                rhb_first_pitch = rhb_first_pitch_insights,
                rhb_early_count = rhb_early_count_insights,
                rhb_late_count = rhb_late_count_insights,
                lhb_early_count = lhb_early_count_insights,
                lhb_late_count = lhb_late_count_insights,
                rhb_after_hard_stuff = rhb_after_hard_stuff_insights,
                rhb_after_soft_stuff = rhb_after_soft_stuff_insights,
                lhb_after_hard_stuff = lhb_after_hard_stuff_insights,
                lhb_after_soft_stuff = lhb_after_soft_stuff_insights
               ))
    }

    

get_count_leverage_insights <- function(pitcher_scouting_report_df) {
    likely_pitches_count_leverage <- pitcher_scouting_report_df %>%
    group_by(count_leverage, pitch_name) %>%
    summarise(probability = round(mean(probability), 2),
             .groups='drop') %>%
    group_by(count_leverage) %>%
    slice_max(probability, n=1)

    return(likely_pitches_count_leverage)
    }

get_count_leverage_runners_on_insights <- function(pitcher_scouting_report_df) {
    count_leverage_runners_on_df <- pitcher_scouting_report_df %>%
    filter(runners_on == TRUE) %>%
    group_by(count_leverage, pitch_name) %>%
    summarise(probability = round(mean(probability), 2),
             .groups='drop') %>%
    group_by(count_leverage) %>%
    slice_max(probability, n=1)

    return(count_leverage_runners_on_df)
    }

get_count_leverage_runners_scoring_position_insights <- function(pitcher_scouting_report_df) {
    count_leverage_runners_scoring_position_df <- pitcher_scouting_report_df %>%
    filter(risp == TRUE) %>%
    group_by(count_leverage, pitch_name) %>%
    summarise(probability = round(mean(probability), 2),
             .groups='drop') %>%
    group_by(count_leverage) %>%
    slice_max(probability, n=1)

    return(count_leverage_runners_scoring_position_df)
    }

get_double_up_insights <- function(pitcher_scouting_report_df) {
    double_up_df <- pitcher_scouting_report_df %>%
    filter(prev_pitch_name == pitch_name) %>%
    group_by(pitch_name) %>%
    summarise(
      probability = round(mean(probability), 2),
      .groups = "drop"
    ) %>%
    slice_max(probability, n = 1)
    return(double_up_df)
    }

get_follow_up_insights <- function(pitcher_scouting_report_df) {
    follow_up_df <- pitcher_scouting_report_df %>%
    filter(!is.na(prev_pitch_type)) %>%
    group_by(prev_pitch_name, pitch_name) %>%
    summarise(
      probability = round(mean(probability), 2),
      .groups = "drop"
    ) %>%
    group_by(prev_pitch_name) %>%
    slice_max(probability, n = 1)
    return(follow_up_df)
    }

get_first_pitch_insights <- function(pitcher_scouting_report_df) {
    first_pitch_df <- pitcher_scouting_report_df %>%
    filter(balls == 0 & strikes == 0) %>%
    group_by(pitch_name) %>%
    summarise(probability = round(mean(probability), 2)) %>%
    slice_max(probability, n=1)
    return(first_pitch_df)
    }

get_early_count_probabilites <- function(pitcher_scouting_report_df) {
    early_count_df <- pitcher_scouting_report_df %>%
    filter(balls + strikes <= 2) %>%
    group_by(pitch_name) %>%
    summarise(probability = round(mean(probability), 2))
    return(early_count_df)
    }

get_late_count_probabilites <- function(pitcher_scouting_report_df) {
    late_count_df <- pitcher_scouting_report_df %>%
    filter(balls + strikes >= 3) %>%
    group_by(pitch_name) %>%
    summarise(probability = round(mean(probability), 2))
    return(late_count_df)
    }

get_after_hard_stuff_probabilities <- function(pitcher_scouting_report_df) {
    hard_pitch_types <- c('FF', 'FT', 'SI', 'FC')
    after_hard_stuff_df <- pitcher_scouting_report_df %>%
    filter(prev_pitch_type %in% hard_pitch_types) %>%
    group_by(pitch_name) %>%
    summarise(probability = round(mean(probability), 2)) %>%
    arrange(desc(probability))
    return(after_hard_stuff_df)
    }

get_after_soft_stuff_probabilities <- function(pitcher_scouting_report_df) {
    hard_pitch_types <- c('FF', 'FT', 'SI', 'FC')
    after_soft_stuff_df <- pitcher_scouting_report_df %>%
    filter(!(prev_pitch_type %in% hard_pitch_types)) %>%
    group_by(pitch_name) %>%
    summarise(probability = round(mean(probability), 2)) %>%
    arrange(desc(probability))
    return(after_soft_stuff_df)
    }
