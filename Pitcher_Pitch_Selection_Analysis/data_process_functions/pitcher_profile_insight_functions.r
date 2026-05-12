###########################INSIGHTS############################################################
lhb_insights <- function(pitcher_scounting_report_df)  {
    lhb_pitcher_scouting_report <- pitcher_scounting_report_df %>%
    filter(stance == 'L')

    lhb_count_leverage_insights <- get_count_leverage_insights(lhb_pitcher_scouting_report)
    lhb_count_leverage_runners_on_insights <- get_count_leverage_runners_on_insights(lhb_pitcher_scouting_report)
    lhb_count_leverage_runners_scoring_position_insights <- get_count_leverage_runners_scoring_position_insights(lhb_pitcher_scouting_report)
    lhb_double_up_insights <- get_double_up_insights(lhb_pitcher_scouting_report)
    lhb_follow_up_insights <- get_follow_up_insights(lhb_pitcher_scouting_report)
    lhb_first_pitch_insights <- get_first_pitch_insights(lhb_pitcher_scouting_report)
    
    return(list(lhb_count_leverage_insights,
                lhb_count_leverage_runners_on_insights,
                lhb_count_leverage_runners_scoring_position_insights,
                lhb_double_up_insights,
                lhb_follow_up_insights,
                lhb_first_pitch_insights))
    }

rhb_insights <- function(pitcher_scounting_report_df)  {
    rhb_pitcher_scouting_report <- pitcher_scounting_report_df %>%
    filter(stance == 'R')

    rhb_count_leverage_insights <- get_count_leverage_insights(rhb_pitcher_scouting_report)
    rhb_count_leverage_runners_on_insights <- get_count_leverage_runners_on_insights(rhb_pitcher_scouting_report)
    rhb_count_leverage_runners_scoring_position_insights <- get_count_leverage_runners_scoring_position_insights(rhb_pitcher_scouting_report)
    rhb_double_up_insights <- get_double_up_insights(rhb_pitcher_scouting_report)
    rhb_follow_up_insights <- get_follow_up_insights(rhb_pitcher_scouting_report)
    rhb_first_pitch_insights <- get_first_pitch_insights(rhb_pitcher_scouting_report)
    
    return(list(rhb_count_leverage_insights,
                rhb_count_leverage_runners_on_insights,
                rhb_count_leverage_runners_scoring_position_insights,
                rhb_double_up_insights,
                rhb_follow_up_insights,
                rhb_first_pitch_insights))
    }
    

get_count_leverage_insights <- function(pitcher_scouting_report_df) {
    likely_pitches_count_leverage <- pitcher_scouting_report_df %>%
    group_by(count_leverage, pitch_name) %>%
    summarise(most_likely_pitch = round(mean(probability), 2),
             .groups='drop') %>%
    group_by(count_leverage) %>%
    slice_max(most_likely_pitch, n=1)

    return(likely_pitches_count_leverage)
    }

get_count_leverage_runners_on_insights <- function(pitcher_scouting_report_df) {
    count_leverage_runners_on_df <- pitcher_scouting_report_df %>%
    filter(runners_on == TRUE) %>%
    group_by(count_leverage, pitch_name) %>%
    summarise(most_likely_pitch = round(mean(probability), 2),
             .groups='drop') %>%
    group_by(count_leverage) %>%
    slice_max(most_likely_pitch, n=1)

    return(count_leverage_runners_on_df)
    }

get_count_leverage_runners_scoring_position_insights <- function(pitcher_scouting_report_df) {
    count_leverage_runners_scoring_position_df <- pitcher_scouting_report_df %>%
    filter(risp == TRUE) %>%
    group_by(count_leverage, pitch_name) %>%
    summarise(most_likely_pitch = round(mean(probability), 2),
             .groups='drop') %>%
    group_by(count_leverage) %>%
    slice_max(most_likely_pitch, n=1)

    return(count_leverage_runners_scoring_position_df)
    }

get_double_up_insights <- function(pitcher_scouting_report_df) {
    double_up_df <- pitcher_scouting_report_df %>%
    filter(prev_pitch_name == pitch_name) %>%
    group_by(pitch_name) %>%
    summarise(
      most_likely_pitch = mean(probability),
      .groups = "drop"
    ) %>%
    slice_max(most_likely_pitch, n = 1)
    return(double_up_df)
    }

get_follow_up_insights <- function(pitcher_scouting_report_df) {
    follow_up_df <- pitcher_scouting_report_df %>%
    filter(!is.na(prev_pitch_type)) %>%
    group_by(prev_pitch_name, pitch_name) %>%
    summarise(
      most_likely_pitch = mean(probability),
      .groups = "drop"
    ) %>%
    group_by(prev_pitch_name) %>%
    slice_max(most_likely_pitch, n = 1)
    return(follow_up_df)
    }

get_first_pitch_insights <- function(pitcher_scouting_report_df) {
    first_pitch_df <- pitcher_scouting_report_df %>%
    filter(balls == 0 & strikes == 0) %>%
    group_by(pitch_name) %>%
    summarise(most_likely_pitch = round(mean(probability), 2)) %>%
    slice_max(most_likely_pitch, n=1)
    return(first_pitch_df)
    }


