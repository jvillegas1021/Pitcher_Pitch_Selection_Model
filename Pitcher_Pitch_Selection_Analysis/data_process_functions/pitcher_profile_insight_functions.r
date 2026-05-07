###########################INSIGHTS############################################################
lhb_insights <- function(lhb_pitcher_scounting_report_df)  {
    return
    }

rhb_insights <- function(rhb_pitcher_scounting_report_df)  {
    return
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
