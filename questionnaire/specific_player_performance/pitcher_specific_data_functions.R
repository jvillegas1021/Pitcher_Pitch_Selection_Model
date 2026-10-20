

create_pitcher_pitch_arsenal <- function(pitcher_statcast_df) {
  pitch_type_list <- unique(pitcher_statcast_df$PitchType)
  return(pitch_type_list)
}

create_pitcher_pitch_usage_df <- function(pitcher_statcast_df) {
  pitch_general_usage_df <- pitcher_statcast_df %>%
    group_by(PitchType) %>%
    summarise(total_count = n(),
              .groups='drop'
    ) %>%
    mutate(pitch_usage = round(total_count / sum(total_count) * 100, 2)) %>%
    select(PitchType,
           pitch_usage
    )
  
  pitch_usage_vs_rhb_df <- pitcher_statcast_df %>%
    filter(BatterSide == 'R') %>%
    group_by(PitchType) %>%
    summarise(total_count = n(),
              .groups='drop'
    ) %>%
    mutate(pitch_usage = round(total_count / sum(total_count) * 100, 2)) %>%
    select(PitchType,
           pitch_usage
    )
  
  pitch_usage_vs_lhb_df <- pitcher_statcast_df %>%
    filter(BatterSide == 'L') %>%
    group_by(PitchType) %>%
    summarise(total_count = n(),
              .groups='drop'
    ) %>%
    mutate(pitch_usage = round(total_count / sum(total_count) * 100, 2)) %>%
    select(PitchType,
           pitch_usage
    )
  
  return(list(pitch_general_usage_df,
              pitch_usage_vs_rhb_df,
              pitch_usage_vs_lhb_df))
}

create_pitcher_pitch_characteristics_df <- function(pitcher_statcast_df) {
  pitch_general_characteristics_df <- pitcher_statcast_df %>%
    group_by(PitchType) %>%
    summarise(
      avg_velo = round(mean(ReleaseSpeed, na.rm = TRUE), 2),
      avg_rel_x = round(mean(ReleasePositionX, na.rm = TRUE), 2),
      avg_rel_y = round(mean(ReleasePositionY, na.rm = TRUE), 2),
      avg_rel_z = round(mean(ReleasePositionZ, na.rm = TRUE), 2),
      avg_plate_x = round(mean(TrajectoryLocationX, na.rm = TRUE), 2),
      avg_plate_z = round(mean(TrajectoryLocationZ, na.rm = TRUE), 2),
      avg_hmov = round(mean(TrajectoryPfxHorz, na.rm = TRUE), 2),
      avg_vmov = round(mean(TrajectoryPfxVert, na.rm = TRUE), 2),
      avg_vx0 = round(mean(TrajectoryV0, na.rm = TRUE), 2),
      avg_vy0 = round(mean(TrajectoryV1, na.rm = TRUE), 2),
      avg_vz0 = round(mean(TrajectoryV2, na.rm = TRUE), 2),
      avg_ax = round(mean(TrajectoryA0, na.rm = TRUE), 2),
      avg_ay = round(mean(TrajectoryA1, na.rm = TRUE), 2),
      avg_az = round(mean(TrajectoryA2, na.rm = TRUE), 2),
      avg_spin = mean(ReleaseSpinRate, na.rm = TRUE),
      .groups='drop'
    )
  
  return(pitch_general_characteristics_df)
}

summarise_pitch_level_data <- function(pitch_level_data, group_by_var = "PitcherId") {
  # set event lists for count flags
  swing_events <- c('field_out', 'single', 'foul', 'foul_bunt', 'foul_tip',
                    'home_run', 'field_error', 'sac_bunt', 'swinging_strike',
                    'double', 'grounded_into_double_play', 'force_out',
                    'triple', 'swinging_strike_blocked')
  
  contact_events <- c('field_out', 'single', 'foul', 'foul_bunt', 'foul_tip',
                      'home_run', 'field_error', 'sac_bunt', 'double',
                      'grounded_into_double_play', 'force_out', 'triple')
  
  strike_events <- c('called_strike', 'swinging_strike', 'foul', 'foul_tip', 'swinging_strike_blocked',
                     'foul_bunt', 'strikeout')
  
  at_bat_events <- c("strikeout", "walk", "single", "double", "triple", "home_run", 
                     "field_out", "force_out", "field_error", "grounded_into_double_play", "sac_bunt")
  
  
  swinging_strike_events <- c('swinging_strike', 'swinging_strike_blocked')
  
  hit_events <- c('single', 'double', 'triple', 'home_run')
  
  out_events <- c('force_out', 'field_out', 'sac_bunt', 'strikeout')
  
  # create count flags
  processed_pitch_level_data <- pitch_level_data %>%
    mutate(
      is_in_zone = if_else(
        (TrajectoryLocationZ <= StrikeZoneTop &
           TrajectoryLocationZ >= StrikeZoneBottom &
           TrajectoryLocationX >= -0.833 &
           TrajectoryLocationX <= 0.833) | PitchCall == 'strikeout', TRUE, FALSE),
      is_first_pitch = if_else(PitchNumber == 1, TRUE, FALSE),
      is_swing = if_else(PitchCall %in% swing_events, TRUE, FALSE),
      is_contact = if_else(PitchCall %in% contact_events, TRUE, FALSE),
      is_swinging_strike = if_else(PitchCall %in% swinging_strike_events, TRUE, FALSE),
      is_called_strike = if_else(PitchCall == 'called_strike', TRUE, FALSE),
      is_strikeout = if_else(PitchCall == 'strikeout', TRUE, FALSE),
      is_walk = if_else(PitchCall == 'walk', TRUE, FALSE),
      is_hit = if_else(PitchCall %in% hit_events, TRUE, FALSE),
      is_out = if_else(PitchCall %in% out_events, TRUE, FALSE),
      is_double_out = if_else(PitchCall == 'grounded_into_double_play', TRUE, FALSE),
      count_leverage = case_when(
        Balls == Strikes ~ "Even",
        Balls > Strikes ~ "Behind",
        Balls < Strikes ~ "Ahead",
        TRUE ~ NA_character_)
      )
  
  # summarize by pitcher or batter
  
  summarized_player_level_data <- processed_pitch_level_data %>%
    group_by(.data[[group_by_var]]) %>%
    summarise(
      pitches              = n(),
      zone_perc            = round(mean(is_in_zone, na.rm = TRUE) * 100, 2),
      outside_zone_perc    = round(mean(!is_in_zone, na.rm = TRUE) * 100, 2),
      swing_perc           = round(mean(is_swing, na.rm = TRUE) * 100, 2),
      take_perc            = round(mean(!is_swing, na.rm = TRUE) * 100, 2),
      zone_swing_perc      = round((sum(is_in_zone & is_swing, na.rm = TRUE) / sum(is_in_zone, na.rm = TRUE)) * 100, 2),
      outside_swing_perc   = round((sum(!is_in_zone & is_swing, na.rm = TRUE) / sum(!is_in_zone, na.rm = TRUE)) * 100, 2),
      
      contact_perc         = round(sum(is_swing & is_contact, na.rm = TRUE) / sum(is_swing, na.rm = TRUE) * 100, 2),
      zone_contact_perc    = round(sum(is_in_zone & is_swing & is_contact, na.rm = TRUE) / sum(is_in_zone & is_swing, na.rm = TRUE) * 100, 2),
      outside_contact_perc = round(sum(!is_in_zone & is_swing & is_contact, na.rm = TRUE) / sum(!is_in_zone & is_swing, na.rm = TRUE) * 100, 2),
      
      cstr_perc            = round(sum(is_called_strike, na.rm = TRUE) / pitches * 100, 2),
      swstr_perc           = round(sum(is_swinging_strike, na.rm = TRUE) / pitches * 100, 2),
      cstr_plus_swstr_perc = round((sum(is_called_strike, na.rm = TRUE) + sum(is_swinging_strike, na.rm = TRUE)) / pitches * 100, 2),
      whiff_perc           = round((sum(is_swinging_strike, na.rm = TRUE) / sum(is_swing, na.rm = TRUE) * 100), 2),
      
      f_strike_perc        = round(sum(is_first_pitch & (is_called_strike | PitchCall %in% swing_events), na.rm = TRUE) / sum(is_first_pitch, na.rm = TRUE) * 100, 2),
      
      batters_faced        = sum(PitchCall %in% at_bat_events, na.rm = TRUE),
      
      outs                 = sum(is_out, na.rm = TRUE) + (sum(is_double_out, na.rm = TRUE) * 2),
      
      singles        = sum(PitchCall == 'single', na.rm = TRUE),
      doubles        = sum(PitchCall == 'double', na.rm = TRUE),
      triples        = sum(PitchCall == 'triple', na.rm = TRUE),
      home_runs        = sum(PitchCall == 'home_run', na.rm = TRUE),
      
      total_hits           = sum(is_hit, na.rm = TRUE),
      
      ip            = round(outs / 3, 3),
      
      k_perc               = round(sum(is_strikeout, na.rm = TRUE) / batters_faced * 100, 2),
      bb_perc              = round(sum(is_walk, na.rm = TRUE) / batters_faced * 100, 2),
    
      ahead_perc = round(
        sum(count_leverage == "Ahead", na.rm = TRUE) / n() * 100, 2
      ),
      
      behind_perc = round(
        sum(count_leverage == "Behind", na.rm = TRUE) / n() * 100, 2
      ),
      
      even_perc = round(
        sum(count_leverage == "Even", na.rm = TRUE) / n() * 100, 2
      ),
      
      avg_pitches_per_ab = round((pitches / batters_faced), 0),
      .groups = 'drop'
    )
  
  
  # summarize pitcher or batter per pitch type 
  
  summarized_player_pitchtype_level_data <- processed_pitch_level_data %>%
    group_by(.data[[group_by_var]], PitchType) %>%
    summarise(
      pitches              = n(),
      zone_perc            = round(mean(is_in_zone, na.rm = TRUE) * 100, 2),
      outside_zone_perc    = round(mean(!is_in_zone, na.rm = TRUE) * 100, 2),
      swing_perc           = round(mean(is_swing, na.rm = TRUE) * 100, 2),
      zone_swing_perc      = round((sum(is_in_zone & is_swing, na.rm = TRUE) / sum(is_in_zone, na.rm = TRUE)) * 100, 2),
      outside_swing_perc   = round((sum(!is_in_zone & is_swing, na.rm = TRUE) / sum(!is_in_zone, na.rm = TRUE)) * 100, 2),
      
      contact_perc         = round(sum(is_swing & is_contact, na.rm = TRUE) / sum(is_swing, na.rm = TRUE) * 100, 2),
      zone_contact_perc    = round(sum(is_in_zone & is_swing & is_contact, na.rm = TRUE) / sum(is_in_zone & is_swing, na.rm = TRUE) * 100, 2),
      outside_contact_perc = round(sum(!is_in_zone & is_swing & is_contact, na.rm = TRUE) / sum(!is_in_zone & is_swing, na.rm = TRUE) * 100, 2),
      
      cstr_perc            = round(sum(is_called_strike, na.rm = TRUE) / pitches * 100, 2),
      swstr_perc           = round(sum(is_swinging_strike, na.rm = TRUE) / pitches * 100, 2),
      cstr_plus_swstr_perc = round((sum(is_called_strike, na.rm = TRUE) + sum(is_swinging_strike, na.rm = TRUE)) / pitches * 100, 2),
      
      f_strike_perc        = round(sum(is_first_pitch & (is_called_strike | PitchCall %in% swing_events), na.rm = TRUE) / sum(is_first_pitch, na.rm = TRUE) * 100, 2),
      
      good_take_perc       = round(sum(!is_in_zone & !is_swing, na.rm = TRUE) / sum(!is_in_zone, na.rm = TRUE) * 100, 2),
      
      avg_release_speed = mean(ReleaseSpeed, na.rm = TRUE),
      avg_zone_speed = mean(TrajectoryZoneSpeed, na.rm = TRUE),
      avg_zone_time = mean(TrajectoryZoneTime, na.rm = TRUE),
      avg_location_x = mean(TrajectoryLocationX, na.rm = TRUE),
      avg_location_y = mean(TrajectoryLocationY, na.rm = TRUE),
      avg_location_z = mean(TrajectoryLocationZ, na.rm = TRUE),
      avg_spin_rate = mean(ReleaseSpinRate, na.rm = TRUE),
      avg_release_x = mean(ReleasePositionX, na.rm = TRUE),
      avg_release_y = mean(ReleasePositionY, na.rm = TRUE),
      avg_release_z = mean(ReleasePositionZ, na.rm = TRUE),
      
      avg_vertical_break = mean(TrajectoryVerticalBreakInduced, na.rm = TRUE),
      avg_horizontal_break = mean(TrajectoryPfxHorz, na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%
    group_by(.data[[group_by_var]]) %>%
    mutate(pitch_usage = round(pitches / sum(pitches) * 100, 2)) %>%
    ungroup()
  
  if (group_by_var == 'BatterId') {
    
    # For split metrics, rename 'pitch_usage' to 'pitch_type_seen_perc' 
    # and rename 'pitches' to 'pitches_seen' to follow batter logic
    summarized_player_pitchtype_level_data <- summarized_player_pitchtype_level_data %>%
      rename(pitches_seen = pitches, pitch_type_seen_perc = pitch_usage)
    
    
    summarized_player_level_data <- summarized_player_level_data %>%
      rename(pitches_seen = pitches, at_bats = batters_faced) %>%
      select(-ip)
    
  }
  
  return(list(
    Player_Performance = summarized_player_level_data,
    Player_Pitch_Performance = summarized_player_pitchtype_level_data
  ))
  
}
