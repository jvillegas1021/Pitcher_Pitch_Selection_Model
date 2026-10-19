library(tidyverse)


clean_pitch_level_data <- function(pitch_level_data) {
  
  # order data by GamePk, AtBatNumber, PitchNumber
  ordered_pitch_data <- pitch_level_data %>%
    arrange(GamePk, AtBatNumber, PitchNumber)
  
  # 0,1,2,3,4,5,6,7,8,9,10 - error with inning = 0?
  which(ordered_pitch_data$Inning == 0)
  which(colnames(ordered_pitch_data) == 'Inning')
  
  # PitchId 502, (row 6, column 9) has incorrect Inning, fix, set to 1
  ordered_pitch_data[6, 9] <- 1
  
  # for pitch analysis, remove non pitches, 'pickoffs', ect
  cleaned_pitch_data <- ordered_pitch_data %>%
    drop_na(PitchType) %>%
    filter(PitchType != 'NULL')
  
  return (cleaned_pitch_data)
}

summarise_pitch_level_data <- function(pitch_level_data) {
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
      is_walk = if_else(PitchCall == 'walk', TRUE, FALSE)
    )
  
  # summarize by pitcher
  
  summarized_pitcher_level_data <- processed_pitch_level_data %>%
    group_by(PitcherId) %>%
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
      
      k_perc               = round(sum(is_strikeout, na.rm = TRUE) / batters_faced * 100, 2),
      bb_perc              = round(sum(is_walk, na.rm = TRUE) / batters_faced * 100, 2),
      
      
      
      .groups = 'drop'
    )
  
  
  # summarize pitcher per pitch type 
  
  summarized_pitcher_pitchtype_level_data <- processed_pitch_level_data %>%
    group_by(PitcherId, PitchType) %>%
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
    )
  
  return(list(
    Pitcher_Performance = summarized_pitcher_level_data,
    Pitcher_Pitch_Performance = summarized_pitcher_pitchtype_level_data
  ))
  
}


