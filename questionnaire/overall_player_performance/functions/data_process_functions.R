
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
  
  ball_events <- c('ball', 'walk', 'wild_pitch', 'blocked_ball')
  
  at_bat_events <- c("strikeout", "walk", "single", "double", "triple", "home_run", 
                     "field_out", "force_out", "field_error", "grounded_into_double_play", "sac_bunt")
  
  whiff_events <- c('swinging_strike', 'swinging_strike_blocked')
  
  swinging_strike_events <- c('swinging_strike', 'foul', 'foul_tip', 'foul_bunt', 'swinging_strike_blocked')
  
  hit_events <- c('single', 'double', 'triple', 'home_run')
  
  in_play_events <- c('single', 'sac_bunt', 'dobule', 'grounded_into_double_play', 'field_out',
                      'home_run', 'field_error', 'force_out', 'triple')
  
  out_events <- c('force_out', 'field_out', 'sac_bunt', 'strikeout')
  
  stolen_base_events <- c('stolen_base_3b', 'stolen_base_2b')
  # create count flags
  processed_pitch_level_data <- pitch_level_data %>%
    mutate(
      is_in_zone = if_else(
        (TrajectoryLocationZ <= StrikeZoneTop &
           TrajectoryLocationZ >= StrikeZoneBottom &
           TrajectoryLocationX >= -0.833 &
           TrajectoryLocationX <= 0.833) | PitchCall == 'strikeout', TRUE, FALSE),
      is_strike = if_else(PitchCall %in% strike_events, TRUE, FALSE),
      is_ball = if_else(PitchCall %in% ball_events, TRUE, FALSE),
      is_first_pitch = if_else(PitchNumber == 1, TRUE, FALSE),
      is_swing = if_else(PitchCall %in% swing_events, TRUE, FALSE),
      is_contact = if_else(PitchCall %in% contact_events, TRUE, FALSE),
      is_swinging_strike = if_else(PitchCall %in% swinging_strike_events, TRUE, FALSE),
      is_whiff = if_else(PitchCall %in% whiff_events, TRUE, FALSE),
      is_called_strike = if_else(PitchCall == 'called_strike', TRUE, FALSE),
      is_strikeout = if_else(PitchCall == 'strikeout', TRUE, FALSE),
      is_walk = if_else(PitchCall == 'walk', TRUE, FALSE),
      is_hit = if_else(PitchCall %in% hit_events, TRUE, FALSE),
      is_in_play = if_else(PitchCall %in% in_play_events, TRUE, FALSE),
      is_out = if_else(PitchCall %in% out_events, TRUE, FALSE),
      is_wild_pitch = if_else(PitchCall == 'wild_pitch', TRUE, FALSE),
      is_double_out = if_else(PitchCall == 'grounded_into_double_play', TRUE, FALSE),
      is_error = if_else(PitchCall == 'field_error', TRUE, FALSE),
      is_stolen_base = if_else(PitchCall %in% stolen_base_events, TRUE, FALSE),
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
      strikes              = sum(is_strike, na.rm = TRUE) + sum(is_in_play, na.rm = TRUE),
      balls                = sum(is_ball, na.rm = TRUE),
      strike_perc          = round(strikes / pitches * 100, 2),
      ball_perc            = round(balls / pitches * 100, 2),
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
      whiff_perc           = round((sum(is_whiff, na.rm = TRUE) / sum(is_swing, na.rm = TRUE) * 100), 2),
      
      f_strike_perc        = round(sum(is_first_pitch & (is_called_strike | PitchCall %in% swing_events), na.rm = TRUE) / sum(is_first_pitch, na.rm = TRUE) * 100, 2),
      wild_pitch           = sum(is_wild_pitch, na.rm = TRUE),
      good_take_perc       = round(sum(!is_in_zone & !is_swing, na.rm = TRUE) / sum(!is_in_zone, na.rm = TRUE) * 100, 2),
      
      batters_faced        = sum(PitchCall %in% at_bat_events, na.rm = TRUE),
      
      outs                 = sum(is_out, na.rm = TRUE) + (sum(is_double_out, na.rm = TRUE) * 2),
      errors               = sum(is_error, na.rm = TRUE),
      walks                = sum(is_walk, na.rm = TRUE),
      singles        = sum(PitchCall == 'single', na.rm = TRUE),
      doubles        = sum(PitchCall == 'double', na.rm = TRUE),
      triples        = sum(PitchCall == 'triple', na.rm = TRUE),
      home_runs      = sum(PitchCall == 'home_run', na.rm = TRUE),
      stolen_bases   = sum(is_stolen_base, na.rm = TRUE),
      
      total_hits           = sum(is_hit, na.rm = TRUE),
      
      ip            = round(outs / 3, 3),
      strike_outs   = sum(is_strikeout, na.rm = TRUE),
      k_perc               = round(strike_outs / batters_faced * 100, 2),
      bb_perc              = round(sum(walks) / batters_faced * 100, 2),
      
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
      strikes              = sum(is_strike, na.rm = TRUE) + sum(is_in_play, na.rm = TRUE),
      balls                = sum(is_ball, na.rm = TRUE),
      strike_perc          = round(strikes / pitches * 100, 2),
      ball_perc            = round(balls / pitches * 100, 2),
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
      whiff_perc           = round((sum(is_whiff, na.rm = TRUE) / sum(is_swing, na.rm = TRUE) * 100), 2),
      
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
    
    batter_id_hand_df <- pitch_level_data %>%
      distinct(BatterId, BatterSide)
    
    summarized_player_level_data <- summarized_player_level_data %>%
      rename(pitches_seen = pitches, at_bats = batters_faced) %>%
      mutate(batter_ahead_perc = behind_perc,
             batter_even_perc = even_perc,
             batter_behind_perc = ahead_perc) %>%
      select(-ip) %>%
      left_join(batter_id_hand_df, by=('BatterId'))
    
    summarized_player_pitchtype_level_data <- summarized_player_pitchtype_level_data %>%
      rename(pitches_seen = pitches, pitch_type_seen_perc = pitch_usage) %>%
      left_join(batter_id_hand_df, by=('BatterId'))
    
    
  }
  
  if (group_by_var == 'PitcherId') {
    
    pitcher_id_hand_df <- pitch_level_data %>%
      distinct(PitcherId, PitcherHand)
    
    summarized_player_level_data <- summarized_player_level_data %>%
      left_join(pitcher_id_hand_df, by=('PitcherId'))
    
    summarized_player_pitchtype_level_data <- summarized_player_pitchtype_level_data %>%
      left_join(pitcher_id_hand_df, by=('PitcherId'))
  
  }
  
  return(list(
    Player_Performance = summarized_player_level_data,
    Player_Pitch_Performance = summarized_player_pitchtype_level_data
  ))
  
}

create_pitcher_summary_table <- function(pitcher_df) {
  pitcher_table_df <- pitcher_df %>%
    select(
      PitcherId,
      PitcherHand,
      pitches,
      batters_faced,
      total_hits,
      outs,
      ip,
      k_perc,
      bb_perc
    ) %>%
    rename(
      Pitcher_ID = PitcherId,
      Pitcher_Hand = PitcherHand,
      Total_Pitches = pitches,
      Batters_Faced = batters_faced,
      Total_Hits = total_hits,
      Outs = outs,
      IP = ip,
      `K%` = k_perc,
      `BB%` = bb_perc
    )
  
  return(pitcher_table_df)
}


create_pitcher_pitchtype_summary_table <- function(pitcher_pitch_df) {
  
  pitcher_pitch_table_df <- pitcher_pitch_df %>%
    select(
      PitcherId,
      PitcherHand,
      PitchType,
      pitches,
      pitch_usage,
      avg_release_speed,
      avg_spin_rate,
      avg_vertical_break,
      avg_horizontal_break
    ) %>%
    rename(
      Pitcher_ID = PitcherId,
      Pitcher_Hand = PitcherHand,
      Pitch_Type = PitchType,
      Total_Pitches = pitches,
      Pitch_Usage = pitch_usage,
      Avg_Release_Speed = avg_release_speed,
      Avg_Spin_Rate = avg_spin_rate,
      Avg_Vert_Break = avg_vertical_break,
      Avg_Horiz_Break = avg_horizontal_break
    )
  
  return(pitcher_pitch_table_df)
  
}


create_batter_summary_table <- function(batter_df) {
  batter_table_df <- batter_df %>%
    select(
      BatterId,
      BatterSide,
      pitches_seen,
      at_bats,
      total_hits,
      batter_ahead_perc,
      batter_behind_perc,,
      k_perc,
      bb_perc
    ) %>%
    rename(
      Batter_ID = BatterId,
      Batter_Hand = BatterSide,
      Total_Pitches = pitches_seen,
      At_Bats = at_bats,
      Total_Hits = total_hits,
      `Batter_Ahead%` = batter_ahead_perc,
      `Batter_Behind%` = batter_behind_perc,
      `K%` = k_perc,
      `BB%` = bb_perc
    )
  
  return(batter_table_df)
}

create_summary_pdf <- function(table_df, table_title = 'Summary Table') {
  custom_theme <- ttheme_default(
    core = list(
      bg_params = list(fill = c("white", "#F2F2F2")), 
      fg_params = list(fontsize = 10)
    ),
    colhead = list(
      bg_params = list(fill = "#1E4A75"), 
      fg_params = list(col = "white", fontface = "bold", fontsize = 10) 
    )
  )
  
  
  title_grob <- textGrob(
    label = paste0(table_title), 
    gp = gpar(fontsize = 20, fontface = "bold", col = "black")
  )
  
  
  table_grob <- tableGrob(table_df, rows = NULL, theme = custom_theme)
  
  
  pdf(paste0(table_title, ".pdf"), width = 11, height = 8.5)
  
  
  grid.arrange(
    title_grob, 
    table_grob, 
    ncol = 1, 
    heights = c(1, 7) 
  )
  
  dev.off() 
}
