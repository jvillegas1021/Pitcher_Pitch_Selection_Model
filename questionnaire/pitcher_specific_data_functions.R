

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