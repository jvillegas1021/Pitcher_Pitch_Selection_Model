filter_to_current_pitcher_cleaned <- function(pitcher_id, statcast_df) {
    pitcher_df <- statcast_df %>%
    filter(pitcher == pitcher_id) %>%
    drop_na(pitch_type)

    return(pitcher_df)
    }

create_pitcher_pitch_usage_profile <- function(pitcher_df) {
    pitch_usage_df <- pitcher_df %>%
    group_by(pitch_type,
             pitch_name) %>%
    summarise(total_count = n(),
              .groups='drop'
              ) %>%
    mutate(pitch_usage = total_count / sum(total_count)) %>%
    filter(pitch_usage >= 0.1) %>%
    select(pitch_type,
           pitch_name,
           pitch_usage
           )

    filtered_statcast_df <- pitcher_df %>%
    filter(pitch_type %in% pitch_usage_df$pitch_type)
    
    return(list(pitch_usage_df, filtered_statcast_df))
    }

create_pitcher_pitch_characteristics_profile <- function(pitcher_df) {
    pitch_characteristics_df <- pitcher_df %>%
    group_by(pitch_type, pitch_name) %>%
    summarise(
      avg_velo = round(mean(release_speed, na.rm = TRUE), 2),
      avg_rel_x = round(mean(release_pos_x, na.rm = TRUE), 2),
      avg_rel_y = round(mean(release_pos_y, na.rm = TRUE), 2),
      avg_rel_z = round(mean(release_pos_z, na.rm = TRUE), 2),
      avg_hmov = round(mean(pfx_x, na.rm = TRUE), 2),
      avg_vmov = round(mean(pfx_z, na.rm = TRUE), 2),
      avg_vx0 = round(mean(vx0, na.rm = TRUE), 2),
      avg_vy0 = round(mean(vy0, na.rm = TRUE), 2),
      avg_vz0 = round(mean(vz0, na.rm = TRUE), 2),
      avg_ax = round(mean(ax, na.rm = TRUE), 2),
      avg_ay = round(mean(ay, na.rm = TRUE), 2),
      avg_az = round(mean(az, na.rm = TRUE), 2),
      avg_spin = mean(release_spin_rate, na.rm = TRUE),
        .groups='drop'
    )

    return(pitch_characteristics_df)
}

create_pitcher_pitch_performance_profile <- function(pitcher_df) {
    
    strike_zone = seq(1,9)

    swinging_strike_event_list = c('swinging_strike', 'swinging_strike_blocked')
    
    contact_event_list = c('foul', 'foul_tip', 'hit_into_play', 'foul_pitchout')
    
    strike_event_list = c(
        'foul', 'foul_tip', 'hit_into_play', 'foul_pitchout',
        'swinging_strike', 'swinging_strike_blocked', 'called_strike'
    )
    
    strike_out_event_list = c('strikeout', 'strikeout_double_play')
    
    ball_event_list = c('blocked_ball', 'ball','hit_by_pitch')
    
    swing_event_list = c(contact_event_list , swinging_strike_event_list)
    
    
    out_event_list = c(
        'grounded_into_double_play',
        'field_out',
        'force_out',
        'fielders_choice_out',
        'double_play',
        'triple_play',
        'sac_fly',
        'sac_fly_double_play'
    )
    
    hit_event_list = c(
        'single',
        'double',
        'triple',
        'home_run'
    )

    total_first_pitches <- sum(pitcher_df$pitch_number ==1)
    total_two_strike_pitches <- 
    
    pitcher_performance_df <- pitcher_df %>%
    group_by(pitch_type, pitch_name) %>%
    summarise(total_thrown = n(),
              pitches_in_zone = sum(zone %in% strike_zone),
              pitches_out_zone = sum(!zone %in% strike_zone),
              swings = sum(description %in% swing_event_list),
              swings_in_zone = sum(description %in% swing_event_list & zone %in% strike_zone),
              swings_out_zone = sum(description %in% swing_event_list & !zone %in% strike_zone),
              contacted_balls = sum(description %in% contact_event_list),
              contacted_balls_in_zone = sum(description %in% contact_event_list & zone %in% strike_zone),
              contacted_balls_out_zone = sum(description %in% contact_event_list & !zone %in% strike_zone),
              whiffs = sum(description %in% swinging_strike_event_list),
              whiffs_in_zone = sum(description %in% swinging_strike_event_list & zone %in% strike_zone),
              whiffs_out_zone = sum(description %in% swinging_strike_event_list & !zone %in% strike_zone),
              first_pitches = sum(pitch_number == 1),
              first_pitch_strikes = sum(pitch_number == 1 & description %in% strike_event_list),
              strikes = sum(description %in% strike_event_list),
              balls = sum(description %in% ball_event_list),
              strike_outs = sum(events %in% strike_out_event_list),
              hits = sum(events %in% hit_event_list),
              outs = sum(events %in% out_event_list),
              home_runs = sum(events == 'home_run', na.rm=TRUE),
              ground_balls = sum(bb_type == 'ground_ball', na.rm=TRUE),
              fly_balls = sum(bb_type == 'fly_ball', na.rm=TRUE),
              line_drives = sum(bb_type == 'line_drive', na.rm=TRUE),
              popups = sum(bb_type == 'popup', na.rm=TRUE),
              batted_balls = sum(!is.na(bb_type)),
              hard_hit_balls = sum(launch_speed >= 95, na.rm=TRUE),
              .groups='drop'
              ) %>%
    mutate(in_zone_perc = round(pitches_in_zone / total_thrown * 100, 2),
           out_zone_perc = round(pitches_out_zone / total_thrown * 100, 2),
           swing_perc = round(swings / total_thrown * 100, 2),
           swing_in_zone_perc = round(swings_in_zone / pitches_in_zone * 100, 2),
           swing_out_zone_perc = round(swings_out_zone / pitches_out_zone * 100, 2),
           contact_perc = round(contacted_balls / total_thrown * 100, 2),
           contact_in_zone_perc = round(contacted_balls_in_zone / pitches_in_zone * 100, 2),
           contact_out_zone_perc = round(contacted_balls_out_zone / pitches_out_zone * 100, 2),
           whiff_perc = round(whiffs / total_thrown * 100, 2),
           whiff_in_zone_perc = round(whiffs_in_zone / pitches_in_zone * 100, 2),
           whiff_out_zone_perc = round(whiffs_out_zone / pitches_out_zone * 100, 2),
           first_pitch_perc = round(first_pitches / total_first_pitches * 100, 2),
           first_pitch_strike_perc = round(first_pitch_strikes / first_pitches * 100, 2),
           strike_perc = round(strikes / total_thrown * 100, 2),
           ball_perc = round(balls / total_thrown * 100, 2),
           strike_out_perc = round(strike_outs / total_thrown * 100, 2),
           hit_perc = round(hits / batted_balls * 100, 2),
           out_perc = round(outs / batted_balls * 100, 2),
           home_run_perc = round(home_runs / contacted_balls * 100, 2),
           ground_ball_perc = round(ground_balls / batted_balls * 100, 2),
           fly_ball_perc = round(fly_balls / batted_balls * 100, 2),
           line_drive_perc = round(line_drives / batted_balls * 100, 2),
           popup_perc = round(popups / batted_balls * 100, 2),
           batted_ball_perc = round(batted_balls / total_thrown * 100, 2),
           hard_hit_perc = round(hard_hit_balls / batted_balls * 100, 2)
          ) %>%
    select(pitch_type,
           pitch_name,
           total_thrown,
           in_zone_perc,
           out_zone_perc,
           swing_perc,
           swing_in_zone_perc,
           swing_out_zone_perc,
           contact_perc,
           contact_in_zone_perc,
           contact_out_zone_perc,
           whiff_perc,
           whiff_in_zone_perc,
           whiff_out_zone_perc,
           first_pitch_perc,
           first_pitch_strike_perc,
           strike_perc,
           ball_perc,
           strike_out_perc,
           hit_perc,
           out_perc,
           batted_ball_perc,
           hard_hit_perc,
           home_run_perc,
           ground_ball_perc,
           fly_ball_perc,
           line_drive_perc,
           popup_perc
            )
    
    pitcher_performance_df

    return(pitcher_performance_df)
    }



create_pitcher_pitch_zone_profile <- function(pitcher_df) {
    return
    }

create_pitcher_pitch_plots <- function(pitcher_df) {
    ### WHERE PITCHES THROWN

    pitch_general_location <- ggplot(pitcher_df, aes(plate_x, plate_z)) +
      stat_density_2d_filled(bins = 20, show.legend = FALSE) +
      facet_grid(stand ~ pitch_name) +
      annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.5, ymax=3.5,
               fill=NA, color="white", linewidth=1) +
      coord_fixed() +
      theme_minimal()

    # WHERE PITCHES ARE CONTACTED!

    contact_df <- pitcher_df %>% filter(description == 'hit_into_play')
    
    pitch_contact_location <- ggplot(contact_df, aes(plate_x, plate_z)) +
      stat_density_2d_filled(bins = 20, show.legend = FALSE) +
      facet_grid(stand ~ pitch_name) +
      annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.5, ymax=3.5,
               fill=NA, color="white", linewidth=1) +
      coord_fixed() +
      theme_minimal()

    
    # WHERE PITCHES ARE MISSED THE MOST

    whiff_df <- pitcher_df %>% filter(description %in% c('swinging_strike', 'swinging_strike_blocked'))

    pitch_whiff_location <- ggplot(contact_df, aes(plate_x, plate_z)) +
      stat_density_2d_filled(bins = 20, show.legend = FALSE) +
      facet_grid(stand ~ pitch_name) +
      annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.5, ymax=3.5,
               fill=NA, color="white", linewidth=1) +
      coord_fixed() +
      theme_minimal()

    # WEAK CONTACT, VS HARD CONTACT

    hit_hard_df <- pitcher_pitch_filtered_df %>%
    filter(type == 'X') %>%
    mutate(hard_hit = launch_speed >= 95)
    
    weak_df <- hit_hard_df %>% filter(!hard_hit)
    hard_df <- hit_hard_df %>% filter(hard_hit)
    
    pitch_hard_vs_weak_location <- ggplot() +
      stat_density_2d(
        data = weak_df,
        aes(plate_x, plate_z, color = "weak"),
        bins = 10,
        alpha = 0.8
      ) +
      stat_density_2d(
        data = hard_df,
        aes(plate_x, plate_z, color = "hard"),
        bins = 10,
        alpha = 0.8
      ) +
      scale_color_manual(values = c("weak" = "cyan", "hard" = "red")) +
      facet_grid(stand ~ pitch_name) +
      annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.5, ymax=3.5,
               fill=NA, color="black", linewidth=1, linetype = 'dashed') +
      coord_fixed(xlim = c(-2, 2), ylim = c(0, 4)) +
      theme_minimal()

    return(
        list(
        pitch_location = pitch_general_location,
        contact_location = pitch_contact_location,
        whiff_location = pitch_whiff_location,
        hard_vs_weak_location = pitch_hard_vs_weak_location)
           )
}





       