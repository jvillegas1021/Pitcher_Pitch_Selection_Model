get_pitch_palette <- function(pitch_names) {
  library(RColorBrewer)
  library(scales)

  n <- length(pitch_names)
  base_cols <- brewer.pal(max(3, n), "Set2")[1:n]

  list(
    border = base_cols,
    fill   = alpha(base_cols, 0.35)
  )
}

create_pitcher_pitch_arsenal <- function(pitcher_scouting_report_df) {
    pitch_type_list <- unique(pitcher_scouting_report_df$pitch_type)
    return(pitch_type_list)
    }
 
create_pitcher_pitch_usage_df <- function(pitcher_statcast_df) {
    pitch_general_usage_df <- pitcher_statcast_df %>%
    group_by(pitch_type,
             pitch_name) %>%
    summarise(total_count = n(),
              .groups='drop'
              ) %>%
    mutate(pitch_usage = round(total_count / sum(total_count) * 100, 2)) %>%
    select(pitch_type,
           pitch_name,
           pitch_usage
           )

    pitch_usage_vs_rhb_df <- pitcher_statcast_df %>%
    filter(stand == 'R') %>%
    group_by(pitch_type,
             pitch_name) %>%
    summarise(total_count = n(),
              .groups='drop'
              ) %>%
    mutate(pitch_usage = round(total_count / sum(total_count) * 100, 2)) %>%
    select(pitch_type,
           pitch_name,
           pitch_usage
           )

    pitch_usage_vs_lhb_df <- pitcher_statcast_df %>%
    filter(stand == 'L') %>%
    group_by(pitch_type,
             pitch_name) %>%
    summarise(total_count = n(),
              .groups='drop'
              ) %>%
    mutate(pitch_usage = round(total_count / sum(total_count) * 100, 2)) %>%
    select(pitch_type,
           pitch_name,
           pitch_usage
           )
    
    return(list(pitch_general_usage_df,
               pitch_usage_vs_rhb_df,
               pitch_usage_vs_lhb_df))
    }

create_pitcher_pitch_characteristics_df <- function(pitcher_statcast_df) {
    pitch_general_characteristics_df <- pitcher_statcast_df %>%
    group_by(pitch_type, pitch_name) %>%
    summarise(
        avg_velo = round(mean(release_speed, na.rm = TRUE), 2),
        avg_rel_x = round(mean(release_pos_x, na.rm = TRUE), 2),
        avg_rel_y = round(mean(release_pos_y, na.rm = TRUE), 2),
        avg_rel_z = round(mean(release_pos_z, na.rm = TRUE), 2),
        avg_plate_x = round(mean(plate_x, na.rm = TRUE), 2),
        avg_plate_z = round(mean(plate_z, na.rm = TRUE), 2),
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

    return(pitch_general_characteristics_df)
    }

create_pitcher_pitch_performance_df <- function(pitcher_statcast_df) {

    stance_list = c('R', 'L')
    
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

    ############## statcast, stance #######################
    
    total_first_pitches <- sum(pitcher_statcast_df$pitch_number ==1)
    
    
    pitcher_performance_df <- pitcher_statcast_df %>%
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

create_pitcher_usage_plots <- function(pitcher_pitch_usage_df) {
    
    palette <- get_pitch_palette(pitcher_pitch_usage_df[[1]]$pitch_name)
    
                             
    
    pitcher_general_usage_plot <- ggplot(pitcher_pitch_usage_df[[1]], 
                                         aes(x = "", y = pitch_usage, fill = pitch_name)) +
      geom_col(width = 1, color = "black") +
      scale_fill_manual(values = palette$border) +   # FIXED
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Pitch General Usage", fill = 'Pitch Name') + 
      geom_text(
        aes(label = paste0(pitch_usage, " %")),
        position = position_stack(vjust = 0.5)
      )
    
    pitcher_usage_vs_rhb_plot <- ggplot(pitcher_pitch_usage_df[[2]], 
                                        aes(x = "", y = pitch_usage, fill = pitch_name)) +
      geom_col(width = 1, color = "black") +
      scale_fill_manual(values = palette$border) +   # FIXED
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Pitch Usage vs RHB", fill = 'Pitch Name') + 
      geom_text(
        aes(label = paste0(pitch_usage, " %")),
        position = position_stack(vjust = 0.5)
      )
    
    pitcher_usage_vs_lhb_plot <- ggplot(pitcher_pitch_usage_df[[3]], 
                                        aes(x = "", y = pitch_usage, fill = pitch_name)) +
      geom_col(width = 1, color = "black") +
      scale_fill_manual(values = palette$border) +   # FIXED
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Pitch Usage vs LHB", fill = 'Pitch Name') + 
      geom_text(
        aes(label = paste0(pitch_usage, " %")),
        position = position_stack(vjust = 0.5)
      )
    


    return(list(pitcher_general_usage_plot,
                pitcher_usage_vs_rhb_plot,
                pitcher_usage_vs_lhb_plot))
    }

create_pitcher_pitch_characteristics_plots <- function(pitcher_pitch_characteristics_df) {
    palette <- get_pitch_palette(pitcher_pitch_characteristics_df$pitch_name)

    pitch_velo_spin_plot <- ggplot(pitcher_pitch_characteristics_df,
                                aes(x = avg_velo, y = avg_spin, color = pitch_name)) +
                                scale_color_manual(values = palette$border) +
                                geom_point(size = 10, shape = 21, fill = "white", stroke = 1.2) +
                                geom_text(aes(label = pitch_type),
                                    size = 4,
                                    fontface = "bold",
                                    show.legend = FALSE) +
                                labs(
                                title = "Pitch Velocity and Spin Rate Average",
                                x = "Average Velocity",
                                y = "Average Spin Rate",
                                color = NULL   # ← removes legend title
                                ) +
                                theme_bw(base_size = 14) +
                                theme(
                                plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                                axis.title = element_text(face = "bold"),
                                panel.grid = element_blank(),
                                legend.position = "bottom",
                                legend.title = element_blank(),   # ← also removes legend title
                                panel.border = element_rect(color = "black", fill = NA)
                                )

    pitch_release_to_finish_plot <- ggplot(pitcher_pitch_characteristics_df, aes(color = pitch_name)) +
                                geom_segment(aes(
                                    x = avg_rel_x,
                                    y = avg_rel_z,
                                    xend = avg_plate_x,
                                    yend = avg_plate_z),
                                    linewidth = 1, linetype = 'dashed') +
                                scale_color_manual(values = palette$border) +
                                geom_point(aes(x = avg_rel_x, y = avg_rel_z), size = 4, shape = 21, fill = "white") +
                                geom_point(aes(x = avg_plate_x, y = avg_plate_z), size = 4) +
                                coord_fixed(xlim = c(-3.5, 3.5), ylim = c(-1, 8)) +
                                annotate("rect",
                                       xmin = -0.85, xmax = 0.85,
                                       ymin = 1.5, ymax = 3.5,
                                       fill = NA, color = "black",
                                       linetype = "dashed", linewidth = 1) +
                                labs(
                                  title = 'Pitch Avg Release to Avg Finish (Catcher View)',
                                x = "Horizontal Position (ft)",
                                y = "Vertical Position (ft)",
                                color = 'Pitch Name'
                                ) +
                                theme(
                                  plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
                                  axis.title = element_text(face = "bold"),
                                  panel.grid = element_blank(),
                                  panel.border = element_rect(color = "black", fill = NA))


    return(list(pitch_velo_spin_plot,
                pitch_release_to_finish_plot))

}

pitcher_radar_acceleration_plot <- function(pitcher_pitch_characteristics_df) {
    palette <- get_pitch_palette(pitcher_pitch_characteristics_df$pitch_name)
    
    colors_border <- palette$border
    colors_fill   <- palette$fill

  # Build the data
  pitcher_acceleration_radar_df <- pitcher_pitch_characteristics_df %>%
    select(pitch_type,
           pitch_name,
           avg_ax,
           avg_ay,
           avg_az) %>%
    mutate(avg_ax = abs(avg_ax),
           avg_ay = abs(avg_ay),
           avg_az = abs(avg_az))

  # Numeric-only radar base
  radar_base <- pitcher_acceleration_radar_df %>%
    select(avg_ax, avg_ay, avg_az) %>%
    rename(
      Horizontal= avg_ax,
      Forward  = avg_ay,
      Vertical  = avg_az
    ) %>%
    as.data.frame()

  # Required rows
  max_row <- apply(radar_base, 2, max)
  min_row <- apply(radar_base, 2, min)

  # Final radar matrix
  radar_ready <- rbind(max_row, min_row, radar_base)
  rownames(radar_ready) <- c("MAX", "MIN", pitcher_acceleration_radar_df$pitch_type)


  # Wrap the plot in a function so it behaves like a ggplot object
  radar_plot <- function() {
    par(mar = c(2, 6, 4, 6))  # prevent label cutoff

    radarchart(
      radar_ready,
      axistype = 1,
      pcol = colors_border,
      pfcol = colors_fill,
      plwd = 3,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(min(min_row), max(max_row), length.out = 5),
      cglwd = 0.8,
      vlcex = .7
    )
    title("Pitch Acceleration Profile")
    legend(
      "topright",
      legend = pitcher_acceleration_radar_df$pitch_name,
      col = colors_border,
      lwd = 2,
      bty = "n"
    )
  }

  return(radar_plot)
}


    
create_pitcher_pitch_visual_plots <- function(pitcher_statcast_df) {
    ### WHERE PITCHES THROWN
    pitcher_statcast_df <- pitcher_statcast_df %>%
    mutate(batter_hand = if_else(stand == 'R', 'RHB', 'LHB'))
    
    pitch_general_location <- ggplot(pitcher_statcast_df, aes(plate_x, plate_z)) +
        stat_density_2d_filled(bins = 20, show.legend = FALSE) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        labs(title='Pitch Locations (Catcher View)',
            x = "Horizontal Plate Position (ft)",
            y = "Vertical Plate Position (ft)") +
        facet_grid(batter_hand ~ pitch_name) +
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.5, ymax=3.5,
               fill=NA, color="white", linewidth=1) +
        coord_fixed(xlim = c(-2, 2), ylim = c(0, 5)) +
        theme_bw() +
        theme(panel.grid = element_blank())

    # WHERE PITCHES ARE CONTACTED!

    hard_hit_df <- pitcher_statcast_df %>% filter(launch_speed >= 95)
    
    pitch_hard_hit_location <- ggplot(hard_hit_df, aes(plate_x, plate_z)) +
        stat_density_2d_filled(bins = 20, show.legend = FALSE)  +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        labs(title='Hard Hit Locations (Catcher View)',
             x = "Horizontal Plate Position (ft)",
             y = "Vertical Plate Position (ft)") +
        facet_grid(batter_hand ~ pitch_name) +
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.5, ymax=3.5,
               fill=NA, color="white", linewidth=1) +
        coord_fixed() +
        theme_bw() +
        theme(panel.grid = element_blank())

    
    # WHERE PITCHES ARE MISSED THE MOST

    whiff_df <- pitcher_statcast_df %>% filter(description %in% c('swinging_strike', 'swinging_strike_blocked'))

    pitch_whiff_location <- ggplot(whiff_df, aes(plate_x, plate_z)) +
        stat_density_2d_filled(bins = 20, show.legend = FALSE) +
        scale_x_continuous(breaks = NULL) +
        scale_y_continuous(breaks = NULL) +
        labs(title='Whiff Locations (Catcher View)',
             x = "Horizontal Plate Position (ft)",
             y = "Vertical Plate Position (ft)") +
        facet_grid(batter_hand ~ pitch_name) +
        annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.5, ymax=3.5,
               fill=NA, color="white", linewidth=1) +
        coord_fixed() +
        theme_bw() +
        theme(panel.grid = element_blank())


    return(
        list(
        pitch_location = pitch_general_location,
        pitch_hard_location = pitch_hard_hit_location,
        whiff_location = pitch_whiff_location))
}

create_pitch_tendency_plots <- function(pitcher_scouting_report_df) {
        ########## PITCH COUNT HEAT MAP ####################
    heatmap_df <- pitcher_scouting_report_df %>%
    mutate(count = paste0(balls, "-", strikes),
          batter_hand = if_else(stance == 'R', 'RHB', 'LHB')) %>%
    group_by(batter_hand, count, pitch_name) %>%
    summarise(prob = mean(probability), .groups = "drop") %>%
    mutate(count = factor(count, levels = c("0-0","1-0","2-0","3-0",
                                            "0-1","1-1","2-1","3-1",
                                            "0-2","1-2","2-2","3-2")))

    pitch_count_heatmap_plot <- ggplot(heatmap_df, aes(x = pitch_name, y = count, fill = prob)) +
        geom_tile(color = "white") +
        scale_fill_viridis_c(option = "H") +
        facet_wrap(~ batter_hand) +
        labs(
            title = "Pitch Tendencies by Count",
            x = "Pitch Type",
            y = "Count",
            fill = "Probability (%)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))




    #################### PITCH TIME THRU ORDER #######################
    heatmap_tto <- pitcher_scouting_report_df %>%
    mutate(count = paste0(balls, "-", strikes)) %>%
    group_by(tto, count, pitch_name) %>%
    summarise(prob = mean(probability), .groups = "drop") %>%
    mutate(
        count = factor(count, levels = c(
            "0-0","1-0","2-0","3-0",
            "0-1","1-1","2-1","3-1",
            "0-2","1-2","2-2","3-2"
        ))
    )

    pitch_tto_heatmap_plot <- ggplot(heatmap_tto, aes(x = pitch_name, y = count, fill = prob)) +
        geom_tile(color = "white") +
        scale_fill_viridis_c(option = "C") +
        facet_wrap(~ tto) +
        labs(
            title = "Pitch Tendencies by Count and Times Through Order",
            x = "Pitch Type",
            y = "Count",
            fill = "Probability (%)"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))


    ##################### PITCH COUNT GRID ########################
    count_df <- pitcher_scouting_report_df %>%
    mutate(count = paste0(balls, "-", strikes),
          batter_hand = if_else(stance == 'R', 'RHB', 'LHB')) %>%
    group_by(batter_hand, count) %>%
    slice_max(probability, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
        balls = as.integer(substr(count, 1, 1)),
        strikes = as.integer(substr(count, 3, 3)),
        count = factor(count, levels = c(
            "0-0","1-0","2-0","3-0",
            "0-1","1-1","2-1","3-1",
            "0-2","1-2","2-2","3-2"
        ))
    )
    
    pitch_count_grid <- ggplot(count_df, aes(x = strikes, y = balls, fill = pitch_type)) +
        geom_tile(color = "white", linewidth = 0.7) +
        geom_text(aes(label = pitch_type), color = "black", fontface = "bold") +
        scale_y_reverse(breaks = 0:3) +
        scale_x_continuous(breaks = 0:2) +
        facet_wrap(~ batter_hand) +
        labs(
            title = "Most Likely Pitch by Count",
            x = "Strikes",
            y = "Balls",
            fill = 'Pitch Type'
        ) +
        theme_minimal(base_size = 14) +
        theme(
            panel.grid = element_blank(),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
        )
    return(list(pitch_count_heatmap_plot,
                pitch_tto_heatmap_plot,
                pitch_count_grid)
           )
}   

###########################INSIGHTS############################################################
lhb_insights <- function(lhb_pitcher_scounting_report_df)  {
    return
    }

get_count_leverage_insights <- function(pitcher_scouting_report_df) {
    #  exclude 3-0 and first pitch count, NO RUNNERS ON, first time thru
    likely_pitches_count_leverage <- pitcher_scouting_report_df %>%
    filter(count != '3-0',
           count != '0-0',
           runners_on == FALSE,
           tto == 1) %>%
    group_by(count_leverage, pitch_name) %>%
    summarise(most_likely_pitch = round(mean(probability), 2),
             .groups='drop') %>%
    group_by(count_leverage) %>%
    slice_max(most_likely_pitch, n=1)

    return(likely_pitches_count_leverage)

    }