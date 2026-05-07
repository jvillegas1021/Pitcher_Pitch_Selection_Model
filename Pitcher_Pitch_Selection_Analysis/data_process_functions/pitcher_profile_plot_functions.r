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