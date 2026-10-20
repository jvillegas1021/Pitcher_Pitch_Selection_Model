get_pitch_palette <- function(pitch_names) {
  # Clean up unique pitch names
  unique_pitches <- unique(pitch_names)
  n <- length(unique_pitches)
  
  # Brewer.pal requires a minimum 'n' of 3 for Set2
  palette_size <- max(3, n)
  base_cols <- brewer.pal(palette_size, "Set2")[1:n]
  
  # CRITICAL: Name the colors by pitch type so ggplot maps them explicitly
  names(base_cols) <- unique_pitches
  
  list(
    border = base_cols,
    fill   = setNames(alpha(base_cols, 0.35), unique_pitches)
  )
}

create_pitcher_usage_plots <- function(pitcher_pitch_usage_df) {
  
  # 1. Combine pitch types across ALL split sheets to build a master palette list
  all_pitch_types <- unique(c(
    pitcher_pitch_usage_df[[1]]$PitchType,
    pitcher_pitch_usage_df[[2]]$PitchType,
    pitcher_pitch_usage_df[[3]]$PitchType
  ))
  
  palette <- get_pitch_palette(all_pitch_types)
  
  # General Usage Plot
  pitcher_general_usage_plot <- ggplot(pitcher_pitch_usage_df[[1]], 
                                       aes(x = "", y = pitch_usage, fill = PitchType)) +
    geom_col(width = 1, color = "black") +
    scale_fill_manual(values = palette$border) +   
    coord_polar(theta = "y") +
    theme_void() +
    labs(title = "Pitch General Usage", fill = 'PitchType') + 
    geom_text(
      aes(label = paste0(pitch_usage, " %")),
      position = position_stack(vjust = 0.5)
    )
  
  # RHB Usage Plot
  pitcher_usage_vs_rhb_plot <- ggplot(pitcher_pitch_usage_df[[2]], 
                                      aes(x = "", y = pitch_usage, fill = PitchType)) +
    geom_col(width = 1, color = "black") +
    scale_fill_manual(values = palette$border) +   
    coord_polar(theta = "y") +
    theme_void() +
    labs(title = "Pitch Usage vs RHB", fill = 'PitchType') + 
    geom_text(
      aes(label = paste0(pitch_usage, " %")),
      position = position_stack(vjust = 0.5)
    )
  
  # LHB Usage Plot
  pitcher_usage_vs_lhb_plot <- ggplot(pitcher_pitch_usage_df[[3]], 
                                      aes(x = "", y = pitch_usage, fill = PitchType)) +
    geom_col(width = 1, color = "black") +
    scale_fill_manual(values = palette$border) +   
    coord_polar(theta = "y") +
    theme_void() +
    labs(title = "Pitch Usage vs LHB", fill = 'PitchType') + 
    geom_text(
      aes(label = paste0(pitch_usage, " %")),
      position = position_stack(vjust = 0.5)
    )
  
  return(list(pitcher_general_usage_plot,
              pitcher_usage_vs_rhb_plot,
              pitcher_usage_vs_lhb_plot))
}

  
create_pitcher_pitch_visual_plots <- function(pitcher_statcast_df) {
  ### WHERE PITCHES THROWN
  pitcher_statcast_df <- pitcher_statcast_df %>%
    mutate(batter_hand = if_else(BatterSide == 'R', 'RHB', 'LHB'))
  
  pitch_general_location <- ggplot(pitcher_statcast_df, aes(TrajectoryLocationX, TrajectoryLocationZ)) +
    stat_density_2d_filled(bins = 20, show.legend = FALSE) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    labs(title='Pitch Locations (Catcher View)',
         x = "Horizontal Plate Position (ft)",
         y = "Vertical Plate Position (ft)") +
    facet_grid(batter_hand ~ PitchType) +
    annotate("rect", xmin=-0.85, xmax=0.85, ymin=1.5, ymax=3.5,
             fill=NA, color="white", linewidth=1) +
    coord_fixed(xlim = c(-2, 2), ylim = c(0, 5)) +
    theme_bw() +
    theme(panel.grid = element_blank())
  return(
    list(pitch_location = pitch_general_location))

}

create_pitcher_pitch_characteristics_plots <- function(pitcher_pitch_characteristics_df) {
  palette <- get_pitch_palette(pitcher_pitch_characteristics_df$PitchType)
  
  pitch_velo_spin_plot <- ggplot(pitcher_pitch_characteristics_df,
                                 aes(x = avg_velo, y = avg_spin, color = PitchType)) +
    scale_color_manual(values = palette$border) +
    geom_point(size = 10, shape = 21, fill = "white", stroke = 1.2) +
    geom_text(aes(label = PitchType),
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
  
  pitch_release_to_finish_plot <- ggplot(pitcher_pitch_characteristics_df, aes(color = PitchType)) +
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
  
  
  ########## HORIZONTAL MOVEMENT PLOT #################
  horizontal_movement_df <- pitcher_pitch_characteristics_df %>%
    # 1. Calculate the raw break (plate minus release)
    mutate(net_movement = avg_plate_x - avg_rel_x) %>%
    # 2. Use abs() if you want ALL pitches to move right, 
    # or leave it as net_movement if you want to show glove-side vs. arm-side break!
    mutate(normalized_plate = abs(net_movement)) %>% 
    select(PitchType, normalized_plate) %>%
    # 3. Pivot so we have a starting row (0) and an ending row (normalized_plate)
    pivot_longer(
      cols = c(normalized_plate),
      names_to = "point_type",
      values_to = "x"
    ) %>%
    # Force a matching row for the release point starting at 0
    bind_rows(
      pitcher_pitch_characteristics_df %>% 
        select(PitchType) %>% 
        mutate(point_type = "release", x = 0)
    ) %>%
    mutate(
      point_type = ifelse(point_type == "release", "release", "plate")
    )
  
  # 4. Plot the newly aligned data
  pitch_horizontal_movement_plot <- ggplot(horizontal_movement_df, aes(x = x, y = PitchType, color = PitchType, group = PitchType)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = palette$border) +
    labs(
      title = "Horizontal Movement (Normalized From 0)",
      x = "Total Horizontal Break (ft)",
      y = "Pitch Type",
      color = 'PitchType'
    ) +
    theme_bw()
  
  ########## Vertical MOVEMENT PLOT#################
  vertical_movement_df <- pitcher_pitch_characteristics_df %>%
    # 1. Calculate raw drop (plate height minus release height)
    # This will result in negative numbers since plate is lower than release
    mutate(net_drop = avg_plate_z - avg_rel_z) %>%
    select(PitchType, net_drop) %>%
    # 2. Pivot so we have the ending point row
    pivot_longer(
      cols = c(net_drop),
      names_to = "point_type",
      values_to = "z"
    ) %>%
    # 3. Force a matching row for the release point starting at 0
    bind_rows(
      pitcher_pitch_characteristics_df %>% 
        select(PitchType) %>% 
        mutate(point_type = "release", z = 0)
    ) %>%
    mutate(
      point_type = if_else(point_type == "release", "release", "plate")
    )
  
  # 4. Plot the newly aligned data
  pitch_vertical_movement_plot <- ggplot(vertical_movement_df, aes(x = PitchType, y = z, color = PitchType, group = PitchType)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = palette$border) +
    # Optional: Add a horizontal line at 0 to explicitly anchor the release baseline
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
    labs(
      title = "Vertical Drop (Normalized From Release = 0)",
      x = "Pitch Type",
      y = "Total Vertical Drop (ft)",
      color = 'Pitch Name'
    ) +
    theme_bw()
  
  
  return(list(pitch_velo_spin_plot,
              pitch_release_to_finish_plot,
              pitch_horizontal_movement_plot,
              pitch_vertical_movement_plot))
  
}