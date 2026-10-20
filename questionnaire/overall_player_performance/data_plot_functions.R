pitch_release_to_finish_plot_function <- function(summarized_pitcher_pitchtype_level_data) {
  pitch_release_to_finish_plot <- ggplot(summarized_pitcher_pitchtype_level_data, aes(color = PitchType)) +
    geom_segment(aes(
      x = avg_release_x,
      y = avg_release_z,
      xend = avg_location_x,
      yend = avg_location_z),
      linewidth = .5, linetype = 'dashed') +
    geom_point(aes(x = avg_release_x, y = avg_release_z), size = 2, shape = 21, fill = "white") +
    geom_point(aes(x = avg_location_x, y = avg_location_z), size = 2) +
    
    facet_wrap(~PitcherId, ncol = 5) + 
    
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
  
  return(pitch_release_to_finish_plot)
}

pitch_location_plot_function <- function(pitch_level_data) {
  
  location_data <- pitch_level_data %>%
    filter(
      !is.na(TrajectoryLocationX),
      !is.na(TrajectoryLocationZ),
      !is.na(PitchType)
    )
  
  ggplot(
    location_data,
    aes(
      x = TrajectoryLocationX,
      y = TrajectoryLocationZ,
      color = PitchType
    )
  ) +
    geom_hline(yintercept = 0, color = "grey75") +
    geom_vline(xintercept = 0, color = "grey75") +
    geom_point(alpha = 0.8) +
    facet_wrap(~ PitcherId, ncol = 8) +
    annotate("rect",
             xmin = -0.85, xmax = 0.85,
             ymin = 1.5, ymax = 3.5,
             fill = NA, color = "black",
             linetype = "dashed", linewidth = 1) +
    labs(
      title = "Pitch Location Profile",
      subtitle = "Pitch-level summary by pitch type",
      x = "Horizontal Axis",
      y = "Vertical Axis",
      color = "Pitch Type"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

pitch_movement_plot_function <- function(pitch_level_data) {
  
  movement_data <- pitch_level_data %>%
    filter(
      !is.na(TrajectoryPfxHorz),
      !is.na(TrajectoryPfxVert),
      !is.na(PitchType),
      !is.na(PitcherId)
    )
  
  ggplot(
    movement_data,
    aes(
      x = TrajectoryPfxHorz,
      y = TrajectoryPfxVert,
      color = PitchType
    )
  ) +
    # Reference lines at zero movement
    geom_hline(
      yintercept = 0,
      color = "grey75",
      linewidth = 0.4
    ) +
    geom_vline(
      xintercept = 0,
      color = "grey75",
      linewidth = 0.4
    ) +
    
    # Movement points
    geom_point(
      alpha = 0.65,
      size = 1.8
    ) +
    
    facet_wrap(
      ~ PitcherId,
      ncol = 5
    ) +
    
    labs(
      title = "Pitch Movement by Pitcher",
      subtitle = "Horizontal vs. vertical spin-induced movement",
      x = "Horizontal Movement (inches)",
      y = "Vertical Movement (inches)",
      color = "Pitch Type"
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      plot.subtitle = element_text(
        color = "grey40"
      ),
      strip.text = element_text(
        face = "bold"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(
        color = "grey90",
        linewidth = 0.3
      ),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      panel.spacing = grid::unit(1, "lines")
    )
}

pitch_general_usage_plot_function <- function(summarized_pitch_level_data) {
  
  ggplot(
  summarized_pitch_level_data,
  aes(x = "", y = pitch_usage, fill = PitchType)
) +
  geom_col(width = 1, color = "black") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(pitch_usage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 2.5
  ) +
  facet_wrap(~PitcherId) +
  theme_void() +
  labs(
    title = "Pitch Usage by Pitcher",
    fill = "Pitch Name"
  )
}

pitcher_fastball_velocity_plot_function <- function(summarized_pitch_leve_data) {
  
  fastball_velocity_df <- summarized_pitch_leve_data %>%
  filter(PitchType %in% c("FF", "SI", "FC")) %>%
  group_by(PitcherId) %>%
  summarise(
    AvgVelocity = weighted.mean(
      avg_release_speed,
      w = pitches,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(PitcherId)
  
  ggplot(
  fastball_velocity_df,
  aes(x = PitcherId, y = AvgVelocity)
  ) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkred", size = 3) +
  scale_x_continuous(
    breaks = sort(unique(fastball_velocity$PitcherId))
  ) +
  labs(
    title = 'Average Velocity for Fastball Types (FF, SI, FC) Across Pitchers',
    x = "Pitcher ID",
    y = "Average Velocity (mph)"
  ) +
  theme_minimal()
}

pitcher_chase_rate_plot_function <- function(summarized_pitch_level_data) {
  ggplot(
    summarized_pitch_level_data,
    aes(
      x = factor(PitcherId),
      y = outside_swing_perc,
      fill = factor(PitcherId)
    )
  ) +
    geom_col() +
    geom_text(
      aes(label = paste0(round(outside_swing_perc, 1), "%")),
      vjust = 0.5,
      color = "black",
      size = 3
    ) +
    labs(
      title = "Chase Rate by Pitcher",
      x = "Pitcher ID",
      y = "Chase Rate (%)",
      fill = "Pitcher ID"
    ) +
    theme_minimal()
}

pitcher_contact_rate_plot_function <- function(summarized_pitch_level_data) {
  ggplot(
    summarized_pitch_level_data,
    aes(
      x = factor(PitcherId),
      y = contact_perc,
      fill = factor(PitcherId)
    )
  ) +
    geom_col() +
    geom_text(
      aes(label = paste0(round(contact_perc, 1), "%")),
      vjust = 0.5,
      color = "black",
      size = 3
    ) +
    labs(
      title = "Contact Rate by Pitcher",
      x = "Pitcher ID",
      y = "Contact Rate (%)",
      fill = "Pitcher ID"
    ) +
    theme_minimal()
}

