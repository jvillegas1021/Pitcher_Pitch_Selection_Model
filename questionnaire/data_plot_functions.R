pitch_release_to_finish_plot <- ggplot(summarized_pitcher_pitchtype_level_data, aes(color = PitchType)) +
  geom_segment(aes(
    x = avg_release_x,
    y = avg_release_z,
    xend = avg_location_x,
    yend = avg_location_z),
    linewidth = 1, linetype = 'dashed') +
  geom_point(aes(x = avg_release_x, y = avg_release_z), size = 4, shape = 21, fill = "white") +
  geom_point(aes(x = avg_location_x, y = avg_location_z), size = 4) +
  
  facet_wrap(~PitcherId) + 
  
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