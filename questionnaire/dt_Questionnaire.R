library(tidyverse)


source("data_process_functions.R")


# pull evaluate data
pitch_level_data <- read.csv()

cleaned_pitch_data <- clean_pitch_level_data(pitch_level_data)

pitcher_details <- summarise_pitch_level_data(cleaned_pitch_data)

processed_pitch_level_data <- cleaned_pitch_data %>%
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

view(summarized_pitcher_pitchtype_level_data)


movement_plot <- ggplot(processed_pitch_level_data, 
                        aes(x = TrajectoryPfxHorz, y = TrajectoryVerticalBreakInduced, color = PitchType)) +
  
  # Add raw pitch dots with transparency so dense clusters are obvious
  geom_point(alpha = 0.5, size = 1.8) +
  
  # Add central axis helper crosshairs at (0,0) representing a spinless ball
  geom_hline(yintercept = 0, color = "gray60", linetype = "dotted") +
  geom_vline(xintercept = 0, color = "gray60", linetype = "dotted") +
  
  # Facet by PitcherId to automatically create side-by-side charts for each pitcher
  facet_wrap(~PitcherId) +
  
  # Tailor labels and titles to match executive formats
  labs(
    title = "Pitch Arsenal Architecture & Movement Profiles",
    subtitle = "Induced Vertical Break (IVB) vs. Horizontal Movement (PfxHorz)",
    x = "Horizontal Break (inches, (-) tail / (+) glove)",
    y = "Induced Vertical Break (inches)",
    color = "Pitch Type"
  ) +
  
  # Clean, minimalist presentation theme
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 12),  # Bold headings for pitcher IDs
    panel.background = element_rect(fill = "gray98", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# 3. Render the graph on your screen
print(movement_plot)

processed_pitch_level_data
