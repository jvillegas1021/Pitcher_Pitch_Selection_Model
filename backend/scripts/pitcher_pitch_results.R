library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)
library(fmsb)

source("backend/data_extract_functions/extract_data_from_database.r")

pitcher_data <- get_data_from_database('pitcher_seasonal_data_statcast')

split_finger_df <- pitcher_data %>%
  select(all_of(starts_with('Split_Finger')))

yamamoto_df <- pitcher_data %>%
  filter(xMLBAMID == 808967) 

yamamoto_split_finger_df <- yamamoto_df %>%
  select(all_of(starts_with('Split_Finger')))

yamamoto_splitter_avg <- yamamoto_split_finger_df %>%
  summarise(
    # 1. Sum up the raw counting metrics for the whole league first
    total_pitches     = sum(Split_Finger_pitches, na.rm = TRUE),
    total_pas         = sum(Split_Finger_pas, na.rm = TRUE),
    total_abs         = sum(Split_Finger_abs, na.rm = TRUE),
    total_hits        = sum(Split_Finger_hits, na.rm = TRUE),
    total_bips        = sum(Split_Finger_bips, na.rm = TRUE),
    total_swings      = sum(Split_Finger_swings, na.rm = TRUE),
    total_whiffs      = sum(Split_Finger_whiffs, na.rm = TRUE),
    total_barrels     = sum(Split_Finger_barrels, na.rm = TRUE),
    total_in_zone     = sum(Split_Finger_in_zone, na.rm = TRUE),
    
    total_swings_out_zone  = sum(Split_Finger_swings_outside_zone, na.rm = TRUE),
    total_outside_zone     = sum(Split_Finger_outside_zone, na.rm = TRUE),
    
    total_xba_sum     = sum(Split_Finger_xba_sum, na.rm = TRUE),
    total_xwoba_sum   = sum(Split_Finger_xwoba_sum, na.rm = TRUE),
    
    # 2. Calculate the Rate Metrics / Averages
    ba          = total_hits / total_abs,
    xba         = total_xba_sum / total_abs,
    xwoba       = total_xwoba_sum / total_pas,  # Note: xwOBA uses PAs as the denominator
    
    whiff_pct   = total_whiffs / total_swings,
    zone_pct    = total_in_zone / total_pitches,
    chase_pct   = total_swings_out_zone / total_outside_zone, # (O-Swing%)
    barrel_pct  = total_barrels / total_bips   # Statcast barrels per BBE (Balled Balls In Play)
  )

league_splitter_avg <- split_finger_df %>%
  summarise(
    # 1. Sum up the raw counting metrics for the whole league first
    total_pitches     = sum(Split_Finger_pitches, na.rm = TRUE),
    total_pas         = sum(Split_Finger_pas, na.rm = TRUE),
    total_abs         = sum(Split_Finger_abs, na.rm = TRUE),
    total_hits        = sum(Split_Finger_hits, na.rm = TRUE),
    total_bips        = sum(Split_Finger_bips, na.rm = TRUE),
    total_swings      = sum(Split_Finger_swings, na.rm = TRUE),
    total_whiffs      = sum(Split_Finger_whiffs, na.rm = TRUE),
    total_barrels     = sum(Split_Finger_barrels, na.rm = TRUE),
    total_in_zone     = sum(Split_Finger_in_zone, na.rm = TRUE),
    
    total_swings_out_zone  = sum(Split_Finger_swings_outside_zone, na.rm = TRUE),
    total_outside_zone     = sum(Split_Finger_outside_zone, na.rm = TRUE),
    
    total_xba_sum     = sum(Split_Finger_xba_sum, na.rm = TRUE),
    total_xwoba_sum   = sum(Split_Finger_xwoba_sum, na.rm = TRUE),
    
    # 2. Calculate the Rate Metrics / Averages
    ba          = total_hits / total_abs,
    xba         = total_xba_sum / total_abs,
    xwoba       = total_xwoba_sum / total_pas,  # Note: xwOBA uses PAs as the denominator
    
    whiff_pct   = total_whiffs / total_swings,
    zone_pct    = total_in_zone / total_pitches,
    chase_pct   = total_swings_out_zone / total_outside_zone, # (O-Swing%)
    barrel_pct  = total_barrels / total_bips   # Statcast barrels per BBE (Balled Balls In Play)
  )

yamamoto_splitter_avg$player <- 'Yoshinobu Yamamoto'
league_splitter_avg$player <- 'League'

combined_df <- bind_rows(yamamoto_splitter_avg, league_splitter_avg)

write.csv(combined_df, 'split_finger_Yoshinobu_Yamamoto.csv')

library(dplyr)
library(tidyr)
library(ggplot2)
plot_df
# 1. Pivot the metrics you want to compare into a long format
plot_df <- combined_df %>%
  select(player, lg_ba, lg_xba, lg_whiff_pct, lg_chase_pct, lg_barrel_pct) %>%
  pivot_longer(
    cols = -player,
    names_to = "metric",
    values_to = "value"
  ) %>%
  # Give the metrics clean, readable labels for the plot x-axis
  mutate(metric_label = case_when(
    metric == "lg_ba" ~ "Batting Avg",
    metric == "lg_xba" ~ "Expected BA",
    metric == "lg_whiff_pct" ~ "Whiff %",
    metric == "lg_chase_pct" ~ "Chase %",
    metric == "lg_barrel_pct" ~ "Barrel %"
  ))

# 2. Filter for specific metrics to plot (e.g., let's look at the plate discipline/contact metrics)
discipline_plot_df <- plot_df %>% 
  filter(metric_label %in% c("Whiff %", "Chase %", "Barrel %", "Expected BA", "Batting Avg"))

# 3. Create the side-by-side bar chart
ggplot(discipline_plot_df, aes(x = metric_label, y = value, fill = player)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  # Use exact colors (e.g., Dodgers Blue vs a neutral gray for the league)
  scale_fill_manual(values = c("Yoshinobu Yamamoto" = "#005A9C", "League" = "gray60")) +
  # Convert y-axis to clean percentages
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Yoshinobu Yamamoto Splitter Profile vs. MLB League Average",
    subtitle = "Statcast Data Ingestion (2026-2026)",
    x = "Metric",
    y = "Rate",
    fill = "Context"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )
