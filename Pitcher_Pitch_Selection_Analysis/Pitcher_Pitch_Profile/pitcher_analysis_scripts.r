library(DBI)
library(RPostgres)
library(tidyverse)
library(glue)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(readr)


create_complete_pitcher_df <- function(pitcher_id) {
    
    con <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "neondb",
    host     = "ep-crimson-forest-ahw3nquq-pooler.c-3.us-east-1.aws.neon.tech",  # Neon gives you this
    port     = 5432,
    user     = "neondb_owner",
    password = "npg_gW6jAT5xvnbc",
    sslmode  = "require"   # Neon requires SSL
    )
    
    query <- glue("
    SELECT *
    FROM blake_snell_statcast
    WHERE pitcher = {pitcher_id}
    ORDER BY game_pk, at_bat_number, pitch_number;
    ")
    
    pitcher_df <- dbGetQuery(con, query)

    dbDisconnect(con)

    return(pitcher_df)
           
    }



create_pitcher_df <- function(pitcher_id) {
    
    con <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "neondb",
    host     = "ep-crimson-forest-ahw3nquq-pooler.c-3.us-east-1.aws.neon.tech",  # Neon gives you this
    port     = 5432,
    user     = "neondb_owner",
    password = "npg_gW6jAT5xvnbc",
    sslmode  = "require"   # Neon requires SSL
    )
    
    query <- glue("SELECT
                player_name,
                pitch_name,
                stand,
                description,
                strikes,
                balls,
                release_pos_x,
                release_pos_z,
                plate_x,
                plate_z,
                team_name,
                pitcher
                FROM ind_pitcher_statcast WHERE pitcher = {pitcher_id};")
    
    pitcher_df <- dbGetQuery(con, query)

    dbDisconnect(con)

    return(pitcher_df)
           
    }

create_pitch_plot_location_per_pitch_by_stance_and_count <-function(pitcher_df,
                                                                    player_name,
                                                                    pitch,
                                                                    batter_stance = NULL,
                                                                    ball_count = NULL,
                                                                    strike_count = NULL)
    {

    gg <- NULL
    
    filtered_pitch_df <- pitcher_df %>%
    drop_na(pitch_name) %>%
    filter(pitch_name == pitch)

    if (batter_stance != "Both") {
    filtered_pitch_df <- filtered_pitch_df %>%
      filter(stand == batter_stance)
    }
    
    if (ball_count != "No Count") {
        ball_count <- as.integer(ball_count)
        filtered_pitch_df <- filtered_pitch_df %>%
        filter(balls == ball_count)
    }
    
    if (strike_count != "No Count") {
        strike_count <- as.integer(strike_count)
        filtered_pitch_df <- filtered_pitch_df %>%
        filter(strikes == strike_count)
    }

    stance_label <- if(batter_stance == "Both") "R & L" else batter_stance
    strike_label <- if (strike_count == "No Count") "X" else strike_count
    ball_label   <- if (ball_count == "No Count") "X" else ball_count
    
    if (!is.na(pitch) & nrow(filtered_pitch_df) >= 2) {
    
    pitch_location <- filtered_pitch_df %>%
                summarise(
                    avg_release_pos_x = mean(release_pos_x, na.rm = TRUE),
                    avg_release_pos_z = mean(release_pos_z, na.rm = TRUE),
                    avg_finish_pos_x = mean(plate_x, na.rm = TRUE),
                    avg_finish_pos_z = mean(plate_z, na.rm = TRUE)
                    )
    
    plot_df <- tibble(
      stage = c("Release", "Finish"),
      x     = c(pitch_location$avg_release_pos_x, pitch_location$avg_finish_pos_x),
      z     = c(pitch_location$avg_release_pos_z, pitch_location$avg_finish_pos_z)
    ) %>% drop_na(x, z)

    if (nrow(plot_df) == 0) return(NULL)
        
    gg <- ggplot(plot_df, aes(x = x, y = z, color = stage)) +
      geom_point(size = 3) +
      geom_line(aes(group = 1), linetype = "dashed", color = "black", alpha = 0.5) +
      coord_fixed(xlim = c(-3.5, 3.5), ylim = c(-1, 8)) +
      labs(
        x = "Horizontal Plate Position (ft)",
        y = "Vertical Plate Position (ft)",
        title = paste(player_name, "\n", pitch, "Release to Finish vs", stance_label,
                      "Count", ball_label, "-", strike_label),
        color = NULL
      ) +
      scale_color_manual(values = c("Release" = "blue", "Finish" = "red")) +
      annotate("rect",
               xmin = -0.85, xmax = 0.85,
               ymin = 1.5,  ymax = 3.5,
               fill = NA, color = "black",
               linetype = "dashed", linewidth = 1) +
      theme_minimal()

    }
    return(gg)  
    }

create_pitch_scatter_plot_location_per_pitch_by_stance_and_count <-function(pitcher_df,
                                                                            player_name,
                                                                            pitch,
                                                                            batter_stance = NULL,
                                                                            ball_count = NULL,
                                                                            strike_count = NULL)

    {

    gg <- NULL
    
    filtered_pitch_df <- pitcher_df %>%
    drop_na(pitch_name) %>%
    filter(pitch_name == pitch)

    if (batter_stance != "Both") {
    filtered_pitch_df <- filtered_pitch_df %>%
      filter(stand == batter_stance)
    }
    
    if (ball_count != "No Count") {
        ball_count <- as.integer(ball_count)
        filtered_pitch_df <- filtered_pitch_df %>%
        filter(balls == ball_count)
    }
    
    if (strike_count != "No Count") {
        strike_count <- as.integer(strike_count)
        filtered_pitch_df <- filtered_pitch_df %>%
        filter(strikes == strike_count)
    }


    stance_label <- if(batter_stance == "Both") "R & L" else batter_stance
    strike_label <- if (strike_count == "No Count") "X" else strike_count
    ball_label   <- if (ball_count == "No Count") "X" else ball_count

    if (!is.na(pitch)) {
    
        pitch_location_strikes <- filtered_pitch_df %>%
                filter(description %in% c("called_strike", "hit_into_play",
                                "swinging_strike", "foul", "foul_tip")) %>%
                mutate(outcome_group = "Strike events")
    
        pitch_location_balls <- filtered_pitch_df %>%
                filter(description %in% c("hit_by_pitch",
                                "swinging_strike_blocked", "ball", "blocked_ball")) %>%
                mutate(outcome_group = "Ball events")


        plot_df <- bind_rows(pitch_location_strikes, pitch_location_balls) %>%
            drop_na(plate_x, plate_z)

        if (nrow(plot_df) == 0) return(NULL)
        
        gg <- ggplot(plot_df, aes(x = plate_x, y = plate_z, color = outcome_group)) +
            geom_point(alpha = 0.8) +
            coord_fixed(xlim = c(-3.5, 3.5), ylim = c(-1, 8)) +
            labs(
                x = "Horizontal Plate Position (ft)",
                y = "Vertical Plate Position (ft)",
            title = paste(player_name, "\n", pitch, "Location vs", stance_label, "Count", ball_label, "-", strike_label),
            color = NULL
        ) +
        scale_color_manual(values = c("Strike events" = "red", "Ball events" = "blue"))
        
        gg <- gg +
            annotate("rect",
                xmin = -0.85, xmax = 0.85,
                ymin = 1.5,  ymax = 3.5,
                fill = NA, color = "black",
                linetype = "dashed", linewidth = 1)

    }
    return(gg)
    }


create_pitch_plot_pie_chart_probability <-function(pitcher_df,
                                                    player_name,
                                                    batter_stance = NULL,
                                                    ball_count = NULL,
                                                    strike_count = NULL)
    {

    gg <- NULL
    
    filtered_pitch_df <- pitcher_df %>%
    drop_na(pitch_name)

    if (batter_stance != "Both") {
    filtered_pitch_df <- filtered_pitch_df %>%
      filter(stand == batter_stance)
    }
    
    if (ball_count != "No Count") {
        ball_count <- as.integer(ball_count)
        filtered_pitch_df <- filtered_pitch_df %>%
        filter(balls == ball_count)
    }
    
    if (strike_count != "No Count") {
        strike_count <- as.integer(strike_count)
        filtered_pitch_df <- filtered_pitch_df %>%
        filter(strikes == strike_count)
    }


    pitch_probs <- filtered_pitch_df %>%
        count(pitch_name) %>%
        mutate(probability = round(n / sum(n), 4),
              label = glue("{pitch_name}-{scales::percent(probability, accuracy = 1)}")) %>%
        filter(probability > 0.01)
    

    stance_label <- if(batter_stance == "Both") "R & L" else batter_stance
    strike_label <- if (strike_count == "No Count") "X" else strike_count
    ball_label   <- if (ball_count == "No Count") "X" else ball_count

    gg <- ggplot(
        pitch_probs,
        aes(x = "", y = probability, fill = pitch_name)
    ) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(
            title = paste(player_name, "\n Pitch Probability vs", stance_label,
                  "Count", ball_label, "-", strike_label),
            color = NULL
    ) +
    geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5)
    ) 
    
    return(gg)
    }

# FOR TESTING   ##################################
run_pitcher_analysis <-function(pitcher_id, batter_stance, balls, strikes) {

                                         
    pitcher_id <- as.integer(pitcher_id)
    pitcher_data <- create_pitcher_df(pitcher_id)
    pitcher_name <- unique(pitcher_data$player_name)[1]

    balls_arg <- balls
    strikes_arg <- strikes

    pitch_name_list <- as.list(unique(pitcher_data$pitch_name))

    plots <- list()

    
    pitch_probabilities <- create_pitch_plot_pie_chart_probability(pitcher_data,
                                                                    player_name = pitcher_name,
                                                                    batter_stance = batter_stance,
                                                                    ball_count = balls_arg,
                                                                    strike_count = strikes_arg)


    if (!is.null(pitch_probabilities)) {
    plots[["pitch_probabilities"]] <- pitch_probabilities
    }

    
    for (pitch in pitch_name_list) {
      p1 <- create_pitch_plot_location_per_pitch_by_stance_and_count(
        pitcher_data, pitcher_name,
        pitch,
        batter_stance = batter_stance,
        ball_count = balls_arg,
        strike_count = strikes_arg
      )

      p2 <- create_pitch_scatter_plot_location_per_pitch_by_stance_and_count(
        pitcher_data, pitcher_name,
        pitch,
        batter_stance = batter_stance,
        ball_count = balls_arg,
        strike_count = strikes_arg
      )

      # Print them so they show up
      if (!is.null(p1)) plots[[paste0(pitch, "_location")]] <- p1 
      if (!is.null(p2)) plots[[paste0(pitch, "_scatter")]] <- p2
      
}
    return(plots)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 4) {
    stop("Usage: Rscript plot_pitcher_tendencies.R <pitcher_id> <stance> <balls> <strikes>")
  }

  pitcher_id <- as.integer(args[1])
  batter_stance <- args[2]
  ball_count <- args[3]
  strike_count <- args[4]

  run_pitcher_analysis(pitcher_id, batter_stance, ball_count, strike_count)
}
############################# pitcher analysis base runners ################################

pitch_data <- function(pitcher_df) {
# maybe pitch probabilty
pitch_probability <- pitcher_df %>%
group_by(pitch_name) %>%
summarise(count = n(),
            probability = (round(n() / nrow(pitcher_df) * 100, 2)),
            avg_release_speed = round(mean(release_speed), 2),
            avg_plate_x = round(mean(plate_x), 3),
            avg_plate_z = round(mean(plate_z), 3)
          )
    
return (pitch_probability)
}

######################## Pitcher analysis plotting using pitch_data function ##############
pitcher_base_runner_analysis <- function(pitcher_df) {

    scenarios_list <- list()
    
    base_states <- list(
    bases_empty = c(NA, NA, NA),
    runner_on_first = c(1, NA, NA),
    runner_on_second = c(NA, 1, NA),
    runner_on_third = c(NA, NA, 1),
    runners_on_first_second = c(1, 1, NA),
    runners_on_first_third = c(1, NA, 1),
    runners_on_second_third = c(NA, 1, 1),
    bases_loaded = c(1, 1, 1)
    )
        
    for (state_name in names(base_states)) {
      for (outs in 0:2) {
    
        runners <- base_states[[state_name]]
    
        df <- pitcher_df %>%
          filter(
            (is.na(on_1b) == is.na(runners[1])),
            (is.na(on_2b) == is.na(runners[2])),
            (is.na(on_3b) == is.na(runners[3])),
            outs_when_up == outs
          )
    
        scenarios_list[[paste0(state_name, "_", outs, "_outs")]] <- pitch_data(df)
      }
    }
    ##### PRINT PIE CHARTS AND LOCATION WITH RELEASE SPEED ############
    
    for (i in (1:24)) {
        scenarios_list[[i]] <- scenarios_list[[i]] %>%
        mutate(label_pie = paste0(pitch_name, " (", count, ")" ),
              label_plot = paste0(pitch_name, " - Average Release Speed ", avg_release_speed))
        gg <- ggplot(
            scenarios_list[[i]],
            aes(x = "", y = probability, fill = label_pie)
        ) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar(theta = "y") +
        labs(title = names(scenarios_list[i]),
            fill = "Pitch Name (Count)")  +
        geom_text(
            aes(label = paste0(pitch_name, " ", probability, "%")),
            position = position_stack(vjust = 0.5)
            )
        #print(gg)
    
        # Print Each Pitch Location and Release Speed
        for (pitch in (1:nrow(scenarios_list[[i]]))) {
        
        gg <- ggplot(scenarios_list[[i]][pitch,], aes(x = avg_plate_x,
                                           y = avg_plate_z)) +
          geom_point(size = 3) +
          coord_fixed(xlim = c(-3.5, 3.5), ylim = c(-1, 8)) +
          labs(
            x = "Horizontal Plate Position (ft)",
            y = "Vertical Plate Position (ft)",
            title = scenarios_list[[i]][pitch,]$label_plot,
            color = NULL
          ) +
          annotate("rect",
                   xmin = -0.85, xmax = 0.85,
                   ymin = 1.5,  ymax = 3.5,
                   fill = NA, color = "black",
                   linetype = "dashed", linewidth = 1) 
    
        #print(gg)
        }
        
        }
    return (scenarios_list)
}
######################### Helper Function #######################################
align_metric <- function(df, col, all_pitches) {
  vec <- setNames(df[[col]], df$pitch_name)
  vec <- vec[all_pitches]
  vec[is.na(vec)] <- 0
  unname(vec)
}
########################### PITCH ANALYSIS PER INNING #############################################
pitch_analysis_by_inning <- function(pitcher_df) {
    
ball_event_list <- list('ball', 'hit_by_pitch', 'blocked_ball')

pitch_analysis <- pitcher_df %>%
group_by(inning) %>%
  mutate(total_count = n()) %>%  
  group_by(inning, pitch_name) %>%
  summarise(
    pitch_count = n(),
    probability = round(pitch_count / unique(total_count) * 100, 2),
    avg_release_speed = round(mean(release_speed), 2),
    avg_plate_x = round(mean(plate_x), 3),
    avg_plate_z = round(mean(plate_z), 3),
    ball_count = sum(description %in% ball_event_list),
    strike_count = sum(!description %in% ball_event_list),
    ball_pct = round((ball_count / pitch_count), 2),
    strike_pct = round((strike_count / pitch_count), 2),
    .groups = "drop"
  )
return (pitch_analysis)
}

######################### PITCH ANALYSIS BY COUNT #################################################

pitch_analysis_by_count <- function(pitcher_df) {

ball_event_list <- list('ball', 'hit_by_pitch', 'blocked_ball')
    
pitch_analysis <- pitcher_df %>%
group_by(balls, strikes) %>%
    mutate(total_count = n()) %>%  
    group_by(balls, strikes, pitch_name) %>%
    summarise(
        pitch_count = n(),
        probability = round(pitch_count / unique(total_count) * 100, 2),
        avg_release_speed = round(mean(release_speed), 2),
        avg_plate_x = round(mean(plate_x), 3),
        avg_plate_z = round(mean(plate_z), 3),
        ball_count = sum(description %in% ball_event_list),
        strike_count = sum(!description %in% ball_event_list),
        ball_pct = round((ball_count / pitch_count), 2),
        strike_pct = round((strike_count / pitch_count), 2),
        .groups = 'drop'
  )
    
return (pitch_analysis)
}

####################### PITCH ANALYSIS BY HAND #################################################
pitch_analysis_by_batter_hand <- function(pitcher_df) {

ball_event_list <- list('ball', 'hit_by_pitch', 'blocked_ball')

pitch_analysis <- pitcher_df %>%
group_by(stand) %>%
    mutate(total_count = n()) %>%  
    group_by(stand, pitch_name) %>%
    summarise(
        pitch_count = n(),
        probability = round(pitch_count / unique(total_count) * 100, 2),
        avg_release_speed = round(mean(release_speed), 2),
        avg_plate_x = round(mean(plate_x), 3),
        avg_plate_z = round(mean(plate_z), 3),
        ball_count = sum(description %in% ball_event_list),
        strike_count = sum(!description %in% ball_event_list),
        ball_pct = round((ball_count / pitch_count), 2),
        strike_pct = round((strike_count / pitch_count), 2),
        .groups = 'drop'
  )
    
return (pitch_analysis)
}

####################### PITCH ANALYSIS BY BASE STATE ###################################
pitch_analysis_by_base_state <- function(pitcher_df) {

ball_event_list <- list('ball', 'hit_by_pitch', 'blocked_ball')
    
pitcher_df <- pitcher_df %>%
  mutate(
    runner_1b = !is.na(on_1b),
    runner_2b = !is.na(on_2b),
    runner_3b = !is.na(on_3b)
  )

pitch_analysis <- pitcher_df %>%
  mutate(
    base_state = case_when(
      !runner_1b & !runner_2b & !runner_3b ~ "none",
      runner_1b & !runner_2b & !runner_3b ~ "1B",
      !runner_1b & runner_2b & !runner_3b ~ "2B",
      !runner_1b & !runner_2b & runner_3b ~ "3B",
      runner_1b & runner_2b & !runner_3b ~ "1B_2B",
      runner_1b & !runner_2b & runner_3b ~ "1B_3B",
      !runner_1b & runner_2b & runner_3b ~ "2B_3B",
      runner_1b & runner_2b & runner_3b ~ "1B_2B_3B"
    )
  )

pitch_analysis <- pitch_analysis %>%
group_by(base_state) %>%
    mutate(total_count = n()) %>%
    group_by(base_state, pitch_name) %>%
    summarise(
        pitch_count = n(),
        probability = round(pitch_count / unique(total_count) * 100, 2),
        avg_release_speed = round(mean(release_speed), 2),
        avg_plate_x = round(mean(plate_x), 3),
        avg_plate_z = round(mean(plate_z), 3),
        ball_count = sum(description %in% ball_event_list),
        strike_count = sum(!description %in% ball_event_list),
        ball_pct = round((ball_count / pitch_count), 2),
        strike_pct = round((strike_count / pitch_count), 2),
        .groups = 'drop'
              )

return (pitch_analysis)

}

################################ PITCH ANALYSIS BY OUT ################################
pitch_analysis_by_out <- function(pitcher_df) {

ball_event_list <- list('ball', 'hit_by_pitch', 'blocked_ball')

pitch_analysis <- pitcher_df %>%
group_by(outs_when_up) %>%
    mutate(total_count = n()) %>%  
    group_by(outs_when_up, pitch_name) %>%
    summarise(
        pitch_count = n(),
        probability = round(pitch_count / unique(total_count) * 100, 2),
        avg_release_speed = round(mean(release_speed), 2),
        avg_plate_x = round(mean(plate_x), 3),
        avg_plate_z = round(mean(plate_z), 3),
        ball_count = sum(description %in% ball_event_list),
        strike_count = sum(!description %in% ball_event_list),
        ball_pct = round((ball_count / pitch_count), 2),
        strike_pct = round((strike_count / pitch_count), 2),
        .groups = 'drop'
  )
    
return (pitch_analysis)
}

############################ PITCHER SCENARIO ANALYSIS BREAKDOWN ############################
pitch_analysis_breakdown <- function(pitcher_df,
                                     inn = 1,
                                     state = 'none',
                                     outs = 0,
                                     batter_hand = 'R',
                                     ball = 0,
                                     strike = 0) {
    
pitch_by_inning <- pitch_analysis_by_inning(pitcher_df)
pitch_by_base_state <- pitch_analysis_by_base_state(pitcher_df)
pitch_by_batter_hand <- pitch_analysis_by_batter_hand(pitcher_df)
pitch_by_count <- pitch_analysis_by_count(pitcher_df)
pitch_by_out <- pitch_analysis_by_out(pitcher_df)

pitch_by_inning <- pitch_by_inning %>% filter(inning == inn)
pitch_by_base_state <- pitch_by_base_state %>% filter(base_state == state)
pitch_by_batter_hand <- pitch_by_batter_hand %>% filter(stand == batter_hand)
pitch_by_count <- pitch_by_count %>% filter(balls == ball,
                                           strikes == strike)
pitch_by_out <- pitch_by_out %>% filter(outs_when_up == outs)

all_pitches <- union(
  pitch_by_inning$pitch_name,
  union(
    pitch_by_base_state$pitch_name,
    union(
      pitch_by_batter_hand$pitch_name,
        union(pitch_by_count$pitch_name,
      pitch_by_out$pitch_name
      )
    )
  )
)

##### prob #####
inning_prob_vec      <- align_metric(pitch_by_inning, "probability", all_pitches)
base_state_prob_vec  <- align_metric(pitch_by_base_state, "probability", all_pitches)
batter_hand_prob_vec <- align_metric(pitch_by_batter_hand, "probability", all_pitches)
count_prob_vec       <- align_metric(pitch_by_count, "probability", all_pitches)
out_prob_vec         <- align_metric(pitch_by_out, "probability", all_pitches)   

##### speed ######
inning_speed_vec      <- align_metric(pitch_by_inning, "avg_release_speed", all_pitches)
base_state_speed_vec  <- align_metric(pitch_by_base_state, "avg_release_speed", all_pitches)
batter_hand_speed_vec <- align_metric(pitch_by_batter_hand, "avg_release_speed", all_pitches)
count_speed_vec       <- align_metric(pitch_by_count, "avg_release_speed", all_pitches)
out_speed_vec         <- align_metric(pitch_by_out, "avg_release_speed", all_pitches)

##### ball_pct ######
inning_ball_pct_vec      <- align_metric(pitch_by_inning, "ball_pct", all_pitches)
base_state_ball_pct_vec  <- align_metric(pitch_by_base_state, "ball_pct", all_pitches)
batter_hand_ball_pct_vec <- align_metric(pitch_by_batter_hand, "ball_pct", all_pitches)
count_ball_pct_vec       <- align_metric(pitch_by_count, "ball_pct", all_pitches)
out_ball_pct_vec         <- align_metric(pitch_by_out, "ball_pct", all_pitches)
    
##### strike_pct ######
inning_strike_pct_vec      <- align_metric(pitch_by_inning, "strike_pct", all_pitches)
base_state_strike_pct_vec  <- align_metric(pitch_by_base_state, "strike_pct", all_pitches)
batter_hand_strike_pct_vec <- align_metric(pitch_by_batter_hand, "strike_pct", all_pitches)
count_strike_pct_vec       <- align_metric(pitch_by_count, "strike_pct", all_pitches)
out_strike_pct_vec         <- align_metric(pitch_by_out, "strike_pct", all_pitches)
    
##### avg_plate_x ######
inning_avg_plate_x_vec      <- align_metric(pitch_by_inning, "avg_plate_x", all_pitches)
base_state_avg_plate_x_vec  <- align_metric(pitch_by_base_state, "avg_plate_x", all_pitches)
batter_hand_avg_plate_x_vec <- align_metric(pitch_by_batter_hand, "avg_plate_x", all_pitches)
count_avg_plate_x_vec       <- align_metric(pitch_by_count, "avg_plate_x", all_pitches)
out_avg_plate_x_vec         <- align_metric(pitch_by_out, "avg_plate_x", all_pitches)
    
##### avg_plate_z ######
inning_avg_plate_z_vec      <- align_metric(pitch_by_inning, "avg_plate_z", all_pitches)
base_state_avg_plate_z_vec  <- align_metric(pitch_by_base_state, "avg_plate_z", all_pitches)
batter_hand_avg_plate_z_vec <- align_metric(pitch_by_batter_hand, "avg_plate_z", all_pitches)
count_avg_plate_z_vec       <- align_metric(pitch_by_count, "avg_plate_z", all_pitches)
out_avg_plate_z_vec         <- align_metric(pitch_by_out, "avg_plate_z", all_pitches)
    
complete_df <- data.frame(
    pitch_name = all_pitches,
    
    inning_prob = inning_prob_vec,
    inning_speed = inning_speed_vec,
    inning_ball_pct = inning_ball_pct_vec,
    inning_strike_pct = inning_strike_pct_vec,
    inning_avg_plate_x = inning_avg_plate_x_vec,
    inning_avg_plate_z = inning_avg_plate_z_vec,
    
    base_state_prob = base_state_prob_vec,
    base_state_speed = base_state_speed_vec,
    base_state_ball_pct = base_state_ball_pct_vec,
    base_state_strike_pct = base_state_strike_pct_vec,
    base_state_avg_plate_x = base_state_avg_plate_x_vec,
    base_state_avg_plate_z = base_state_avg_plate_z_vec,
    
    batter_hand_prob = batter_hand_prob_vec,
    batter_hand_speed = batter_hand_speed_vec,
    batter_hand_ball_pct = batter_hand_ball_pct_vec,
    batter_hand_strike_pct = batter_hand_strike_pct_vec,
    batter_hand_avg_plate_x = batter_hand_avg_plate_x_vec,
    batter_hand_avg_plate_z = batter_hand_avg_plate_z_vec,
    
    count_prob = count_prob_vec,
    count_speed = count_speed_vec,
    count_ball_pct = count_ball_pct_vec,
    count_strike_pct = count_strike_pct_vec,
    count_avg_plate_x = count_avg_plate_x_vec,
    count_avg_plate_z = count_avg_plate_z_vec,

    out_prob = out_prob_vec,
    out_speed = out_speed_vec,
    out_ball_pct = out_ball_pct_vec,
    out_strike_pct = out_strike_pct_vec,
    out_avg_plate_x = out_avg_plate_x_vec,
    out_avg_plate_z = out_avg_plate_z_vec
)

inning_weight <- 0.1
batter_hand_weight <- 0.2
count_weight <- 0.5
base_state_weight <- 0.1
out_weight <- 0.1

pitch_names <- complete_df$pitch_name

pitch_prob_multiple <- (complete_df$inning_prob ^ inning_weight) *
                    (complete_df$base_state_prob ^ base_state_weight) *
                    (complete_df$batter_hand_prob ^ batter_hand_weight) *
                    (complete_df$count_prob ^ count_weight) *
                    (complete_df$out_prob ^ out_weight)
pitch_probability <- round(pitch_prob_multiple / sum(pitch_prob_multiple) * 100, 2)

pitch_speed <- complete_df %>%
  select(inning_speed, base_state_speed, batter_hand_speed, count_speed, out_speed) %>%
  rowMeans()

pitch_ball_pct <- round(rowMeans(complete_df %>%
  select(inning_ball_pct, base_state_ball_pct, batter_hand_ball_pct, count_ball_pct, out_ball_pct)) * 100, 2)

pitch_strike_pct <- round(rowMeans(complete_df %>%
  select(inning_strike_pct, base_state_strike_pct, batter_hand_strike_pct, count_strike_pct, out_strike_pct)) * 100, 2)

pitch_avg_plate_x <- rowMeans(complete_df %>%
    select(inning_avg_plate_x, base_state_avg_plate_x, batter_hand_avg_plate_x, count_avg_plate_x, out_avg_plate_x))

pitch_avg_plate_z <- rowMeans(complete_df %>%
    select(inning_avg_plate_z, base_state_avg_plate_z, batter_hand_avg_plate_z, count_avg_plate_z, out_avg_plate_z))


final_df <- data.frame(
    pitch_name = pitch_names,
    pitch_probability = pitch_probability,
    pitch_speed = pitch_speed,
    pitch_ball_pct = pitch_ball_pct,
    pitch_strike_pct = pitch_strike_pct,
    pitch_avg_plate_x = pitch_avg_plate_x,
    pitch_avg_plate_z= pitch_avg_plate_z
    )
    
return (final_df)

}
######################### PITCH AFTER PITCH BASIC BY PITCH ANALYSIS #############################

pitch_after_pitch_analysis <- function(pitcher_df) {

pitch_names <- unique(pitcher_df$pitch_name)
pitch_table_list <- list()

for (pitch in pitch_names) {

  # find all occurrences of this pitch
  idx <- which(pitcher_df$pitch_name == pitch)

  # avoid out-of-bounds
  idx <- idx[idx < nrow(pitcher_df)]

  # pitches thrown immediately after
  after_df <- pitcher_df[idx + 1, ]

  # filter out garbage
  after_df <- after_df %>%
    filter(balls >= 1 | strikes >= 1)

  # count distribution of next-pitch types
  summary_df <- after_df %>%
    group_by(pitch_name) %>%        # <-- the AFTER pitch
    summarise(count = n(), .groups = "drop") %>%
    filter(count >= 10)

  if (nrow(summary_df) == 0) next

  # add percentages
  total <- sum(summary_df$count)
  summary_df <- summary_df %>%
    mutate(
      perc = round(count / total, 4),
      prev_pitch = pitch
    ) %>%
    arrange(desc(perc))

  # store in list
  pitch_table_list[[pitch]] <- summary_df
}
    return(pitch_table_list)
}
######################PITCH AFTER PITCH GETTING AHEAD BY PITCH ANALYSIS ############################
pitch_after_pitch_getting_ahead <- function(pitcher_df) {

pitch_names <- unique(pitcher_df$pitch_name)


for (pitch in pitch_names) {

pitch_indexes <- which(pitcher_df$pitch_name == pitch)
pitch_after_df <- pitcher_df[pitch_indexes + 1, ]
pitch_after_df <- pitch_after_df %>%
filter(balls >= 1 | strikes >=1)
    
pitch_after_strike <- pitch_after_df %>%
  filter(strikes == balls + 1) %>%
  group_by(pitch_name) %>%
  summarise(
      count = n(),
      .groups = "drop") %>%
  filter(count >= 10) %>%
  mutate(
    total = sum(count),
    perc = round(count / total, 4),
    prev_pitch = pitch,
  ) %>%
  arrange(desc(perc))

return(pitch_after_strike)
}
}

######################PITCH AFTER PITCH FALLING BEHIND BY PITCH ANALYSIS ############################
pitch_after_pitch_falling_behind <- function(pitcher_df) {

pitch_names <- unique(pitcher_df$pitch_name)

for (pitch in pitch_names) {
    
pitch_indexes <- which(pitcher_df$pitch_name == pitch)
pitch_after_df <- pitcher_df[pitch_indexes + 1, ]
pitch_after_df <- pitch_after_df %>%
filter(balls >= 1 | strikes >=1)
    
pitch_after_ball <- pitch_after_df %>%
  filter(balls == strikes + 1) %>%
  group_by(pitch_name) %>%
  summarise(
      count = n(),
      .groups = "drop") %>%
  filter(count >= 10) %>%
  mutate(
    total = sum(count),
    perc = round(count / total, 4),
    prev_pitch = pitch,
  ) %>%
  arrange(desc(perc))

return(pitch_after_ball)
}
}

##################### PITCH AFTER PITCH SWINGING STRIKE ANALYSIS #################################
pitch_after_swinging_strike_analysis <- function(pitcher_df) {

pitch_names <- unique(pitcher_df$pitch_name)

for (pitch in pitch_names){
pitch_after_swinging_strike_idx <- which((pitcher_df$description == 'swinging_strike' |
                                            pitcher_df$description == 'swinging_strike_blocked') &
                                            pitcher_df$strikes != 2 &
                                            pitcher_df$pitch_name == pitch)

same_at_bat_number <- pitcher_df$at_bat_number[pitch_after_swinging_strike_idx] == 
                        pitcher_df$at_bat_number[pitch_after_swinging_strike_idx + 1]


pitch_after_swinging_strike <- pitcher_df[pitch_after_swinging_strike_idx[same_at_bat_number] + 1, ]

pitch_after_swinging_strike_grouped <- pitch_after_swinging_strike %>%
group_by(pitch_name) %>%
  summarise(
      count = n(),
      .groups = "drop") %>%
  filter(count >= 10) %>%
  mutate(
    total = sum(count),
    perc = round(count / total, 4),
    prev_pitch = pitch,
  ) %>%
  arrange(desc(perc))

return(pitch_after_swinging_strike_grouped)
}
}

######################## PLOT PITCH ACTION - BALLS , STRIKES, HIT INTO PLAY ############################
plot_pitch_action <-function(pitcher_df) {
    pitches <- unique(pitcher_df$pitch_name)
    pitch_action <- pitcher_df %>%
      group_by(pitch_name, type) %>%
      summarise(count = n(),
                avg_plate_x = mean(plate_x),
                avg_plate_z = mean(plate_z),
                .groups = "drop_last") %>%
      mutate(
        total = sum(count),
        perc = count / total
      ) %>%
      ungroup()
    for (pitch in pitches) {
    pitch_selected <- pitch_action[pitch_action$pitch_name == pitch, ]
    gg<- ggplot(pitch_selected, aes(x = avg_plate_x, y = avg_plate_z, fill = type, color = type)) +
    geom_point(size = 3) + 
    coord_fixed(xlim = c(-3.5, 3.5), ylim = c(-1, 8)) +
    labs(title = pitch) +
    annotate("rect",
                   xmin = -0.85, xmax = 0.85,
                   ymin = 1.5,  ymax = 3.5,
                   fill = NA, color = "black",
                   linetype = "dashed", linewidth = 1) +
    theme_minimal()
    print(gg)
    }
    }