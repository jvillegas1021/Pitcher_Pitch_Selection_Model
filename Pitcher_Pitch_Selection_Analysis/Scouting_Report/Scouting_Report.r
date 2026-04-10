build_situation_row <- function(pitcher_df, pitcher_static_model, balls, strikes, stance, runners_on, risp, tto, prev_pitch, prev_result) {
    
    # Run prediction
    probs <- predict_pitch_probabilities(
        pitcher_df,
        pitcher_static_model,
        balls,
        strikes,
        stance,
        runners_on,
        risp,
        tto,
        prev_pitch,
        prev_result
    )
    
    # Build row
    row <- data.frame(
        balls = balls,
        strikes = strikes,
        stance = stance,
        runners_on = runners_on,
        risp = risp,
        tto = tto,
        prev_pitch = prev_pitch,
        prev_result = prev_result,
        stringsAsFactors = FALSE
    )
    
    # Add pitch probability columns dynamically
    for (pitch in names(probs)) {
        row[[pitch]] <- probs[[pitch]]
    }
    
    return(row)
}


