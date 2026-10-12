write_df_to_sql_replace <- function(table_name, df) {
    con <- dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        port = as.integer(Sys.getenv("DB_PORT")),
        sslmode = Sys.getenv("DB_SSLMODE")
      )

    dbWriteTable(
        conn = con,
        name = table_name,
        value = df,
        overwrite = TRUE,   # ⭐ THIS REPLACES THE TABLE
        row.names = FALSE
      )
    rows <- nrow(df)
    print(paste("Successfully replaced", rows, "to the ", table_name, "."))
    dbDisconnect(con)
}

write_df_to_sql_append <- function(table_name, df) {
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )
  
  dbWriteTable(
    conn = con,
    name = table_name,
    value = df,
    append = TRUE,
    row.names = FALSE
  )
  
  rows <- nrow(df)
  print(paste("Successfully appended", rows, "to the ", table_name, "."))
  
  dbDisconnect(con)
}

delete_games_from_historical_matchup_df <- function(table_name, game_ids) {
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = Sys.getenv("DB_SSLMODE")
  )
  
  quoted_game_ids <- sprintf("'%s'", game_ids)
  game_id_string <- paste(quoted_game_ids, collapse = ",")
  
  query <- sprintf('DELETE FROM %s WHERE "Game_ID" IN (%s)', table_name, game_id_string)
  
  rows_affected <- dbExecute(con, query)
  print(paste("Successfully cleared", rows_affected, "stale placeholder rows from database."))
  
  # 4. Clean up connection channel
  dbDisconnect(con)
}