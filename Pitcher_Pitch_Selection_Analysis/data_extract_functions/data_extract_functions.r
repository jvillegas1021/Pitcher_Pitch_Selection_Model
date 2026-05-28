
read_df_from_sql <- function(table_name) {
    con <- dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        port = as.integer(Sys.getenv("DB_PORT")),
        sslmode = Sys.getenv("DB_SSLMODE")
      )

    df <- dbReadTable(con, table_name)

    dbDisconnect(con)

    return(df) 
    }

pull_pitcher_scouting_report_from_database <- function(pitcher_id) {

    #scouting_report table name

    table_name <- 'pitcher_scouting_reports'

    query <- paste0('SELECT * FROM ', table_name, ' WHERE pitcher_id = ', pitcher_id)
    
    con <- dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        port = as.integer(Sys.getenv("DB_PORT")),
        sslmode = Sys.getenv("DB_SSLMODE")
      )

    df <- dbGetQuery(con, query)

    dbDisconnect(con)

    return(df)
    }

pull_pitcher_info_from_database <- function(pitcher_id) {

    #scouting_report table name

    table_name <- 'active_pitcher_stats_current_year'

    query <- paste0('SELECT * FROM ', table_name, ' WHERE "xMLBAMID" = ', pitcher_id)
    
    con <- dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        port = as.integer(Sys.getenv("DB_PORT")),
        sslmode = Sys.getenv("DB_SSLMODE")
      )

    df <- dbGetQuery(con, query)

    dbDisconnect(con)

    return(df)
    }

############# READ CSV FUNCTION####################
pull_pitcher_statcast_data_from_database <- function(pitcher_id, pitch_type_list) {

    table_name <- 'statcast_2023_2026'

    query <- paste0('SELECT * FROM ', table_name, ' WHERE pitcher = ', pitcher_id)
    
    con <- dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        port = as.integer(Sys.getenv("DB_PORT")),
        sslmode = Sys.getenv("DB_SSLMODE")
      )

    df <- dbGetQuery(con, query)

    dbDisconnect(con)
    
    filtered_statcast_df <- df %>%
    filter(pitch_type %in% pitch_type_list)
    
    return(filtered_statcast_df)
    }