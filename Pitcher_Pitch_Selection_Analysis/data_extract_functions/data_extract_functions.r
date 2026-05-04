write_df_to_sql <- function(table_name, df) {
    conn <- dbConnect(
        RPostgres::Postgres(),
        dbname   = "neondb",
        host     = "ep-crimson-forest-ahw3nquq-pooler.c-3.us-east-1.aws.neon.tech",  # Neon gives you this
        port     = 5432,
        user     = "neondb_owner",
        password = "npg_Y0emi4RMaCdS",
        sslmode  = "require"   # Neon requires SSL
        )

    dbWriteTable(conn, table_name, df, append = TRUE, row.names = FALSE)

    dbDisconnect(conn)
    }

read_df_from_sql <- function(table_name) {
    conn <- dbConnect(
        RPostgres::Postgres(),
        dbname   = "neondb",
        host     = "ep-crimson-forest-ahw3nquq-pooler.c-3.us-east-1.aws.neon.tech",  # Neon gives you this
        port     = 5432,
        user     = "neondb_owner",
        password = "npg_Y0emi4RMaCdS",
        sslmode  = "require"   # Neon requires SSL
        )

    df <- dbReadTable(conn, table_name)

    dbDisconnect(conn)

    return(df) 
    }

pull_pitcher_scouting_report_from_database <- function(pitcher_id) {

    #scouting_report table name

    table_name <- 'pitcher_scouting_reports'

    query <- paste0('SELECT * FROM ', table_name, ' WHERE pitcher_id = ', pitcher_id)
    
    conn <- dbConnect(
        RPostgres::Postgres(),
        dbname   = "neondb",
        host     = "ep-crimson-forest-ahw3nquq-pooler.c-3.us-east-1.aws.neon.tech",  # Neon gives you this
        port     = 5432,
        user     = "neondb_owner",
        password = "npg_Y0emi4RMaCdS",
        sslmode  = "require"   # Neon requires SSL
        )

    df <- dbGetQuery(conn, query)

    dbDisconnect(conn)

    return(df)
    }

############# READ CSV FUNCTION####################
pull_pitcher_statcast_data_from_csv <- function(pitcher_id, pitch_type_list) {
    statcast_df <- read_csv("C:/Users/james.villegas/OneDrive - Rotork plc/Desktop/Baseball/statcast_25_26.csv", show_col_types = FALSE)
    filtered_statcast_df <- statcast_df %>%
    filter(pitcher == pitcher_id,
           pitch_type %in% pitch_type_list)
    return(filtered_statcast_df)
    }