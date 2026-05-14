write_df_to_sql <- function(table_name, df) {
    con <- dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD"),
        port = as.integer(Sys.getenv("DB_PORT")),
        sslmode = Sys.getenv("DB_SSLMODE")
      )

    dbWriteTable(con, table_name, df, append = TRUE, row.names = FALSE)

    dbDisconnect(con)
    }