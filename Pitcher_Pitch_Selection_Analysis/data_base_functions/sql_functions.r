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

    df <- dbWriteTable(conn, table_name)

    dbDisconnect(conn)

    return(df)
}