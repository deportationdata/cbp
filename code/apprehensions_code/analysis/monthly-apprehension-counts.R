# load packages
library(tidyverse)
library(DBI)
library(duckdb)

# paths
apprehensions_final_path <- file.path(
  "data",
  "apprehensions",
  "processed",
  "apprehensions_final.parquet"
)

monthly_apprehensions_path <- file.path(
  "data",
  "apprehensions",
  "validation",
  "monthly-apprehensions-by-part.csv"
)

# connect to DuckDB
con <- dbConnect(
  duckdb()
)

apprehensions_final_sql <- as.character(
  dbQuoteString(
    con,
    apprehensions_final_path
  )
)

# count apprehensions per month per original part file
apprehensions_monthly_by_part <- dbGetQuery(
  con,
  sprintf(
    "
    SELECT
      source_file,
      DATE_TRUNC(
        'month',
        COALESCE(
          CAST(apprehension_datetime AS DATE),
          apprehension_date
        )
      )::DATE AS month_start,
      COUNT(*) AS apprehension_count
    FROM read_parquet(%s)
    WHERE COALESCE(
      CAST(apprehension_datetime AS DATE),
      apprehension_date
    ) >= DATE '2020-01-01'
    GROUP BY
      source_file,
      month_start
    ORDER BY
      source_file,
      month_start
    ",
    apprehensions_final_sql
  )
) |>
  as_tibble() |>
  mutate(
    month_start = as.Date(month_start)
  )

# disconnect from DuckDB
dbDisconnect(
  con,
  shutdown = TRUE
)

# save table
write_csv(
  apprehensions_monthly_by_part,
  monthly_apprehensions_path,
  na = ""
)

# view table
print(
  apprehensions_monthly_by_part,
  n = Inf
)