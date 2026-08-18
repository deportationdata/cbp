# load packages
library(tidyverse)
library(DBI)
library(duckdb)

# paths
inadmissibles_final_path <- file.path(
  "data",
  "inadmissibles",
  "processed",
  "inadmissibles_final.parquet"
)

monthly_inadmissibles_path <- file.path(
  "data",
  "inadmissibles",
  "validation",
  "monthly-inadmissibles-by-part.csv"
)

# connect to DuckDB
con <- dbConnect(
  duckdb()
)

inadmissibles_final_sql <- as.character(
  dbQuoteString(
    con,
    inadmissibles_final_path
  )
)

# count inadmissibles per month per original part file
inadmissibles_monthly_by_part <- dbGetQuery(
  con,
  sprintf(
    "
    SELECT
      source_file,
      DATE_TRUNC(
        'month',
        CAST(event_created_datetime AS DATE)
      )::DATE AS month_start,
      COUNT(*) AS inadmissible_count
    FROM read_parquet(%s)
    WHERE CAST(event_created_datetime AS DATE) >= DATE '2020-01-01'
    GROUP BY
      source_file,
      month_start
    ORDER BY
      source_file,
      month_start
    ",
    inadmissibles_final_sql
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
  inadmissibles_monthly_by_part,
  monthly_inadmissibles_path,
  na = ""
)

# view table
print(
  inadmissibles_monthly_by_part,
  n = Inf
)