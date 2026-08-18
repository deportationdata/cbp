# load packages
library(tidyverse)
library(DBI)
library(duckdb)

# paths
encounters_final_path <- file.path(
  "data",
  "encounters",
  "processed",
  "encounters_final.parquet"
)

monthly_encounters_path <- file.path(
  "data",
  "encounters",
  "validation",
  "monthly-encounters-by-part.csv"
)

# connect to DuckDB
con <- dbConnect(
  duckdb()
)

encounters_final_sql <- as.character(
  dbQuoteString(
    con,
    encounters_final_path
  )
)

# count encounters per month per original part file
encounters_monthly_by_part <- dbGetQuery(
  con,
  sprintf(
    "
    SELECT
      source_file,
      DATE_TRUNC(
        'month',
        COALESCE(
          CAST(encounter_datetime AS DATE),
          CAST(apprehension_datetime AS DATE),
          apprehension_date
        )
      )::DATE AS month_start,
      COUNT(*) AS encounter_count
    FROM read_parquet(%s)
    WHERE COALESCE(
      CAST(encounter_datetime AS DATE),
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
    encounters_final_sql
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
  encounters_monthly_by_part,
  monthly_encounters_path,
  na = ""
)

# view table
print(
  encounters_monthly_by_part,
  n = Inf
)