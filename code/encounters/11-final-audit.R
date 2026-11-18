# use UTC for source clock times, source time zone is unspecified
Sys.setenv(TZ = "UTC")

# load packages
library(tidyverse)
library(fs)
library(arrow)
library(readxl)
library(janitor)
library(DBI)
library(duckdb)

# paths
encounters_dataset_dir <- "data/encounters"
encounters_metadata_dir <- file.path(encounters_dataset_dir, "metadata")
encounters_processed_dir <- file.path(encounters_dataset_dir, "processed")

apprehensions_dataset_dir <- "data/apprehensions"
apprehensions_metadata_dir <- file.path(apprehensions_dataset_dir, "metadata")

# inputs
encounters_parts_metadata_path <- file.path(
  encounters_metadata_dir,
  "parts-metadata.parquet"
)

apprehensions_parts_metadata_path <- file.path(
  apprehensions_metadata_dir,
  "parts-metadata.parquet"
)



encounters_latest_path <- file.path(
  encounters_processed_dir,
  "usbp-encounters-latest.parquet"
)

# outputs
latest_column_inventory_path <- file.path(
  encounters_metadata_dir,
  "latest-column-inventory.parquet"
)

latest_missingness_matrix_path <- file.path(
  encounters_metadata_dir,
  "latest-column-missingness-matrix.parquet"
)

dir_create(encounters_metadata_dir)
dir_create(apprehensions_metadata_dir)

# check inputs
input_paths <- c(
  encounters_latest_path,
  encounters_parts_metadata_path,
  apprehensions_parts_metadata_path
)

missing_paths <- input_paths[
  !file_exists(input_paths)
]

if (length(missing_paths) > 0) {
  stop(
    "Required dataset(s) do not exist:\n",
    paste(
      missing_paths,
      collapse = "\n"
    )
  )
}

# connect to duckDB
con <- dbConnect(
  duckdb()
)

# use UTC for database timestamps
dbExecute(con, "SET TimeZone = \'UTC\'")

dbExecute(
  con,
  "SET threads = 2"
)

dbExecute(
  con,
  "SET preserve_insertion_order = false"
)

final_sql <- as.character(
  dbQuoteString(
    con,
    encounters_latest_path
  )
)


# helper functions
sql_identifier <- function(x) {
  as.character(
    dbQuoteIdentifier(
      con,
      x
    )
  )
}

get_columns <- function(path_sql) {
  dbGetQuery(
    con,
    paste0(
      "DESCRIBE SELECT * ",
      "FROM read_parquet(",
      path_sql,
      ")"
    )
  ) |>
    pull(column_name)
}

# count nonmissing and nonblank values in every column by record type
count_nonmissing <- function(path_sql, columns) {
  count_expressions <- map_chr(
    columns,
    \(column) {
      column_sql <- sql_identifier(column)
      
      paste0(
        "COUNT(CASE WHEN ",
        column_sql,
        " IS NOT NULL AND ",
        "TRIM(CAST(",
        column_sql,
        " AS VARCHAR)) <> '' ",
        "THEN 1 END) AS ",
        column_sql
      )
    }
  )
  
  dbGetQuery(
    con,
    paste0(
      'SELECT record_type AS "Record Type", COUNT(*) AS total_rows, ',
      paste(
        count_expressions,
        collapse = ",\n"
      ),
      " FROM read_parquet(",
      path_sql,
      ") GROUP BY record_type"
    )
  ) |>
    as_tibble() |>
    mutate(
      total_rows = as.numeric(total_rows),
      `Record Type` = case_when(
        `Record Type` == "Apprehension" ~ "Apprehensions",
        `Record Type` == "Encounter" ~ "Encounters",
        .default = `Record Type`
      )
    ) |>
    pivot_longer(
      all_of(columns),
      names_to = "column",
      values_to = "final_nonmissing_rows",
      values_transform = as.numeric
    )
}


#### Final col inventory ####
# get final column names, positions, and types
final_column_schema <- dbGetQuery(
  con,
  paste0(
    "DESCRIBE SELECT * ",
    "FROM read_parquet(",
    final_sql,
    ")"
  )
) |>
  as_tibble() |>
  transmute(
    column_position = row_number(),
    column = column_name,
    data_type = column_type
  )

final_columns <- final_column_schema$column

# count populated values in every final column by record type
inventory_final_counts <- count_nonmissing(
  final_sql,
  final_columns
)

final_column_inventory <- final_column_schema |>
  left_join(inventory_final_counts, by = "column") |>
  mutate(
    final_missing_rows =
      total_rows - final_nonmissing_rows,
    final_percent_missing = if_else(
      total_rows > 0,
      round(
        final_missing_rows / total_rows * 100,
        2
      ),
      NA_real_
    ),
    is_empty = final_nonmissing_rows == 0
  ) |>
  relocate(`Record Type`, .before = column_position) |>
  arrange(
    `Record Type`,
    column_position
  )

write_parquet(
  final_column_inventory,
  latest_column_inventory_path
)

#### Source column matrices ####

encounters_parts_metadata <- read_parquet(
  encounters_parts_metadata_path,
  mmap = FALSE
)

apprehensions_parts_metadata <- read_parquet(
  apprehensions_parts_metadata_path,
  mmap = FALSE
)

parts_metadata <- bind_rows(
  encounters_parts_metadata,
  apprehensions_parts_metadata
)

# observed date ranges, preserve NA if no dates are available
min_observed_date <- function(x) {
  x <- as.Date(x)
  if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
}

max_observed_date <- function(x) {
  x <- as.Date(x)
  if (all(is.na(x))) as.Date(NA) else max(x, na.rm = TRUE)
}

source_periods <- parts_metadata |>
  group_by(source_file, source_sheet) |>
  summarize(
    source_start_date = min_observed_date(min_date),
    source_end_date = max_observed_date(max_date),
    .groups = "drop"
  )



#### Final column matrix ####

# one row per source file/sheet, using retained final records
matrix_fields <- setdiff(
  final_columns,
  c("source_file", "source_sheet")
)

missing_expressions <- map_chr(matrix_fields, function(field) {
  
  field_sql <- sql_identifier(field)
  
  sprintf(
    paste0(
      "ROUND(100.0 * AVG(CASE WHEN ",
      "%s IS NULL OR ",
      "regexp_full_match(CAST(%s AS VARCHAR), '[[:space:]]*') ",
      "THEN 1.0 ELSE 0.0 END), 2) AS %s"
    ),
    field_sql,
    field_sql,
    sql_identifier(field)
  )
})

final_source_matrix <- dbGetQuery(
  con,
  paste0(
    "SELECT source_file, source_sheet, ",
    "COUNT(*) AS n_rows, ",
    paste(missing_expressions, collapse = ", "),
    " FROM read_parquet(", final_sql, ") ",
    "GROUP BY source_file, source_sheet"
  )
) |>
  as_tibble()
  
final_column_missingness_matrix <- final_source_matrix |>
  left_join(
    source_periods, by = c("source_file", "source_sheet")
    ) |>
  mutate(
    n_rows = coalesce(as.numeric(n_rows), 0),
    status = if_else(
      n_rows == 0,
      "No retained rows",
      "Retained rows"
    ),
    across(
      all_of(matrix_fields),
      ~ if_else(is.na(.x), NA_character_, sprintf("%.2f%%", .x))
    )
  ) |>
  relocate(source_start_date, source_end_date, n_rows, status, .after = source_sheet) |>
  arrange(source_start_date, source_file, source_sheet)

write_parquet(
  final_column_missingness_matrix,
  latest_missingness_matrix_path
)

dbDisconnect(
  con,
  shutdown = TRUE
)

# results
cat(
  "\nFinal column inventory:",
  latest_column_inventory_path,
  "\n"
)

print(
  final_column_inventory,
  n = Inf
)
# END
