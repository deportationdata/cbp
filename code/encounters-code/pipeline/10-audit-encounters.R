# load packages
library(tidyverse)
library(fs)
library(arrow)
library(DBI)
library(duckdb)

# paths
download_dir <- "data/encounters"
processed_dir <- file.path(download_dir, "processed")
metadata_dir <- file.path(download_dir, "metadata")

encounters_stacked_path <- file.path(
  processed_dir,
  "encounters-stacked.parquet"
)

encounters_final_path <- file.path(
  processed_dir,
  "encounters-final.parquet"
)

final_column_inventory_path <- file.path(
  metadata_dir,
  "final-column-inventory.parquet"
)

dir_create(metadata_dir)


# check inputs
input_paths <- c(
  encounters_stacked_path,
  encounters_final_path
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

dbExecute(
  con,
  "SET threads = 2"
)

dbExecute(
  con,
  "SET preserve_insertion_order = false"
)

stacked_sql <- as.character(
  dbQuoteString(
    con,
    encounters_stacked_path
  )
)

final_sql <- as.character(
  dbQuoteString(
    con,
    encounters_final_path
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

# count nonmissing and nonblank values in every column
count_nonmissing <- function(path_sql, columns) {
  if (length(columns) == 0) {
    return(
      tibble(
        column = character(),
        nonmissing_rows = numeric()
      )
    )
  }
  
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
  
  counts <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      paste(
        count_expressions,
        collapse = ",\n"
      ),
      " FROM read_parquet(",
      path_sql,
      ")"
    )
  )
  
  tibble(
    column = columns,
    nonmissing_rows = map_dbl(
      columns,
      \(column) as.numeric(counts[[column]][1])
    )
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

stacked_columns <- get_columns(
  stacked_sql
)

# count total final rows
final_total_rows <- dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) AS n ",
    "FROM read_parquet(",
    final_sql,
    ")"
  )
) |>
  pull(n) |>
  as.numeric()

# count populated values in every final column
inventory_final_counts <- count_nonmissing(
  final_sql,
  final_columns
) |>
  rename(
    final_nonmissing_rows = nonmissing_rows
  )

# include stacked counts for columns present at that stage
inventory_stacked_columns <- intersect(
  final_columns,
  stacked_columns
)

inventory_stacked_counts <- count_nonmissing(
  stacked_sql,
  inventory_stacked_columns
) |>
  rename(
    stacked_nonmissing_rows = nonmissing_rows
  )

final_column_inventory <- final_column_schema |>
  left_join(
    inventory_stacked_counts,
    by = "column"
  ) |>
  left_join(
    inventory_final_counts,
    by = "column"
  ) |>
  mutate(
    total_rows = final_total_rows,
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
    is_empty = final_nonmissing_rows == 0,
    is_redaction_flag = str_ends(
      column,
      "_redacted"
    ),
    has_redaction_flag = paste0(
      column,
      "_redacted"
    ) %in% final_columns
  ) |>
  arrange(
    column_position
  )

write_parquet(
  final_column_inventory,
  final_column_inventory_path
)

dbDisconnect(
  con,
  shutdown = TRUE
)

# results
cat(
  "\nFinal column inventory:",
  final_column_inventory_path,
  "\n"
)

print(
  final_column_inventory,
  n = Inf
)