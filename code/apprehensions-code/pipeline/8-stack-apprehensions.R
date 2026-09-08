# load packages
library(tidyverse)
library(arrow)
library(fs)
library(DBI)
library(duckdb)

# paths
download_dir <- "data/apprehensions"
processed_dir <- file.path(download_dir, "processed")
parts_to_stack_dir <- file.path(processed_dir, "parts-to-stack")

apprehensions_stacked_path <- file.path(
  processed_dir,
  "apprehensions-stacked.parquet"
)

apprehensions_audit_path <- file.path(
  processed_dir,
  "apprehensions-audit.parquet"
)

#### Remove Faulty Parts #### 

# to delete from parts-to-stack
parts_to_delete <- c(
  "0065-usbp-nationwide-apprehension-q4-fy-2024-fy24q4.parquet"
)
  # 0065-usbp-nationwide-apprehension-q4-fy-2024-fy24q4 is an erroneous duplicate

paths_to_delete <- file.path(
  parts_to_stack_dir,
  parts_to_delete
)

# check which files still exist
existing_parts <- paths_to_delete[
  file_exists(paths_to_delete)
]

missing_parts <- paths_to_delete[
  !file_exists(paths_to_delete)
]

# report files that were already deleted
if (length(missing_parts) > 0) {
  warning(
    "These parts were already missing and will be skipped:\n",
    paste(missing_parts, collapse = "\n")
  )
}

# delete only files that still exist
if (length(existing_parts) > 0) {
  
  file_delete(existing_parts)
  
  cat(
    "Deleted",
    length(existing_parts),
    "part(s).\n"
  )
  
} else {
  
  cat(
    "No parts needed to be deleted.\n"
  )
}

# list remaining parts 
part_files <- dir_ls(
  parts_to_stack_dir,
  regexp = "\\.parquet$"
)

parts <- tibble(
  part_path = as.character(part_files),
  part_file = path_file(part_files)
)

cat(
  "Total parts:",
  nrow(parts),
  "\n"
)

#### Audit Parts #### 

# get row counts from parquet metadata
apprehensions_audit <- parts |>
  mutate(
    n_rows = map_dbl(
      part_path,
      \(x) {
        ParquetFileReader$create(x)$num_rows
      }
    )
  )

expected_rows <- sum(
  apprehensions_audit$n_rows
)

cat(
  "Expected rows:",
  expected_rows,
  "\n"
)

#### Combine Parts #### 

# connect to DuckDB
con <- dbConnect(
  duckdb()
)

# configuration for memory reduction
dbExecute(
  con,
  "SET preserve_insertion_order = false"
)

dbExecute(
  con,
  "SET threads = 2"
)

# SQL file paths
input_glob <- file.path(
  parts_to_stack_dir,
  "*.parquet"
)

# format as SQL string, return as character 
input_sql <- as.character(
  dbQuoteString(
    con,
    input_glob
  )
)

output_sql <- as.character(
  dbQuoteString(
    con,
    apprehensions_stacked_path
  )
)

# combine parquets (%s are placeholders, inputs follow commas)
combine_query <- sprintf(
  "
  COPY (
    SELECT *
    FROM read_parquet(
      %s,
      union_by_name = true
    )
  )
  TO %s (
    FORMAT parquet,
    COMPRESSION snappy,
    USE_TMP_FILE true
  )
  ",
  input_sql,
  output_sql
)

# run command through DuckDB
dbExecute(
  con,
  combine_query
)

# disconnect 
dbDisconnect(
  con,
  shutdown = TRUE
)

#### Audit Stacked File #### 

# get final row count from metadata 
actual_rows <- ParquetFileReader$create(
  apprehensions_stacked_path
)$num_rows

if (actual_rows != expected_rows) {
  stop(
    "Final row count does not match expected rows from all remaining parts."
  )
}

cat(
  "Rows in final file:",
  actual_rows,
  "\n"
)

cat(
  "Combined dataset saved to:",
  apprehensions_stacked_path,
  "\n"
)

# save 
write_parquet(
  apprehensions_audit,
  apprehensions_audit_path
)

cat(
  "Audit saved to:",
  apprehensions_audit_path,
  "\n"
)
