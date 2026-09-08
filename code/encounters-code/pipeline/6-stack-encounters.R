# load packages
library(tidyverse)
library(arrow)
library(fs)
library(DBI)
library(duckdb)

# paths
download_dir <- "data/encounters"
processed_dir <- file.path(download_dir, "processed")
parts_to_stack_dir <- file.path(processed_dir, "parts-to-stack")

encounters_stacked_path <- file.path(
  processed_dir,
  "encounters-stacked.parquet"
)

encounters_audit_path <- file.path(
  processed_dir,
  "encounters-audit.parquet"
)

#### Remove Faulty Parts #### 
# N/A for encounters as of 8/10 5:36PM EST. 


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
encounters_audit <- parts |>
  mutate(
    n_rows = map_dbl(
      part_path,
      \(x) {
        ParquetFileReader$create(x)$num_rows
      }
    )
  )

expected_rows <- sum(
  encounters_audit$n_rows
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
    encounters_stacked_path
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

# get final row count from parquet metadata 
actual_rows <- ParquetFileReader$create(
  encounters_stacked_path
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
  encounters_stacked_path,
  "\n"
)

# save
write_parquet(
  encounters_audit,
  encounters_audit_path
)

cat(
  "Audit saved to:",
  encounters_audit_path,
  "\n"
)
