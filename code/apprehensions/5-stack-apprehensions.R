# use UTC for source clock times, source time zone is unspecified
Sys.setenv(TZ = "UTC")

# load packages
library(tidyverse)
library(arrow)
library(fs)
library(DBI)
library(duckdb)

# paths
dataset_dir <- "data/apprehensions"
processed_dir <- file.path(dataset_dir, "processed")
metadata_dir <- file.path(dataset_dir, "metadata")
parts_to_stack_dir <- file.path(processed_dir, "parts-to-stack")

apprehensions_stacked_path <- file.path(
  processed_dir,
  "apprehensions-stacked.parquet"
)

apprehensions_audit_path <- file.path(
  processed_dir,
  "apprehensions-audit.parquet"
)

parts_metadata_path <- file.path(
  metadata_dir,
  "parts-metadata.parquet"
)

dir_create(metadata_dir)

#### Remove Faulty Parts #### 

# to delete from parts-to-stack
parts_to_delete <- c(
  "0067-usbp-nationwide-apprehension-q4-fy-2024-fy24q4.parquet"
)
  # 0067-usbp-nationwide-apprehension-q4-fy-2024-fy24q4.parquet is an erroneous duplicate

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

#### Part Date Range ####

# possible date columns, in preferred order
date_columns <- c(
  "apprehension_datetime",
  "apprehension_date",
  "arrest_datetime",
  "arrest_date"
)

# convert CBP dates to date class
parse_dates <- function(x) {
  
  # blanks 
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  
  out <- rep(
    as.Date(NA),
    length(x)
  )
  
  # Excel serial numbers (with or without decimal time)
  numeric_vals <- suppressWarnings(
    as.numeric(x)
  )
  
  numeric_rows <- !is.na(numeric_vals)
  
  out[numeric_rows] <- as.Date(
    floor(numeric_vals[numeric_rows]),
    origin = "1899-12-30")
  
  # yyyy-mm-dd
  remaining <- is.na(out) & !is.na(x)
  
  if (any(remaining)) {
    out[remaining] <- suppressWarnings(
      as.Date(
        x[remaining],
        format = "%Y-%m-%d")
    )
  }
  
  # mm/dd/yyyy
  remaining <- is.na(out) & !is.na(x) &
    str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
  
  if (any(remaining)) {
    out[remaining] <- suppressWarnings(
      as.Date(x[remaining], format = "%m/%d/%Y")
    )
  }
  
  # mm/dd/yy
  remaining <- is.na(out) & !is.na(x) &
    str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{2}$")
  
  if (any(remaining)) {
    out[remaining] <- suppressWarnings(
      as.Date(x[remaining], format = "%m/%d/%y")
    )
  }
  
  out
}

# summarize one part
summarize_part <- function(part_path) {
  
  # read each part once to find date and col metadata in same pass
  df <- read_parquet(part_path)
  
  part_file <- path_file(part_path)
  
  source_file_value <- unique(df$source_file)
  source_sheet_value <- unique(df$source_sheet)
  
  if (
    length(source_file_value) != 1 ||
    length(source_sheet_value) != 1
  ) {
    stop(
      "Part contains more than one source file or sheet: ",
      part_file
    )
  }
  
  # identify all nonempty columns
  non_empty_cols <- names(df)[
    map_lgl(
      df,
      ~ any(
        !is.na(.x) &
          str_squish(as.character(.x)) != ""
      )
    )
  ]
  
  non_empty_cols <- setdiff(
    non_empty_cols,
    c("source_file", "source_sheet")
  )
  
  # identify available date columns
  available_cols <- intersect(
    date_columns,
    names(df)
  )
  
  # choose first preferred date column with at least one nonblank value
  date_column <- NA_character_
  
  for (nm in available_cols) {
    
    vals <- df[[nm]]
    
    if (
      any(
        !is.na(vals) &
        str_squish(as.character(vals)) != ""
      )
    ) {
      date_column <- nm
      break
    }
  }
  
  # parse dates if usable date column was found
  if (is.na(date_column)) {
    
    min_date <- as.Date(NA)
    max_date <- as.Date(NA)
    
  } else {
    
    dates <- parse_dates(
      df[[date_column]]
    )
    
    valid_dates <- dates[
      !is.na(dates)
    ]
    
    if (length(valid_dates) == 0) {
      min_date <- as.Date(NA)
      max_date <- as.Date(NA)
    } else {
      min_date <- min(valid_dates)
      max_date <- max(valid_dates)
    }
  }
  
  tibble(
    part_file = part_file,
    source_file = source_file_value[[1]],
    source_sheet = source_sheet_value[[1]],
    n_rows = nrow(df),
    date_column = date_column,
    min_date = min_date,
    max_date = max_date,
    n_non_empty_cols = length(non_empty_cols),
    non_empty_cols = paste(
      sort(non_empty_cols),
      collapse = " | "
    )
  )
}

# summarize all parts
parts_metadata <- map_dfr(
  part_files,
  summarize_part
) |>
  arrange(
    min_date,
    max_date,
    part_file
  )

# save complete part metadata
write_parquet(
  parts_metadata,
  parts_metadata_path
)

cat(
  "\nParts metadata saved to:",
  parts_metadata_path,
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

# use UTC for database timestamps
dbExecute(con, "SET TimeZone = \'UTC\'")

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
# END
