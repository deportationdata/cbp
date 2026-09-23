# load packages
library(tidyverse)
library(fs)
library(DBI)
library(duckdb)
library(arrow)
library(readxl)
library(janitor)

# paths
dataset_dir <- "data/apprehensions"
processed_dir <- file.path(dataset_dir, "processed")
metadata_dir <- file.path(dataset_dir, "metadata")
validation_dir <- file.path(dataset_dir, "validation")

# inputs
raw_column_inventory_path <- file.path(
  metadata_dir,
  "raw-column-inventory.parquet"
)

parts_metadata_path <- file.path(
  metadata_dir,
  "parts-metadata.parquet"
)

apprehensions_stacked_path <- file.path(
  processed_dir,
  "apprehensions-stacked.parquet"
)

apprehensions_cleaned_path <- file.path(
  processed_dir,
  "apprehensions-cleaned.parquet"
)

apprehensions_final_all_cols_path <- file.path(
  processed_dir,
  "apprehensions-final-all-cols.parquet"
)

# outputs
empty_before_cleaning_path <- file.path(
  validation_dir,
  "apprehensions-cols-empty-before-cleaning.parquet"
)

redacted_to_null_path <- file.path(
  validation_dir,
  "apprehensions-cols-redacted-to-null.parquet"
)

excluded_by_overlap_path <- file.path(
  validation_dir,
  "apprehensions-cols-excluded-by-overlap-resolution.parquet"
)

final_column_inventory_path <- file.path(
  metadata_dir,
  "final-column-inventory.parquet"
)

raw_column_matrix_path <- file.path(
  metadata_dir,
  "raw-column-matrix.parquet"
)

final_column_matrix_path <- file.path(
  metadata_dir,
  "final-column-matrix.parquet"
)

dir_create(metadata_dir)
dir_create(validation_dir)


# check inputs 
input_paths <- c(
  apprehensions_stacked_path,
  apprehensions_cleaned_path,
  apprehensions_final_all_cols_path,
  raw_column_inventory_path,
  parts_metadata_path
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
    apprehensions_stacked_path
  )
)

cleaned_sql <- as.character(
  dbQuoteString(
    con,
    apprehensions_cleaned_path
  )
)

final_sql <- as.character(
  dbQuoteString(
    con,
    apprehensions_final_all_cols_path
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
    nonmissing_rows = as.numeric(
      counts[1, columns]
    )
  )
}

# count true values in logical redaction flags
count_true <- function(path_sql, columns) {
  count_expressions <- map_chr(
    columns,
    \(column) {
      column_sql <- sql_identifier(column)

      paste0(
        "COUNT(CASE WHEN ",
        column_sql,
        " IS TRUE THEN 1 END) AS ",
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
    true_rows = as.numeric(
      counts[1, columns]
    )
  )
}


# count values by pipeline stage 
stacked_columns <- get_columns(
  stacked_sql
)

cleaned_columns <- get_columns(
  cleaned_sql
)

final_columns <- get_columns(
  final_sql
)

audit_columns <- reduce(
  list(
    stacked_columns,
    cleaned_columns,
    final_columns
  ),
  intersect
)

stacked_counts <- count_nonmissing(
  stacked_sql,
  audit_columns
) |>
  rename(
    stacked_nonmissing_rows = nonmissing_rows
  )

cleaned_counts <- count_nonmissing(
  cleaned_sql,
  audit_columns
) |>
  rename(
    cleaned_nonmissing_rows = nonmissing_rows
  )

final_counts <- count_nonmissing(
  final_sql,
  audit_columns
) |>
  rename(
    final_nonmissing_rows = nonmissing_rows
  )

column_audit <- stacked_counts |>
  left_join(
    cleaned_counts,
    by = "column"
  ) |>
  left_join(
    final_counts,
    by = "column"
  ) |>
  filter(
    final_nonmissing_rows == 0
  )


#### Empty before cleaning cols ####
empty_before_cleaning <- column_audit |>
  filter(
    stacked_nonmissing_rows == 0
  ) |>
  arrange(column)

write_parquet(
  empty_before_cleaning,
  empty_before_cleaning_path
)


#### Redacted to NULL cols ####
# i.e. cols that contained redacted vals AND were converted to non-character
# types in cleaning (should also have accompanying flag col)

redacted_columns <- column_audit |>
  mutate(
    redaction_flag_column = paste0(
      column,
      "_redacted"
    )
  ) |>
  filter(
    stacked_nonmissing_rows > 0,
    cleaned_nonmissing_rows == 0,
    redaction_flag_column %in% cleaned_columns
  )

if (nrow(redacted_columns) > 0) {
  cleaned_redaction_counts <- count_true(
    cleaned_sql,
    redacted_columns$redaction_flag_column
  ) |>
    rename(
      redaction_flag_column = column,
      cleaned_redaction_flag_rows = true_rows
    )

  final_redaction_counts <- count_true(
    final_sql,
    redacted_columns$redaction_flag_column
  ) |>
    rename(
      redaction_flag_column = column,
      final_redaction_flag_rows = true_rows
    )

  redacted_to_null <- redacted_columns |>
    left_join(
      cleaned_redaction_counts,
      by = "redaction_flag_column"
    ) |>
    left_join(
      final_redaction_counts,
      by = "redaction_flag_column"
    ) |>
    arrange(column)

} else {
  redacted_to_null <- tibble(
    column = character(),
    stacked_nonmissing_rows = numeric(),
    cleaned_nonmissing_rows = numeric(),
    final_nonmissing_rows = numeric(),
    redaction_flag_column = character(),
    cleaned_redaction_flag_rows = numeric(),
    final_redaction_flag_rows = numeric()
  )
}

write_parquet(
  redacted_to_null,
  redacted_to_null_path
)


#### Excluded by overlap resolution cols ####

excluded_by_overlap <- column_audit |>
  filter(
    cleaned_nonmissing_rows > 0
  ) |>
  mutate(
    nonmissing_rows_excluded =
      cleaned_nonmissing_rows -
      final_nonmissing_rows
  ) |>
  arrange(
    desc(nonmissing_rows_excluded),
    column
  )

write_parquet(
  excluded_by_overlap,
  excluded_by_overlap_path
)



# validate classifications
classified_columns <- union(
  empty_before_cleaning$column,
  union(
    redacted_to_null$column,
    excluded_by_overlap$column
  )
)

unclassified_columns <- column_audit |>
  filter(
    !column %in% classified_columns
  )

if (nrow(unclassified_columns) > 0) {
  warning(
    nrow(unclassified_columns),
    " empty column(s) could not be classified."
  )

  print(
    unclassified_columns,
    n = Inf
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
  pull(n)

# count populated values in every final column
inventory_final_counts <- count_nonmissing(
  final_sql,
  final_columns
) |>
  rename(
    final_nonmissing_rows = nonmissing_rows
  )

# include cleaned counts for columns present at that stage
inventory_cleaned_columns <- intersect(
  final_columns,
  cleaned_columns
)

inventory_cleaned_counts <- count_nonmissing(
  cleaned_sql,
  inventory_cleaned_columns
) |>
  rename(
    cleaned_nonmissing_rows = nonmissing_rows
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
    inventory_cleaned_counts,
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
    final_percent_missing = round(
      final_missing_rows / total_rows * 100,
      2
    ),
    is_empty = final_nonmissing_rows == 0,
    is_redaction_flag = str_ends(
      column,
      "_redacted"
    ),
    has_redaction_flag = paste0(
      column,
      "_redacted"
    ) %in% final_columns,
    nonmissing_rows_removed =
      cleaned_nonmissing_rows -
      final_nonmissing_rows
  ) |>
  arrange(
    column_position
  )

write_parquet(
  final_column_inventory,
  final_column_inventory_path
)

cat(
  "\nFinal column inventory:",
  final_column_inventory_path,
  "\n"
)

#### Source column matrices ####

raw_column_inventory <- read_parquet(
  raw_column_inventory_path,
  mmap = FALSE
)

parts_metadata <- read_parquet(
  parts_metadata_path,
  mmap = FALSE
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

# one entry per successfully profiled sheet
source_sheets <- raw_column_inventory |>
  distinct(
    file_path,
    file_name,
    sheet_name,
    header_row
  ) |>
  rename(
    source_file = file_name,
    source_sheet = sheet_name
  ) |>
  left_join(
    source_periods,
    by = c("source_file", "source_sheet")
  ) |>
  arrange(source_file, source_sheet)

if (anyDuplicated(source_sheets[c("source_file", "source_sheet")])) {
  stop("Source file/sheet identifiers are not unique.")
}

# all raw columns, including columns absent from individual sheets
raw_fields <- sort(unique(raw_column_inventory$clean_column))

#### Raw column matrix ####

# count blank/NA cells as missing
is_blank <- function(x) {
  is.na(x) | str_squish(x) == ""
}

normalize_header <- function(x) {
  x |>
    str_squish() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

raw_matrix_rows <- vector("list", nrow(source_sheets))

for (i in seq_len(nrow(source_sheets))) {
  
  info <- source_sheets[i, ]
  
  message(
    "Raw matrix: ",
    info$source_file,
    " / ",
    info$source_sheet
  )
  
  # explicit range starts at the recorded worksheet header row
  sheet <- read_excel(
    path = info$file_path,
    sheet = info$source_sheet,
    range = cell_limits(
      c(info$header_row, 1),
      c(NA, NA)
    ),
    col_names = FALSE,
    col_types = "text",
    .name_repair = "unique"
  )
  
  header <- sheet |>
    slice(1) |>
    unlist(use.names = FALSE) |>
    as.character() |>
    str_squish()
  
  keep_columns <- !is.na(header) & header != ""
  
  raw_data <- sheet[-1, keep_columns, drop = FALSE]
  header <- header[keep_columns]
  
  names(raw_data) <- make_clean_names(header)
  
  expected_fields <- raw_column_inventory |>
    filter(
      file_path == info$file_path,
      sheet_name == info$source_sheet
    ) |>
    pull(clean_column)
  
  if (!setequal(names(raw_data), expected_fields)) {
    stop(
      "Headers differ from the profiling inventory: ",
      info$source_file,
      " / ",
      info$source_sheet,
      ". Reprofile before creating matrices."
    )
  }
  
  # remove completely blank rows and repeated header rows
  blank_cells <- do.call(
    cbind,
    lapply(raw_data, is_blank)
  )
  
  header_matches <- do.call(
    cbind,
    lapply(seq_along(raw_data), function(j) {
      normalize_header(raw_data[[j]]) ==
        normalize_header(header[j])
    })
  )
  
  keep_rows <-
    rowSums(!blank_cells) > 0 &
    rowSums(header_matches, na.rm = TRUE) < 2
  
  raw_data <- raw_data[keep_rows, , drop = FALSE]
  n_records <- nrow(raw_data)
  
  field_summary <- map_dfr(names(raw_data), function(field) {
    
    values <- raw_data[[field]]
    
    tibble(
      column = field,
      percent_missing = if (n_records == 0) {
        NA_real_
      } else {
        100 * sum(is_blank(values)) / n_records
      }
    )
  })
  
  raw_matrix <- expand_grid(
    metric = "percent_missing",
    column = raw_fields
  ) |>
    left_join(
      field_summary |>
        pivot_longer(
          -column,
          names_to = "metric",
          values_to = "percent"
        ),
      by = c("column", "metric")
    ) |>
    mutate(
      value = case_when(
        !column %in% names(raw_data) ~ "Absent",
        n_records == 0 ~ "No data rows",
        TRUE ~ sprintf("%.2f%%", percent)
      )
    ) |>
    select(metric, column, value) |>
    pivot_wider(
      names_from = column,
      values_from = value
    ) |>
    mutate(
      source_file = info$source_file,
      source_sheet = info$source_sheet,
      source_start_date = info$source_start_date,
      source_end_date = info$source_end_date,
      n_rows = n_records,
      .before = 1
    )
  
  raw_matrix_rows[[i]] <- raw_matrix
}

raw_column_matrix <- bind_rows(raw_matrix_rows) |>
  arrange(source_start_date, source_file, source_sheet)

write_parquet(
  raw_column_matrix,
  raw_column_matrix_path
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

# include sources with no retained final records
matrix_sources <- source_sheets |>
  select(
    source_file,
    source_sheet,
    source_start_date,
    source_end_date
  )

final_column_matrix <- matrix_sources |>
  full_join(
    final_source_matrix,
    by = c("source_file", "source_sheet")
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
  relocate(n_rows, status, .after = source_end_date) |>
  arrange(source_start_date, source_file, source_sheet)

write_parquet(
  final_column_matrix,
  final_column_matrix_path
)

dbDisconnect(
  con,
  shutdown = TRUE
)

# results 
cat(
  "\nFinal empty columns:",
  nrow(column_audit),
  "\nEmpty before cleaning:",
  nrow(empty_before_cleaning),
  "\nRedacted and converted to null:",
  nrow(redacted_to_null),
  "\nExcluded by overlap resolution:",
  nrow(excluded_by_overlap),
  "\n"
)

cat(
  "\nEmpty-before-cleaning audit:",
  empty_before_cleaning_path,
  "\nRedaction audit:",
  redacted_to_null_path,
  "\nOverlap-resolution audit:",
  excluded_by_overlap_path,
  "\n"
)

print(
  empty_before_cleaning,
  n = Inf
)

print(
  redacted_to_null,
  n = Inf
)

print(
  excluded_by_overlap,
  n = Inf
)
# END
