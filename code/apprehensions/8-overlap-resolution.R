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

apprehensions_cleaned_path <- file.path(
  processed_dir,
  "usbp-apprehensions-cleaned.parquet"
)

apprehensions_all_cols_path <- file.path(
  processed_dir,
  "usbp-apprehensions-final-all-cols.parquet"
)

parts_metadata_path <- file.path(
  metadata_dir,
  "parts-metadata.parquet"
)

resolution_audit_path <- file.path(
  metadata_dir,
  "usbp-apprehensions-overlap-resolution.parquet"
)

# check inputs
if (!file_exists(apprehensions_cleaned_path)) {
  stop("Cleaned apprehensions dataset does not exist.")
}

if (!file_exists(parts_metadata_path)) {
  stop("Parts metadata does not exist.")
}


#### Manual overlap decisions ####
include_parts <- c(
  # FY2014
  "0025-usbp-percent-20apprehensions-percent-20fy2014-q1q2-forrelease-q1.parquet",
  "0026-usbp-percent-20apprehensions-percent-20fy2014-q1q2-forrelease-q2.parquet",
  # FY2024
  "0066-usbp-nationwide-apprehension-q3-fy-2024-fy24q3.parquet",
  # FY2025
  "0071-usbp-nationwide-apprehensions-fy2025-dec-1-2025-through-march-31-2025-fy25-dec-1-202-to-mar-31-2025.parquet",
  "0068-usbp-nationwide-apprehensions-april-2025-fy25april.parquet",
  "0082-usbp-nationwide-apprehensions-may-2025-may-fy25.parquet",
  "0081-usbp-nationwide-apprehensions-june-2025-ap-ps.parquet",
  "0080-usbp-nationwide-apprehensions-july-2025-ap-ps.parquet",
  # FY2026
  "0079-usbp-nationwide-apprehensions-january-1-2026-through-march-31-2026-redacted-usbp-apprehensions.parquet"
)

fallback_parts <- c(
  # FY2014
  "0060-usbp-apprehensions-nationwide-fy14-pdf-extract.parquet",
  # FY2024
  "0078-usbp-nationwide-apprehensions-fy-2024-oct-2023-to-may-31-2024-apr-may.parquet",
  # FY2025
  "0069-usbp-nationwide-apprehensions-august-2025-aug-2025.parquet",
  # FY2026
  "0056-usbp-apprehension-data-february-march-2026-deportable-apprehensions.parquet"
)

con <- dbConnect(duckdb())

dbExecute(con, "SET TimeZone = 'UTC'")
dbExecute(con, "SET threads = 2")
dbExecute(con, "SET preserve_insertion_order = false")

cleaned_sql <- as.character(
  dbQuoteString(con, apprehensions_cleaned_path)
)

final_sql <- as.character(
  dbQuoteString(con, apprehensions_all_cols_path)
)


# find event date 
cleaned_schema <- dbGetQuery(
  con,
  paste0(
    "DESCRIBE SELECT * FROM read_parquet(",
    cleaned_sql,
    ")"
  )
) |>
  as_tibble()

cleaned_columns <- cleaned_schema$column_name

required_columns <- c("source_file", "source_sheet", "apprehension_datetime")

if (!all(required_columns %in% cleaned_columns)) {
  stop("Cleaned dataset is missing required columns.")
}

date_col_sql <- "CAST(apprehension_datetime AS DATE)"

# view with temporary date column
dbExecute(
  con,
  paste0(
    "CREATE TEMP VIEW apprehensions_dated AS ",
    "SELECT *, ",
    date_col_sql,
    " AS _event_date ",
    "FROM read_parquet(",
    cleaned_sql,
    ")"
  )
)

#### Rank metadata ####

included_sources <- dbGetQuery(
  con,
  "
  SELECT DISTINCT source_file, source_sheet
  FROM apprehensions_dated
  "
) |>
  as_tibble()

parts_metadata <- read_parquet(parts_metadata_path) |>
  semi_join(
    included_sources,
    by = c("source_file", "source_sheet")
  )

# check metadata exists for all sources 
unmatched_sources <- included_sources |>
  anti_join(
    parts_metadata,
    by = c("source_file", "source_sheet")
  )

if (nrow(unmatched_sources) > 0) {
  print(unmatched_sources, n = Inf)
  stop("Some sources could not be matched to metadata.")
}

# each source/sheet should identify one part
duplicate_sources <- parts_metadata |>
  count(source_file, source_sheet) |>
  filter(n > 1)

if (nrow(duplicate_sources) > 0) {
  print(duplicate_sources, n = Inf)
  stop("Duplicate source/sheet entries in metadata.")
}

if (anyNA(parts_metadata$min_date) ||
    anyNA(parts_metadata$max_date)) {
  stop("Some parts are missing date-span metadata.")
}

# remove paths/extensions, standardize dash characters
normalize_part <- function(x) {
  x |>
    basename() |>
    str_remove(regex("\\.parquet$", ignore_case = TRUE)) |>
    str_replace_all("[\u2212\u2013\u2014]", "-") |>
    str_to_lower()
}

# add cols to parts_metadata
parts_metadata <- parts_metadata |>
  mutate(
    part_key = normalize_part(part_file),
    date_span_days = as.integer(
      as.Date(max_date) - as.Date(min_date)
    ) + 1L
  )

# confirm all specified parts were found
manual_parts <- c(include_parts, fallback_parts)

missing_parts <- setdiff(
  normalize_part(manual_parts),
  parts_metadata$part_key
)

if (length(missing_parts) > 0) {
  print(missing_parts)
  stop("Some manually specified part names were not found.")
}

# rank priority 
parts_metadata <- parts_metadata |>
  mutate(
    priority = case_when(
      part_key %in% normalize_part(include_parts) ~ 1L,
      part_key %in% normalize_part(fallback_parts) ~ 2L,
      TRUE ~ 3L
    )
  )

# store rankings in DuckDB
dbWriteTable(
  con,
  "ranked_parts",
  parts_metadata |>
    select(
      part_file,
      source_file,
      source_sheet,
      min_date,
      max_date,
      date_span_days,
      priority
    ),
  temporary = TRUE,
  overwrite = TRUE
)

#### Select sources ####
# count observed dates
dbExecute(
  con,
  "
  CREATE TEMP TABLE source_dates AS
  SELECT DISTINCT
    source_file,
    source_sheet,
    _event_date AS event_date
  FROM apprehensions_dated
  WHERE _event_date IS NOT NULL
  "
)

# assign coverage based on part date range and assigned priorities 
dbExecute(
  con,
  "
  CREATE TEMP TABLE selected_part_intervals AS

  WITH boundaries AS (
    SELECT min_date AS start_date FROM ranked_parts
    UNION
    SELECT max_date + 1 AS start_date FROM ranked_parts
  ),

  intervals AS (
    SELECT
      start_date,
      LEAD(start_date) OVER (ORDER BY start_date) AS end_date
    FROM boundaries
  )

  SELECT p.source_file, p.source_sheet, i.start_date, i.end_date
  FROM intervals AS i
  INNER JOIN ranked_parts AS p
    ON i.start_date BETWEEN p.min_date AND p.max_date
  WHERE i.end_date IS NOT NULL
  QUALIFY ROW_NUMBER() OVER (
    PARTITION BY i.start_date
    ORDER BY p.priority ASC, p.date_span_days DESC, p.part_file ASC
  ) = 1
  "
)

#### Resolve overlaps ####

original_rows <- dbGetQuery(
  con,
  "SELECT COUNT(*) AS n FROM apprehensions_dated"
) |>
  pull(n)

missing_event_dates <- dbGetQuery(
  con,
  "
  SELECT COUNT(*) AS n
  FROM apprehensions_dated
  WHERE _event_date IS NULL
  "
) |>
  pull(n)

if (missing_event_dates > 0) {
  warning(
    missing_event_dates,
    " rows without event dates will be excluded."
  )
}

# remove output from previous runs
if (file_exists(apprehensions_all_cols_path)) {
  file_delete(apprehensions_all_cols_path)
}

# Keep rows from the source assigned to each interval.
resolution_query <- paste0(
  "COPY (",
  "\n SELECT data.* EXCLUDE (_event_date)",
  "\n FROM apprehensions_dated AS data",
  "\n INNER JOIN selected_part_intervals AS selected",
  "\n   ON data.source_file IS NOT DISTINCT FROM selected.source_file",
  "\n   AND data.source_sheet IS NOT DISTINCT FROM selected.source_sheet",
  "\n   AND data._event_date >= selected.start_date",
  "\n   AND data._event_date < selected.end_date",
  "\n)",
  "\nTO ",
  final_sql,
  " (",
  "\n FORMAT parquet,",
  "\n COMPRESSION snappy,",
  "\n USE_TMP_FILE true",
  "\n)"
)

dbExecute(con, resolution_query)

final_rows <- dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) AS n FROM read_parquet(",
    final_sql,
    ")"
  )
) |>
  pull(n)

#### Create resolution audit ####

resolution_audit <- dbGetQuery(
  con,
  "
  SELECT
    p.part_file,
    p.source_file,
    p.source_sheet,
    p.min_date,
    p.max_date,
    p.date_span_days,
    p.priority,

    COUNT(d.event_date) AS available_dates,
    COUNT(s.start_date) AS selected_dates

  FROM ranked_parts AS p

  LEFT JOIN source_dates AS d
    ON p.source_file IS NOT DISTINCT FROM d.source_file
    AND p.source_sheet IS NOT DISTINCT FROM d.source_sheet

  LEFT JOIN selected_part_intervals AS s
    ON d.source_file IS NOT DISTINCT FROM s.source_file
    AND d.source_sheet IS NOT DISTINCT FROM s.source_sheet
    AND d.event_date >= s.start_date
    AND d.event_date < s.end_date

  GROUP BY
    p.part_file,
    p.source_file,
    p.source_sheet,
    p.min_date,
    p.max_date,
    p.date_span_days,
    p.priority

  ORDER BY
    p.min_date,
    p.part_file
  "
) |>
  as_tibble() |>
  mutate(
    excluded_dates = available_dates - selected_dates,
    retained = selected_dates > 0
  )

write_parquet(
  resolution_audit,
  resolution_audit_path
)

# results
cat(
  "\nParts examined:", nrow(parts_metadata),
  "\nParts contributing records:", sum(resolution_audit$retained),
  "\nRows before resolution:", original_rows,
  "\nRows after resolution:", final_rows,
  "\nRows excluded:", original_rows - final_rows,
  "\nRows without event dates:", missing_event_dates,
  "\nFinal dataset:", apprehensions_all_cols_path,
  "\nResolution audit:", resolution_audit_path,
  "\n"
)

print(
  resolution_audit,
  n = Inf
)

dbDisconnect(con, shutdown = TRUE)

# END
