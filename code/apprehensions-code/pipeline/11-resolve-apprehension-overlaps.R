# load packages
library(tidyverse)
library(arrow)
library(fs)
library(DBI)
library(duckdb)

# paths
download_dir <- "data/apprehensions"
processed_dir <- file.path(download_dir, "processed")
metadata_dir <- file.path(download_dir, "metadata")
validation_dir <- file.path(download_dir, "validation")

apprehensions_cleaned_path <- file.path(
  processed_dir,
  "apprehensions-cleaned.parquet"
)

apprehensions_final_path <- file.path(
  processed_dir,
  "apprehensions-final.parquet"
)

parts_metadata_path <- file.path(
  metadata_dir,
  "parts-metadata.parquet"
)

resolution_audit_path <- file.path(
  validation_dir,
  "apprehensions-overlap-resolution.parquet"
)

dir_create(validation_dir)


# check inputs 
if (!file_exists(apprehensions_cleaned_path)) {
  stop(
    "Cleaned apprehensions dataset does not exist: ",
    apprehensions_cleaned_path
  )
}

if (!file_exists(parts_metadata_path)) {
  stop(
    "Parts metadata does not exist: ",
    parts_metadata_path
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

cleaned_sql <- as.character(
  dbQuoteString(
    con,
    apprehensions_cleaned_path
  )
)

final_sql <- as.character(
  dbQuoteString(
    con,
    apprehensions_final_path
  )
)



# get dataset info
cleaned_schema <- dbGetQuery(
  con,
  paste0(
    "DESCRIBE SELECT * ",
    "FROM read_parquet(",
    cleaned_sql,
    ")"
  )
) |>
  as_tibble()

cleaned_columns <- cleaned_schema$column_name

required_source_columns <- c(
  "source_file",
  "source_sheet"
)

missing_source_columns <- setdiff(
  required_source_columns,
  cleaned_columns
)

if (length(missing_source_columns) > 0) {
  stop(
    "Cleaned dataset is missing source columns: ",
    paste(
      missing_source_columns,
      collapse = ", "
    )
  )
}


# create event date 
date_expressions <- c(
  apprehension_datetime =
    "CAST(apprehension_datetime AS DATE)",
  apprehension_date =
    "apprehension_date",
  arrest_datetime =
    "CAST(arrest_datetime AS DATE)",
  arrest_date =
    "arrest_date",
  entry_date =
    "entry_date"
)

available_date_columns <- intersect(
  names(date_expressions),
  cleaned_columns
)

if (length(available_date_columns) == 0) {
  stop(
    "No usable event-date columns were found."
  )
}

event_date_sql <- paste0(
  "COALESCE(",
  paste(
    date_expressions[available_date_columns],
    collapse = ", "
  ),
  ")"
)

# only use sources that reached the cleaned dataset
included_sources <- dbGetQuery(
  con,
  paste0(
    "SELECT DISTINCT ",
    "source_file, ",
    "source_sheet ",
    "FROM read_parquet(",
    cleaned_sql,
    ")"
  )
) |>
  as_tibble()

parts_metadata <- read_parquet(
  parts_metadata_path
) |>
  semi_join(
    included_sources,
    by = c(
      "source_file",
      "source_sheet"
    )
  ) |>
  filter(
    !is.na(min_date),
    !is.na(max_date)
  ) |>
  mutate(
    date_span_days = as.integer(
      max_date - min_date
    ) + 1L
  ) |>
  arrange(
    desc(date_span_days),
    min_date,
    max_date,
    part_file
  )

unmatched_sources <- included_sources |>
  anti_join(
    parts_metadata |>
      distinct(
        source_file,
        source_sheet
      ),
    by = c(
      "source_file",
      "source_sheet"
    )
  )

if (nrow(unmatched_sources) > 0) {
  
  print(
    unmatched_sources,
    n = Inf
  )
  
  stop(
    "Some sources could not be matched to parts metadata."
  )
}

#### Rank parts #### 

# broad parts prioritized, granular parts fill gaps only
claimed_dates <- as.Date(character())

parts_metadata$selected_dates <- vector(
  "list",
  nrow(parts_metadata)
)

for (i in seq_len(nrow(parts_metadata))) {
  
  part_dates <- seq(
    parts_metadata$min_date[[i]],
    parts_metadata$max_date[[i]],
    by = "day"
  )
  
  selected_dates <- setdiff(
    part_dates,
    claimed_dates
  )
  
  parts_metadata$selected_dates[[i]] <- selected_dates
  
  claimed_dates <- union(
    claimed_dates,
    part_dates
  )
}


# create resolution tables
selected_part_dates <- parts_metadata |>
  select(
    source_file,
    source_sheet,
    selected_dates
  ) |>
  unnest_longer(
    selected_dates,
    values_to = "event_date"
  ) |>
  mutate(
    event_date = as.Date(
      event_date,
      origin = "1970-01-01"
    )
  )

if (anyDuplicated(selected_part_dates$event_date)) {
  stop(
    "At least one date was assigned to multiple parts."
  )
}

resolution_audit <- parts_metadata |>
  transmute(
    part_file,
    source_file,
    source_sheet,
    min_date,
    max_date,
    date_span_days,
    selected_dates = map_int(
      selected_dates,
      length
    ),
    retained = selected_dates > 0
  )

write_parquet(
  resolution_audit,
  resolution_audit_path
)


# validate event dates 
missing_event_dates <- dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) AS n ",
    "FROM read_parquet(",
    cleaned_sql,
    ") ",
    "WHERE ",
    event_date_sql,
    " IS NULL"
  )
) |>
  pull(n)

if (missing_event_dates > 0) {
  warning(
    missing_event_dates,
    " blank row(s) without a usable event date will be excluded."
  )
}


#### Resolve overlaps ####

dbWriteTable(
  con,
  "selected_part_dates",
  selected_part_dates,
  temporary = TRUE,
  overwrite = TRUE
)

original_rows <- dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) AS n ",
    "FROM read_parquet(",
    cleaned_sql,
    ")"
  )
) |>
  pull(n)

# remove output from a previous run
if (file_exists(apprehensions_final_path)) {
  file_delete(apprehensions_final_path)
}

resolution_query <- paste0(
  "COPY (",
  "\n  SELECT data.*",
  "\n  FROM read_parquet(",
  cleaned_sql,
  ") AS data",
  "\n  INNER JOIN selected_part_dates AS selected",
  "\n    ON data.source_file = selected.source_file",
  "\n    AND data.source_sheet = selected.source_sheet",
  "\n    AND ",
  event_date_sql,
  " = selected.event_date",
  "\n)",
  "\nTO ",
  final_sql,
  " (",
  "\n  FORMAT parquet,",
  "\n  COMPRESSION snappy,",
  "\n  USE_TMP_FILE true",
  "\n)"
)

dbExecute(
  con,
  resolution_query
)



# validate final dataset 
final_rows <- dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) AS n ",
    "FROM read_parquet(",
    final_sql,
    ")"
  )
) |>
  pull(n)

if (final_rows > original_rows) {
  stop(
    "Final dataset has more rows than the cleaned dataset."
  )
}

dbDisconnect(
  con,
  shutdown = TRUE
)



# results 
cat(
  "\nParts examined:",
  nrow(parts_metadata),
  "\nParts contributing records:",
  sum(resolution_audit$retained),
  "\nRows before resolution:",
  original_rows,
  "\nRows after resolution:",
  final_rows,
  "\nOverlapping rows removed:",
  original_rows - final_rows,
  "\nCleaned dataset:",
  apprehensions_cleaned_path,
  "\nFinal dataset:",
  apprehensions_final_path,
  "\nResolution audit:",
  resolution_audit_path,
  "\n"
)

print(
  resolution_audit,
  n = Inf
)

