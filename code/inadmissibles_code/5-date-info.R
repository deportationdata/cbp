# load packages
library(tidyverse)
library(arrow)
library(fs)

#### PART DATE RANGE, METADATA SUMMARY ####

# paths
download_dir <- "data/inadmissibles"
metadata_dir <- file.path(download_dir, "metadata")
processed_dir <- "data/inadmissibles/processed"

# outputs 
parts_to_stack_dir <- file.path(
  processed_dir,
  "parts_to_stack"
)

unique_rows_dir <- file.path(
  processed_dir,
  "unique_rows"
)

dir.create(
  unique_rows_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

parts_metadata_path <- file.path(
  metadata_dir,
  "parts_metadata.parquet"
)

same_date_ranges_path <- file.path(
  metadata_dir,
  "same_date_ranges.parquet"
)

# list part files
part_files <- dir_ls(
  parts_to_stack_dir,
  regexp = "\\.parquet$"
)

# possible date columns, in preferred order
date_cols <- c(
  "event_created_date_time",
  "event_created_datetime"
)

# helper: convert CBP dates to date class
parse_dates <- function(x) {
  
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  
  out <- rep(
    as.Date(NA),
    length(x)
  )
  
  # Excel serial numbers, with or without decimal time
  numeric_vals <- suppressWarnings(
    as.numeric(x)
  )
  
  numeric_rows <- !is.na(numeric_vals)
  
  out[numeric_rows] <- as.Date(
    floor(numeric_vals[numeric_rows]),
    origin = "1899-12-30"
  )
  
  # ISO yyyy-mm-dd
  remaining <- is.na(out) & !is.na(x)
  
  if (any(remaining)) {
    out[remaining] <- suppressWarnings(
      as.Date(
        x[remaining],
        format = "%Y-%m-%d"
      )
    )
  }
  
  # mm/dd/yyyy
  remaining <- is.na(out) & !is.na(x)
  
  if (any(remaining)) {
    out[remaining] <- suppressWarnings(
      as.Date(
        x[remaining],
        format = "%m/%d/%Y"
      )
    )
  }
  
  # mm/dd/yy
  remaining <- is.na(out) & !is.na(x)
  
  if (any(remaining)) {
    out[remaining] <- suppressWarnings(
      as.Date(
        x[remaining],
        format = "%m/%d/%y"
      )
    )
  }
  
  out
}

# helper: summarize one part
summarize_part <- function(part_path) {
  
  # Read each part once so both date and column metadata
  # can be calculated in the same pass
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
  
  # Identify every canonical column containing at least one
  # nonmissing, nonblank value
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
  
  # Identify available date columns.
  available_cols <- intersect(
    date_cols,
    names(df)
  )
  
  # Choose the first preferred date column containing
  # at least one nonblank value
  date_col <- NA_character_
  
  for (nm in available_cols) {
    
    vals <- df[[nm]]
    
    if (
      any(
        !is.na(vals) &
        str_squish(as.character(vals)) != ""
      )
    ) {
      date_col <- nm
      break
    }
  }
  
  # parse dates if a usable date column was found
  if (is.na(date_col)) {
    
    min_date <- as.Date(NA)
    max_date <- as.Date(NA)
    
  } else {
    
    dates <- parse_dates(
      df[[date_col]]
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
    date_column = date_col,
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


#### ID SAME DATE RANGES ####

same_date_ranges <- parts_metadata |>
  filter(
    !is.na(min_date),
    !is.na(max_date)
  ) |>
  group_by(
    min_date,
    max_date
  ) |>
  filter(
    n() > 1
  ) |>
  arrange(
    min_date,
    max_date,
    part_file
  ) |>
  summarize(
    n_parts = n(),
    files_and_rows = paste0(
      part_file,
      " [rows: ",
      n_rows,
      "]",
      collapse = " | "
    ),
    .groups = "drop"
  ) |>
  arrange(
    min_date,
    max_date
  )

print(
  same_date_ranges,
  n = Inf
)

write_parquet(
  same_date_ranges,
  same_date_ranges_path
)

cat(
  "\nSame date ranges saved to:",
  same_date_ranges_path,
  "\n"
)

cat(
  "\nParts summarized:",
  nrow(parts_metadata),
  "\n"
)

cat(
  "Parts without a usable date:",
  sum(is.na(parts_metadata$min_date)),
  "\n"
)

cat(
  "Repeated date-range groups:",
  nrow(same_date_ranges),
  "\n"
)

