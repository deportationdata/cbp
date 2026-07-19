# load packages
library(tidyverse)
library(arrow)
library(fs)

# paths
download_dir <- "data/apprehensions"
metadata_dir <- file.path(download_dir, "metadata")
processed_dir <- file.path(download_dir, "processed")
parts_to_stack_dir <- file.path(processed_dir, "parts_to_stack")

# outputs
unique_rows_dir <- file.path(
  processed_dir,
  "unique_rows"
)

dir.create(
  unique_rows_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

same_date_ranges_path <- file.path(
  metadata_dir,
  "same_date_ranges.parquet"
)

#### ARE SAME-DATE PAIRS EXACT MATCHES? ####

# read same-date pair
same_date <- read_parquet(same_date_ranges_path)

# extract the two file names
files <- str_split(
  same_date$files_and_rows[1],
  " \\| "
)[[1]] |>
  str_remove(" \\[rows:.*")

file_a <- file.path(parts_to_stack_dir, files[1])
file_b <- file.path(parts_to_stack_dir, files[2])

# read data sets
a <- read_parquet(file_a)
b <- read_parquet(file_b)

# helper: does a column contain at least one nonblank value?
has_data <- function(x) {
  any(
    !is.na(x) &
      str_squish(as.character(x)) != ""
  )
}

# columns that exist in both data sets
shared_cols <- intersect(
  names(a),
  names(b)
)

# exclude source columns
shared_cols <- setdiff(
  shared_cols,
  c("source_file", "source_sheet")
)

# keep only columns populated in both data sets
shared_non_empty_cols <- shared_cols[
  map_lgl(
    shared_cols,
    ~ has_data(a[[.x]]) && has_data(b[[.x]])
  )
]

if (length(shared_non_empty_cols) == 0) {
  stop("The two data sets have no shared non-empty columns to compare.")
}

cat(
  "Shared non-empty columns used for comparison:",
  length(shared_non_empty_cols),
  "\n"
)

print(shared_non_empty_cols)

# compare only shared non-empty columns
a_compare <- a |>
  select(all_of(shared_non_empty_cols))

b_compare <- b |>
  select(all_of(shared_non_empty_cols))

# rows unique to each data set based on those columns
rows_only_in_a <- anti_join(
  a_compare,
  b_compare,
  by = shared_non_empty_cols
)

rows_only_in_b <- anti_join(
  b_compare,
  a_compare,
  by = shared_non_empty_cols
)

# report results
cat("\nRows in A to compare:", nrow(a_compare), "\n")
cat("Rows in B to compare:", nrow(b_compare), "\n")
cat("Rows only in A:", nrow(rows_only_in_a), "\n")
cat("Rows only in B:", nrow(rows_only_in_b), "\n")

if (nrow(rows_only_in_a) == 0 && nrow(rows_only_in_b) == 0) {
  cat(
    "\nData sets match across all shared non-empty columns.\n"
  )
} else {
  cat(
    "\nMatching rows across shared non-empty columns:",
    nrow(a_compare) - nrow(rows_only_in_a),
    "of",
    nrow(a_compare),
    "\n"
  )
}

# save differing rows
write_parquet(
  rows_only_in_a,
  file.path(
    unique_rows_dir,
    "rows_only_in_a.parquet"
  )
)

write_parquet(
  rows_only_in_b,
  file.path(
    unique_rows_dir,
    "rows_only_in_b.parquet"
  )
)

cat(
  "\nSaved",
  nrow(rows_only_in_a),
  "rows only in A and",
  nrow(rows_only_in_b),
  "rows only in B.\n"
)

#### UNIQUE NON-EMPTY COLUMNS ####
# non-empty columns in each data set
non_empty_a <- names(a)[
  map_lgl(a, has_data)
]

non_empty_b <- names(b)[
  map_lgl(b, has_data)
]

# ignore source-tracking columns
non_empty_a <- setdiff(
  non_empty_a,
  c("source_file", "source_sheet")
)

non_empty_b <- setdiff(
  non_empty_b,
  c("source_file", "source_sheet")
)

# columns unique to each dataset
cols_only_in_a <- setdiff(
  non_empty_a,
  non_empty_b
)

cols_only_in_b <- setdiff(
  non_empty_b,
  non_empty_a
)

cat("\nColumns with data only in A:\n")
print(cols_only_in_a)

cat("\nColumns with data only in B:\n")
print(cols_only_in_b)

#### COMPARE MISMATCHED ROWS ####
# compare the sets of values in each column among unmatched rows
column_differences <- map_dfr(
  names(rows_only_in_a),
  function(col) {
    
    vals_a <- unique(na.omit(as.character(rows_only_in_a[[col]])))
    
    vals_b <- unique(na.omit(as.character(rows_only_in_b[[col]])))
    
    tibble(
      column = col,
      values_only_in_a = length(setdiff(vals_a, vals_b)),
      values_only_in_b = length(setdiff(vals_b, vals_a)),
      identical_unique_values = setequal(vals_a, vals_b)
    )
  }
)

print(column_differences, n = Inf)

write_parquet(column_differences,
  file.path(unique_rows_dir,
    "column_differences.parquet"))

#### EXPLORE SUSPICIOUS COLUMNS ####
# exclude source-tracking columns and the two suspicious columns
a2 <- a |>
  select(
    -source_file,
    -source_sheet,
    -birth_city,
    -gang_code
  )

b2 <- b |>
  select(
    -source_file,
    -source_sheet,
    -birth_city,
    -gang_code
  )

cat(
  "\nRows only in A after excluding birth_city and gang_code:",
  nrow(anti_join(a2, b2)),
  "\n"
)

cat(
  "Rows only in B after excluding birth_city and gang_code:",
  nrow(anti_join(b2, a2)),
  "\n"
)

rows_only_in_a |>
  count(gang_code, sort = TRUE) |>
  print(n = 10)

rows_only_in_b |>
  count(gang_code, sort = TRUE) |>
  print(n = 10)

rows_only_in_a |>
  count(birth_city, sort = TRUE) |>
  print(n = 10)

rows_only_in_b |>
  count(birth_city, sort = TRUE) |>
  print(n = 10)

vals_a2 <- rows_only_in_a$birth_city |>
  as.character() |>
  str_to_upper() |>
  str_replace_all("[^A-Z0-9]", "")

vals_b2 <- rows_only_in_b$birth_city |>
  as.character() |>
  str_to_upper() |>
  str_replace_all("[^A-Z0-9]", "")

cat(
  "\nNormalized birth-city values only in A:",
  length(setdiff(unique(vals_a2), unique(vals_b2))),
  "\n"
)

cat(
  "Normalized birth-city values only in B:",
  length(setdiff(unique(vals_b2), unique(vals_a2))),
  "\n"
)

df <- read_parquet("~/Desktop/DDP/cbp/data/apprehensions/metadata/same_date_ranges.parquet")
View(df)