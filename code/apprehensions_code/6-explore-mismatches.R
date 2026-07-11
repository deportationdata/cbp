# load packages
library(tidyverse)
library(arrow)
library(fs)

# paths
download_dir <- "data/apprehensions"
processed_dir <- "data/apprehensions/processed"

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

parts_metadata_path <- file.path(
  metadata_dir,
  "parts_metadata.parquet"
)

same_date_ranges_path <- file.path(
  metadata_dir,
  "same_date_ranges.parquet"
)


#### ARE SAME DATE PAIRS EXACT MATCHES? #### 
# read same date pair
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

# remove source columns
a <- a |> select(-source_file, -source_sheet)
b <- b |> select(-source_file, -source_sheet)

# rows only in each data set
only_in_a <- anti_join(a, b)
only_in_b <- anti_join(b, a)

cat("Rows in A:", nrow(a), "\n")
cat("Rows in B:", nrow(b), "\n")
cat("Rows only in A:", nrow(only_in_a), "\n")
cat("Rows only in B:", nrow(only_in_b), "\n")

if (nrow(only_in_a) == 0 && nrow(only_in_b) == 0) {
  cat("\nDatasets are identical.\n")
} else {
  cat(
    "\nMatching rows:",
    nrow(a) - nrow(only_in_a),
    "of",
    nrow(a),
    "\n"
  )
}


# save differing rows
write_parquet(
  only_in_a,
  file.path(
    unique_rows_dir,
    "rows_only_in_a.parquet"
  )
)

write_parquet(
  only_in_b,
  file.path(
    unique_rows_dir,
    "rows_only_in_b.parquet"
  )
)

cat(
  "Saved",
  nrow(only_in_a),
  "rows only in A\n and",
  nrow(only_in_b),
  "rows only in B\n"
)



#### COMPARE MISMATCHES ####
# compare unique values in each column
column_differences <- map_dfr(
  names(only_in_a),
  function(col) {
    
    vals_a <- unique(na.omit(as.character(only_in_a[[col]])))
    vals_b <- unique(na.omit(as.character(only_in_b[[col]])))
    
    tibble(
      column = col,
      values_only_in_a = length(setdiff(vals_a, vals_b)),
      values_only_in_b = length(setdiff(vals_b, vals_a)),
      identical_unique_values = setequal(vals_a, vals_b)
    )
    
  }
)

print(column_differences, n = Inf)

write_parquet(
  column_differences,
  file.path(
    unique_rows_dir,
    "column_differences.parquet"
  )
)


# explore suspicious columns 
a2 <- a |> select(-birth_city, -gang_code)
b2 <- b |> select(-birth_city, -gang_code)

nrow(anti_join(a2, b2))
nrow(anti_join(b2, a2))

only_in_a |>
  count(gang_code, sort = TRUE) |>
  print(n = Inf)

only_in_b |>
  count(gang_code, sort = TRUE) |>
  print(n = Inf)

only_in_a |>
  count(birth_city, sort = TRUE) |>
  print(n = Inf)

only_in_b |>
  count(birth_city, sort = TRUE) |>
  print(n = Inf)



vals_a2 <- only_in_a$birth_city |>
  as.character() |>
  str_to_upper() |>
  str_replace_all("[^A-Z0-9]", "")

vals_b2 <- only_in_b$birth_city |>
  as.character() |>
  str_to_upper() |>
  str_replace_all("[^A-Z0-9]", "")

length(setdiff(unique(vals_a2), unique(vals_b2)))
length(setdiff(unique(vals_b2), unique(vals_a2)))

