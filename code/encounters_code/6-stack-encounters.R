# load packages
library(tidyverse)
library(arrow)
library(fs)

# paths
download_dir <- "data/encounters"
processed_dir <- file.path(download_dir, "processed")
parts_to_stack_dir <- file.path(processed_dir, "parts_to_stack")

combined_encounters_path <- file.path(
  processed_dir, 
  "combined_encounters.parquet")

encounters_audit_path <- file.path(
  processed_dir,
  "encounters_audit_path.parquet"
)


# list all parts
part_files <- dir_ls(
  parts_to_stack_dir,
  regexp = "\\.parquet$"
)

parts <- tibble(
  part_path = as.character(part_files),
  part_file = path_file(part_files)
)

cat("Total parts:", nrow(parts), "\n")

# combine all parts
combined_encounters <- map_dfr(
  parts$part_path,
  read_parquet
)

# verify expected row count
expected_rows <- parts |>
  mutate(
    n_rows = map_int(
      part_path,
      ~ read_parquet(.x, col_select = "source_file") |> nrow()
    )
  ) |>
  summarize(total = sum(n_rows)) |>
  pull(total)

if (nrow(combined_encounters) != expected_rows) {
  stop("Final row count does not match expected rows from all parts.")
}

# save combined dataset
write_parquet(
  combined_encounters,
  combined_encounters_path
)

cat(
  "\nCombined dataset saved to:",
  combined_encounters_path,
  "\n"
)

cat(
  "Rows in final dataset:",
  nrow(combined_encounters),
  "\n"
)

cat(
  "Columns in final dataset:",
  ncol(combined_encounters),
  "\n"
)

# audit of stacked parts
encounters_audit <- parts |>
  mutate(
    n_rows = map_int(
      part_path,
      ~ read_parquet(.x, col_select = "source_file") |> nrow()
    )
  )

write_parquet(
  encounters_audit,
  encounters_audit_path
)

cat(
  "Audit saved to:",
  encounters_audit_path,
  "\n"
)


# df <- read_parquet("~/Desktop/DDP/cbp/data/encounters/processed/combined_encounters.parquet")
# names(df)[colSums(!is.na(df)) == 0]
