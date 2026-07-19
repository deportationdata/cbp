# load packages
library(tidyverse)
library(arrow)
library(fs)

# paths
download_dir <- "data/inadmissibles"
processed_dir <- file.path(download_dir, "processed")
parts_to_stack_dir <- file.path(processed_dir, "parts_to_stack")

combined_inadmissibles_path <- file.path(
  processed_dir, 
  "combined_inadmissibles.parquet")

inadmissibles_audit_path <- file.path(
  processed_dir,
  "inadmissibles_audit_path.parquet"
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
combined_inadmissibles <- map_dfr(
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

if (nrow(combined_inadmissibles) != expected_rows) {
  stop("Final row count does not match expected rows from all parts.")
}

# save combined dataset
write_parquet(
  combined_inadmissibles,
  combined_inadmissibles_path
)

cat(
  "\nCombined dataset saved to:",
  combined_inadmissibles_path,
  "\n"
)

cat(
  "Rows in final dataset:",
  nrow(combined_inadmissibles),
  "\n"
)

cat(
  "Columns in final dataset:",
  ncol(combined_inadmissibles),
  "\n"
)

# audit of stacked parts
inadmissibles_audit <- parts |>
  mutate(
    n_rows = map_int(
      part_path,
      ~ read_parquet(.x, col_select = "source_file") |> nrow()
    )
  )

write_parquet(
  inadmissibles_audit,
  inadmissibles_audit_path
)

cat(
  "Audit saved to:",
  inadmissibles_audit_path,
  "\n"
)


# df <- read_parquet("~/Desktop/DDP/cbp/data/inadmissibles/processed/combined_inadmissibles.parquet")
# names(df)[colSums(!is.na(df)) == 0]
