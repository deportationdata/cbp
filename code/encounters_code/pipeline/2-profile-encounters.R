# load packages
library(tidyverse)
library(readxl)
library(janitor)
library(arrow)
library(fs)

# paths
download_dir <- "data/encounters"
raw_dir <- file.path(download_dir, "raw")
metadata_dir <- file.path(download_dir, "metadata")

dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)

# outputs
sheet_inventory_path <- file.path(metadata_dir, "sheet_inventory.parquet")
column_inventory_path <- file.path(metadata_dir, "column_inventory.parquet")
distinct_columns_path <- file.path(metadata_dir, "distinct_columns.parquet")
failed_sheets_path <- file.path(metadata_dir, "failed_sheets.parquet")

# list files
raw_files <- dir_ls(
  raw_dir,
  regexp = "\\.(xlsx|xls)$"
)

#### Rebuild Profiling Metadata? ####
  # FALSE = only profile new files
  # TRUE = rebuild everything
# IMPORTANT:
# If force_reprofile is set to TRUE, force_rebuild must also be
# set to TRUE in 4-process-parts.R 
force_reprofile <- FALSE

if (
  !force_reprofile &&
  file.exists(sheet_inventory_path)
) {
  
  previous_inventory <- read_parquet(sheet_inventory_path)
  
  previous_files <- unique(previous_inventory$file_path)
  
  new_files <- setdiff(raw_files, previous_files)
  
  if (length(new_files) == 0) {
    
    message("No new Excel files detected. Skipping profiling.")
    
    stop("Nothing to profile.")
    
  } else {
    
    message(length(new_files), " new file(s) detected.")
    raw_files <- new_files
    
  }
}


# old items inventory
old_sheet_inventory <- tibble()
old_column_inventory <- tibble()
old_failed_sheets <- tibble()

if (!force_reprofile) {
  if (file.exists(sheet_inventory_path)) {
    old_sheet_inventory <- read_parquet(sheet_inventory_path)
  }
  if (file.exists(column_inventory_path)) {
    old_column_inventory <- read_parquet(column_inventory_path)
  }
  if (file.exists(failed_sheets_path)) {
    old_failed_sheets <- read_parquet(failed_sheets_path)
  }
}


# remove temp excel lock files
raw_files <- raw_files[
  !str_detect(path_file(raw_files), "^~\\$")
]

# drop fully empty columns
drop_empty_columns <- function(df) {
  
  df |>
    select(
      where(
        ~ any(
          !is.na(.x) &
            str_squish(as.character(.x)) != ""
        )
      )
    )
}

# detect likely header row
find_header_row <- function(file_path, sheet, n_max = 100, min_matches = 3) {
  
  header_terms <- paste(
    c(
      "border", "disposition", "citizenship", "demographic",
      "gender", "latitude", "transferred_to_group", "mpp_indicator",
      "ces", "app_age", "smuggled_cost",
      "statute_charge", "city_of_residence", "arrest_at_checkpoint"),
    collapse = "|"
  )
  
  raw_df <- read_excel(
    path = file_path,
    sheet = sheet,
    col_names = FALSE,
    n_max = n_max,
    .name_repair = "unique"
  ) |>
    mutate(across(everything(), as.character))
  
  row_scores <- raw_df |>
    mutate(row_number = row_number()) |>
    pivot_longer(
      -row_number,
      values_to = "value"
    ) |>
    mutate(
      value = make_clean_names(str_squish(value)),
      has_header_term = str_detect(value, header_terms)
    ) |>
    group_by(row_number) |>
    summarize(
      header_matches = sum(has_header_term, na.rm = TRUE),
      .groups = "drop"
    )
  
  header_row <- row_scores |>
    filter(header_matches >= min_matches) |>
    slice(1) |>
    pull(row_number)
  
  if (length(header_row) == 0) {
    return(NA_integer_)
  }
  
  header_row
}

# profile one sheet
profile_sheet <- function(file_path, sheet) {
  
  header_row <- find_header_row(
    file_path,
    sheet
  )
  
  if (is.na(header_row)) {
    stop("No likely header row found.")
  }
  
  raw_df <- read_excel(
    path = file_path,
    sheet = sheet,
    col_names = FALSE,
    skip = header_row - 1,
    .name_repair = "unique"
  ) |>
    drop_empty_columns()
  
  header <- raw_df |>
    slice(1) |>
    unlist(use.names = FALSE) |>
    as.character() |>
    str_squish()
  
  keep_cols <- !is.na(header) & header != ""
  header <- header[keep_cols]
  
  tibble(
    file_name = path_file(file_path),
    file_path = as.character(file_path),
    sheet_name = sheet,
    header_row = header_row,
    rows_to_skip = header_row - 1,
    ncol = length(header),
    raw_column = header,
    clean_column = make_clean_names(header),
    column_position = seq_along(header)
  )
}

# sheet inventory
sheet_inventory <- map_dfr(
  raw_files,
  ~ tibble(
    file_name = path_file(.x),
    file_path = as.character(.x),
    sheet_name = excel_sheets(.x)
  )
)

# safely process sheets
profile_results <- sheet_inventory |>
  mutate(
    result = map2(
      file_path,
      sheet_name,
      ~ safely(profile_sheet)(.x, .y)
    )
  )

# successful sheets
successful_columns <- profile_results |>
  mutate(data = map(result, "result")) |>
  filter(map_lgl(data, ~ !is.null(.x))) |>
  select(data) |>
  unnest(data)

# failed sheets
failed_sheets <- profile_results |>
  mutate(error = map(result, "error")) |>
  filter(map_lgl(error, ~ !is.null(.x))) |>
  transmute(
    file_name,
    file_path,
    sheet_name,
    error_message = map_chr(error, conditionMessage)
  )

# stop if everything failed
if (nrow(successful_columns) == 0) {
  
  write_parquet(
    failed_sheets,
    failed_sheets_path
  )
  
  print(
    failed_sheets,
    n = Inf
  )
  
  stop("No sheets were successfully profiled.")
}

# combine old + new
sheet_inventory_final <- bind_rows(
  old_sheet_inventory,
  sheet_inventory
) |>
  distinct(file_path, sheet_name, .keep_all = TRUE)

column_inventory_final <- bind_rows(
  old_column_inventory,
  successful_columns
) |>
  distinct(file_path, sheet_name, raw_column, column_position, .keep_all = TRUE)

failed_sheets_final <- bind_rows(
  old_failed_sheets,
  failed_sheets
) |>
  distinct(file_path, sheet_name, error_message, .keep_all = TRUE)

# summarize distinct columns
distinct_columns <- column_inventory_final |>
  group_by(clean_column, raw_column) |>
  summarize(
    n_files = n_distinct(file_name),
    example_files = paste(head(unique(file_name), 5), collapse = " | "),
    .groups = "drop"
  ) |>
  arrange(clean_column, raw_column)


# save outputs
write_parquet(sheet_inventory_final, sheet_inventory_path)
write_parquet(column_inventory_final, column_inventory_path)
write_parquet(distinct_columns, distinct_columns_path)

if (nrow(failed_sheets_final) > 0) {
  write_parquet(failed_sheets_final, failed_sheets_path)
}

# warn if individual sheets failed 
if (nrow(failed_sheets_final) > 0) {
  
  write_parquet(
    failed_sheets_final,
    failed_sheets_path
  )
  
  warning(
    nrow(failed_sheets_final),
    " sheet(s) failed profiling. Review ",
    failed_sheets_path,
    " before treating the dataset as complete."
  )
}




