# load packages
library(tidyverse)
library(readxl)
library(janitor)
library(arrow)
library(fs)

# paths
download_dir <- "data/apprehensions"
metadata_dir <- file.path(download_dir, "metadata")
processed_dir <- file.path(download_dir, "processed")
parts_to_stack_dir <- file.path(processed_dir, "parts_to_stack")
column_inventory_path <- file.path(metadata_dir, "column_inventory.parquet")
crosswalk_path <- file.path(metadata_dir,"crosswalk.parquet")

#### REBUILD ALL PARTS? ####
  # FALSE = only build new parts
  # TRUE = rebuild everything from scratch
force_rebuild <- FALSE

dir.create(
  processed_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  parts_to_stack_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (force_rebuild) {
  
  old_parts <- dir_ls(
    parts_to_stack_dir,
    regexp = "\\.parquet$",
    fail = FALSE
  )
  
  if (length(old_parts) > 0) {
    file_delete(old_parts)
  }
}

# read metadata
column_inventory <- read_parquet(column_inventory_path)

crosswalk <- read_parquet(crosswalk_path)

expected_cols <- unique(crosswalk$canonical_name)

# check whether any canonical merge would collapse columns that appear together in the same file/sheet

collapse_conflicts <- column_inventory |>
  select(
    file_name,
    sheet_name,
    clean_column
  ) |>
  left_join(
    crosswalk |>
      select(clean_column, canonical_name),
    by = "clean_column"
  ) |>
  group_by(
    file_name,
    sheet_name,
    canonical_name
  ) |>
  summarize(
    source_columns = paste(
      sort(unique(clean_column)),
      collapse = " | "
    ),
    n_columns = n_distinct(clean_column),
    .groups = "drop"
  ) |>
  filter(n_columns > 1)

if (nrow(collapse_conflicts) > 0) {
  
  print(collapse_conflicts, n = Inf)
  
  stop(
    "Some canonical mappings would merge columns that coexist in the same sheet."
  )
}

# helper: drop fully empty columns
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

# read and standardize one sheet
read_standardized_sheet <- function(file_path, sheet_name) {
  
  sheet_cols <- column_inventory |>
    filter(
      file_path == !!file_path,
      sheet_name == !!sheet_name
    ) |>
    arrange(column_position)
  
  rows_to_skip <- unique(sheet_cols$rows_to_skip)
  
  if (length(rows_to_skip) != 1) {
    stop(
      "rows_to_skip issue for: ",
      file_path,
      " / ",
      sheet_name
    )
  }
  
  raw_df <- suppressMessages(
    read_excel(
      path = file_path,
      sheet = sheet_name,
      col_names = FALSE,
      col_types = "text",
      skip = rows_to_skip,
      .name_repair = "unique"
    )
  ) |>
    drop_empty_columns()
  
  header <- raw_df |>
    slice(1) |>
    unlist(use.names = FALSE) |>
    as.character() |>
    str_squish()
  
  keep_cols <- !is.na(header) & header != ""
  
  header <- header[keep_cols]
  
  data_df <- raw_df |>
    slice(-1) |>
    select(which(keep_cols))
  
  names(data_df) <- make_clean_names(
    header,
    allow_dupes = TRUE
  )
  
# remove repeated header rows

header_values <- names(data_df) |>
  str_to_lower() |>
  str_replace_all("[^a-z0-9]+", "_") |>
  str_replace_all("^_|_$", "")

row_header_matches <- map_dfc(
  seq_along(data_df),
  function(j) {
    data_df[[j]] |>
      as.character() |>
      str_squish() |>
      str_to_lower() |>
      str_replace_all("[^a-z0-9]+", "_") |>
      str_replace_all("^_|_$", "") == header_values[j]
  }
)

rows_to_remove <- rowSums(row_header_matches, na.rm = TRUE) >= 2

if (any(rows_to_remove)) {
  message(
    "Removing ",
    sum(rows_to_remove),
    " repeated header row(s) from ",
    path_file(file_path),
    " / ",
    sheet_name
  )
  
  data_df <- data_df |>
    filter(!rows_to_remove)
}
  
  rename_lookup <- crosswalk |>
    select(
      clean_column,
      canonical_name
    ) |>
    distinct()
  
  matched_names <- tibble(
    clean_column = names(data_df)
  ) |>
    left_join(
      rename_lookup,
      by = "clean_column"
    ) |>
    mutate(
      final_name = if_else(
        is.na(canonical_name),
        clean_column,
        canonical_name
      )
    )
  
  names(data_df) <- matched_names$final_name
  
  missing_cols <- setdiff(
    expected_cols,
    names(data_df)
  )
  
  if (length(missing_cols) > 0) {
    data_df[missing_cols] <- NA
  }
  
  data_df |>
    mutate(
      source_file = path_file(file_path),
      source_sheet = sheet_name
    ) |>
    select(
      source_file,
      source_sheet,
      all_of(expected_cols)
    ) |>
    mutate(
      across(
        everything(),
        as.character
      )
    )
}

# sheets to stack
sheets_to_stack <- column_inventory |>
  distinct(
    file_name,
    file_path,
    sheet_name
  )

# process one sheet at a time
for (i in seq_len(nrow(sheets_to_stack))) {
  
  file_path_i <- sheets_to_stack$file_path[i]
  sheet_name_i <- sheets_to_stack$sheet_name[i]
  file_name_i <- sheets_to_stack$file_name[i]
  
  part_path <- file.path(
    parts_to_stack_dir,
    paste0(
      str_pad(i, 4, pad = "0"),
      "_",
      make_clean_names(
        tools::file_path_sans_ext(file_name_i)
      ),
      "_",
      make_clean_names(sheet_name_i),
      ".parquet"
    )
  )
  
  if (!force_rebuild && file_exists(part_path)) {
    
    message(
      "Skipping existing part: ",
      path_file(part_path)
    )
    
    next
  }
  
  message(
    "Reading: ",
    file_name_i,
    " / ",
    sheet_name_i
  )
  
  one_sheet <- read_standardized_sheet(
    file_path = file_path_i,
    sheet_name = sheet_name_i
  )
  
  write_parquet(
    one_sheet,
    part_path
  )
  
  rm(one_sheet)
}


cat(
  "\nParts created:",
  length(dir_ls(parts_to_stack_dir, regexp = "\\.parquet$")),
  "\n"
)

cat(
  "Expected parts:",
  nrow(sheets_to_stack),
  "\n"
)

if (length(dir_ls(parts_to_stack_dir, regexp = "\\.parquet$")) !=
    nrow(sheets_to_stack)) {
  
  warning(
    "Number of part files does not match expected number of sheets."
  )
}


