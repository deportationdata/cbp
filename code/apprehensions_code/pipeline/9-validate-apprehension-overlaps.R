# packages
library(tidyverse)
library(arrow)
library(fs)


# paths
apprehensions_dir <- "data/apprehensions"

parts_dir <- file.path(
  apprehensions_dir,
  "processed",
  "parts_to_stack"
)

metadata_dir <- file.path(
  apprehensions_dir,
  "metadata"
)

validation_dir <- file.path(
  apprehensions_dir,
  "validation"
)

parts_metadata_path <- file.path(
  metadata_dir,
  "parts_metadata.parquet"
)

contained_pairs_path <- file.path(
  validation_dir,
  "apprehensions_contained_pairs.csv"
)

daily_comparison_path <- file.path(
  validation_dir,
  "apprehensions_overlap_daily_comparison.csv"
)

pair_summary_path <- file.path(
  validation_dir,
  "apprehensions_overlap_pair_summary.csv"
)

dir_create(validation_dir)


# part metadata 
parts_metadata <- read_parquet(
  parts_metadata_path
) |>
  mutate(
    part_path = file.path(
      parts_dir,
      part_file
    )
  ) |>
  filter(
    !is.na(min_date),
    !is.na(max_date),
    file_exists(part_path)
  )

#### ID Contained Pairs #### 

# find contained pairs (smaller files are contained in larger)
contained_pairs <- parts_metadata |>
  select(
    smaller_file = part_file,
    smaller_path = part_path,
    smaller_min_date = min_date,
    smaller_max_date = max_date
  ) |>
  cross_join(
    parts_metadata |>
      select(
        larger_file = part_file,
        larger_path = part_path,
        larger_min_date = min_date,
        larger_max_date = max_date
      )
  ) |>
  filter(
    smaller_file != larger_file,
    larger_min_date <= smaller_min_date,
    larger_max_date >= smaller_max_date,
    larger_min_date < smaller_min_date |
      larger_max_date > smaller_max_date |
      smaller_file < larger_file
  ) |>
  mutate(
    pair_id = row_number()
  ) |>
  select(
    pair_id,
    everything()
  )

write_csv(
  contained_pairs,
  contained_pairs_path
)

cat(
  "Contained pairs found:",
  nrow(contained_pairs),
  "\n"
)


#### Compare Daily Counts #### 
# find date cols
date_columns <- c(
  "apprehension_datetime",
  "apprehension_date",
  "arrest_datetime",
  "arrest_date",
  "entry_date"
)

# convert CBP date vals to date class
parse_apprehension_date <- function(x) {
  
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  
  out <- rep(
    as.Date(NA),
    length(x)
  )
  
  # excel serial number type
  numeric_values <- suppressWarnings(
    as.numeric(x)
  )
  
  numeric_rows <- !is.na(numeric_values)
  
  out[numeric_rows] <- as.Date(
    floor(numeric_values[numeric_rows]),
    origin = "1899-12-30"
  )
  
  # yyyy-mm-dd type
  remaining <- is.na(out) & !is.na(x)
  
  if (any(remaining)) {
    out[remaining] <- suppressWarnings(
      as.Date(
        x[remaining],
        format = "%Y-%m-%d"
      )
    )
  }
  
  # mm/dd/yyyy type
  remaining <- is.na(out) & !is.na(x)
  
  if (any(remaining)) {
    out[remaining] <- suppressWarnings(
      as.Date(
        x[remaining],
        format = "%m/%d/%Y"
      )
    )
  }
  
  # mm/dd/yy type
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

count_part_by_day <- function(part_file) {
  
  part_path <- file.path(
    parts_dir,
    part_file
  )
  
  if (!file_exists(part_path)) {
    warning(
      "Part does not exist and will be skipped: ",
      part_path
    )
    
    return(
      tibble(
        part_file = character(),
        event_date = as.Date(character()),
        daily_count = integer()
      )
    )
  }
  
  part_data <- read_parquet(
    part_path
  )
  
  available_date_columns <- intersect(
    date_columns,
    names(part_data)
  )
  
  if (length(available_date_columns) == 0) {
    warning(
      "No usable date columns found in: ",
      part_file
    )
    
    return(
      tibble(
        part_file = character(),
        event_date = as.Date(character()),
        daily_count = integer()
      )
    )
  }
  
  # use first nonmissing date 
  event_date <- part_data[
    available_date_columns
  ] |>
    map(parse_apprehension_date) |>
    reduce(coalesce)
  
  tibble(
    part_file = part_file,
    event_date = event_date
  ) |>
    filter(
      !is.na(event_date)
    ) |>
    count(
      part_file,
      event_date,
      name = "daily_count"
    )
}

# count relevant parts 
parts_to_compare <- union(
  contained_pairs$smaller_file,
  contained_pairs$larger_file
)

daily_counts <- parts_to_compare |>
  map_dfr(count_part_by_day)

# create one row per pair / date 
pair_dates <- contained_pairs |>
  rowwise() |>
  mutate(
    event_date = list(
      seq(
        smaller_min_date,
        smaller_max_date,
        by = "day"
      )
    )
  ) |>
  ungroup() |>
  unnest(event_date)

# attach daily counts 
daily_comparison <- pair_dates |>
  left_join(
    daily_counts |>
      rename(
        smaller_file = part_file,
        smaller_daily_count = daily_count
      ),
    by = c(
      "smaller_file",
      "event_date"
    )
  ) |>
  left_join(
    daily_counts |>
      rename(
        larger_file = part_file,
        larger_daily_count = daily_count
      ),
    by = c(
      "larger_file",
      "event_date"
    )
  ) |>
  mutate(
    smaller_daily_count = replace_na(
      smaller_daily_count,
      0L
    ),
    larger_daily_count = replace_na(
      larger_daily_count,
      0L
    ),
    count_difference =
      smaller_daily_count - larger_daily_count,
    daily_counts_match =
      smaller_daily_count == larger_daily_count
  )

write_csv(
  daily_comparison,
  daily_comparison_path
)

#### Summarize Each Pair ####

pair_summary <- daily_comparison |>
  group_by(
    pair_id,
    smaller_file,
    larger_file,
    smaller_min_date,
    smaller_max_date,
    larger_min_date,
    larger_max_date
  ) |>
  summarize(
    days_compared = n(),
    matching_days = sum(daily_counts_match),
    differing_days = sum(!daily_counts_match),
    percent_days_matching = mean(
      daily_counts_match
    ) * 100,
    smaller_records_in_range = sum(
      smaller_daily_count
    ),
    larger_records_in_range = sum(
      larger_daily_count
    ),
    total_count_difference =
      smaller_records_in_range -
      larger_records_in_range,
    maximum_absolute_daily_difference = max(
      abs(count_difference)
    ),
    all_daily_counts_match = all(
      daily_counts_match
    ),
    .groups = "drop"
  ) |>
  arrange(
    desc(all_daily_counts_match),
    desc(percent_days_matching),
    smaller_file
  )

write_csv(
  pair_summary,
  pair_summary_path
)

print(
  pair_summary,
  n = Inf
)

cat(
  "\nPair summary saved to:",
  pair_summary_path,
  "\nDaily comparison saved to:",
  daily_comparison_path,
  "\nContained pairs saved to:",
  contained_pairs_path,
  "\n"
)