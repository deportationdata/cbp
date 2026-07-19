# load packages
library(tidyverse)
library(arrow)

#### WRITE CROSSWALK ####

# paths
metadata_dir <- "data/inadmissibles/metadata"

# outputs 
distinct_columns <- read_parquet(
  file.path(metadata_dir, "distinct_columns.parquet")
)

# build crosswalk
crosswalk <- distinct_columns |>
  mutate(
    canonical_name = case_when(
      
      # age
      clean_column %in% c(
        "age_at_event_creation_time"
      ) ~ "age",
      
      # event datetime
      clean_column %in% c(
        "calendar_month_mon_yy"
      ) ~ "calendar_month_name",
      
      clean_column %in% c(
        "calendar_year_number_yyyy"
      ) ~ "calendar_year",
      
      clean_column %in% c(
        "event_created_date_time"
      ) ~ "event_created_datetime",
      
      # birth country
      clean_column %in% c(
        "birth_country_name"
      ) ~ "birth_country",
      
      # duplicate indicator
      clean_column %in% c(
        "duplicate_indicator",
        "duplicate_subject_indicator",
        "duplicates"
      ) ~ "duplicate",
      
      # transportation
      
      clean_column %in% c(
        "mode_of_transport"
      ) ~ "mode_of_transportation",
      
      # default: keep cleaned name
      TRUE ~ clean_column
    ),
    
    category = NA_character_
  ) |>
  select(
    clean_column,
    raw_column,
    n_files,
    canonical_name
  ) |>
  arrange(clean_column, raw_column)

# save
write_parquet(
  crosswalk,
  file.path(metadata_dir, "crosswalk.parquet")
)

#### AUDITS ####

# audits
cat("distinct_columns rows:", nrow(distinct_columns), "\n")
cat("crosswalk rows:", nrow(crosswalk), "\n")
cat("canonical columns:", n_distinct(crosswalk$canonical_name), "\n")

# check nothing got lost
missing_from_crosswalk <- distinct_columns |>
  anti_join(
    crosswalk,
    by = c("clean_column", "raw_column")
  )

print(missing_from_crosswalk, n = Inf)

# review only collapsed groups
collapsed_groups <- crosswalk |>
  group_by(canonical_name) |>
  summarize(
    n_source_columns = n_distinct(clean_column),
    source_columns = paste(sort(unique(clean_column)), collapse = " | "),
    .groups = "drop"
  ) |>
  filter(n_source_columns > 1) |>
  arrange(desc(n_source_columns), canonical_name)

print(collapsed_groups, n = Inf)

