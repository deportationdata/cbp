# use UTC for source clock times, source time zone is unspecified
Sys.setenv(TZ = "UTC")

# packages
library(tidyverse)
library(arrow)

source("code/functions/save_outputs.R")

# paths

dataset_dir <- "data/encounters"
processed_dir <- file.path(dataset_dir, "processed")

encounters_latest_path <- file.path(
  processed_dir,
  "usbp-encounters-latest.parquet"
)

encounters_fy2010_2025_path <- file.path(
  "data/encounters/processed",
  "usbp-encounters-fy2010-2025.parquet"
)

apprehensions_latest_path <- file.path(
  "data/apprehensions/processed",
  "usbp-apprehensions-latest.parquet"
)

encounters_latest <- read_parquet(
  encounters_fy2010_2025_path,
  mmap = FALSE
)

# encounter start and end dates
encounters_start <- min(
  as.Date(encounters_latest$encounter_datetime),
  na.rm = TRUE
)

encounters_end <- max(
  as.Date(encounters_latest$encounter_datetime),
  na.rm = TRUE
)

# select apprehension dates outside of encounter coverage
apprehensions_added <- read_parquet(
  apprehensions_latest_path,
  mmap = FALSE
) |>
  filter(
    as.Date(apprehension_datetime) < encounters_start |
      as.Date(apprehension_datetime) > encounters_end
  ) |>
  rename(
    encounter_datetime = apprehension_datetime
  )

encounters_latest <- encounters_latest |>
  mutate(record_type = "Encounter")

apprehensions_added <- apprehensions_added |>
  mutate(record_type = "Apprehension")

latest_columns <- c(
  # encounter / arrest info
  "encounter_datetime",
  "border",
  "sector",
  "time_in_us",
  
  # demographics / family
  "age",
  "adult_or_juvenile",
  "gender",
  "citizenship",
  "marital_status",
  "number_of_children_and_nationality",
  
  # charges / disposition / bookout
  "statute_charge",
  "disposition",
  "final_bookout_date",
  
  # source metadata
  "record_type",
  "source_file",
  "source_sheet"
  )

encounters_latest <- bind_rows(
  encounters_latest,
  apprehensions_added
) |>
  mutate(
    adult_or_juvenile = case_when(
      is.na(adult_or_juvenile) & !is.na(age) & age < 18 ~ "Juvenile",
      is.na(adult_or_juvenile) & !is.na(age) & age >= 18 ~ "Adult",
      TRUE ~ adult_or_juvenile
    )
  ) |>
  select(all_of(latest_columns)) |>
  arrange(encounter_datetime)

save_outputs(encounters_latest, file.path(
  "encounters/processed/usbp-encounters-latest"
))

# END