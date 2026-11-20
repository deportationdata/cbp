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
  "entry_date",
  
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
  mutate(
    days_in_us = as.integer(
      as.Date(encounter_datetime, tz = "UTC") - as.Date(entry_date)
    ),
    time_in_us = case_when(
      !is.na(time_in_us) & trimws(time_in_us) != "" ~ time_in_us,
      days_in_us == 0 ~ "At Entry",
      between(days_in_us, 1L, 3L) ~ "Within 72 hours",
      between(days_in_us, 4L, 30L) ~ "4 to 30 days",
      between(days_in_us, 31L, 365L) ~ "1 month to 1 year",
      days_in_us > 365L ~ "Over 1 year",
      TRUE ~ time_in_us
    )
  ) |>
  mutate(
    border = case_when(
      sector %in% c(
        "Miami Sector", "New Orleans Sector", "Ramey Sector"
      ) ~ "Coastal Border",
      sector %in% c(
        "Blaine Sector", "Buffalo Sector", "Detroit Sector",
        "Grand Forks Sector", "Havre Sector", "Houlton Sector",
        "Spokane Sector", "Swanton Sector"
      ) ~ "Northern Border",
      sector %in% c(
        "Big Bend Sector", "Del Rio Sector", "El Centro Sector",
        "El Paso Sector", "Laredo Sector", "Rio Grande Valley Sector",
        "San Diego Sector", "Tucson Sector", "Yuma Sector"
      ) ~ "Southwest Border",
      TRUE ~ border
    )
  ) |>
  select(all_of(setdiff(latest_columns, "entry_date"))) |>
  arrange(encounter_datetime)

save_outputs(encounters_latest, file.path(
  "encounters/processed/usbp-encounters-latest"
))

# END