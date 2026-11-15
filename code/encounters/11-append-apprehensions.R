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
  "arrest_sector",
  "arrest_method",
  "arrest_at_checkpoint_indicator",
  
  # encounter / entry history
  "earliest_encounter_date",
  "most_recent_encounter_date",
  "number_of_previous_encounters",
  "entry_date",
  "entry_status",
  "time_in_us",
  
  # demographics / family
  "age",
  "adult_or_juvenile",
  "gender",
  "citizenship",
  "residence_country",
  "residence_city",
  "marital_status",
  "subject_group_classification",
  "unaccompanied_child_indicator",
  "number_of_children_and_nationality",
  
  # screening / processing
  "credible_fear_indicator",
  "ces_indicator",
  "mpp_indicator",
  "cds_program",
  "transfer_to_group",
  
  # charges / prosecution
  "statute_charge",
  "prosecution_indicator",
  "referred_for_prosecution_under_8usc1325_or_8usc1326",
  "drugs_seized_indicator",
  
  # disposition / bookout
  "disposition",
  "removal_type",
  "final_bookout_datetime",
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
  select(latest_columns) |>
  arrange(encounter_datetime)

save_outputs(encounters_latest, file.path(
  "encounters/processed/usbp-encounters-latest"
))

# END