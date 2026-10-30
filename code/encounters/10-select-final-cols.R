# use UTC for source clock times, source time zone is unspecified
Sys.setenv(TZ = "UTC")

# packages
library(tidyverse)
library(arrow)

# source("code/functions/write_xlsx_by_fy.R")
source("code/functions/save_outputs.R")

# paths

dataset_dir <- "data/encounters"
processed_dir <- file.path(dataset_dir, "processed")

encounters_latest_all_cols_path <- file.path(
  processed_dir,
  "usbp-encounters-final-all-cols.parquet"
)

encounters_latest_path <- file.path(
  processed_dir,
  "usbp-encounters-latest.parquet"
)

apprehensions_final_path <- file.path(
  "data/apprehensions/processed",
  "usbp-apprehensions-latest.parquet"
)

# select final cols

final_columns <- c(
  # event timing
  "encounter_datetime",
  "most_recent_encounter_date",
  "earliest_encounter_date",
  "number_of_previous_encounters",
  "final_bookout_datetime",
  
  # arrest information
  "border",
  "arrest_sector",
  "arrest_state",
  "arrest_at_checkpoint_indicator",
  
  
  # demographic information
  "age",
  "adult_or_juvenile",
  "gender",
  "citizenship",
  "residence_city",
  "residence_country",
  "marital_status",
  "subject_group_classification",
  
  # family / child information
  "number_of_children_and_nationality",
  
  # immigration / processing
  "time_in_us",
  "ces_indicator",
  "mpp_indicator",
  "transfer_to_group",
  "drugs_seized_indicator",
  
  # charges / disposition
  "statute_charge",
  "disposition",
  "referred_for_prosecution_under_8usc1325_or_8usc1326",
  
  # source information
  "source_file",
  "source_sheet"
)

encounters_latest <- read_parquet(
  encounters_latest_all_cols_path,
  col_select = all_of(final_columns),
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
  apprehensions_final_path,
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

encounters_latest <- bind_rows(
  encounters_latest,
  apprehensions_added
)

write_parquet(
  encounters_latest,
  encounters_latest_path
)

save_outputs(encounters_latest, file.path(
  "encounters/processed/usbp-encounters-latest"
))

# write_xlsx_by_fy(encounters_latest,
                 # "data/encounters/processed/usbp-encounters-latest.xlsx",
                 # date_col = encounter_datetime,
                 # label = "USBP Encounters")



# END
