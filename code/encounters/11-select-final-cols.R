# use UTC for source clock times, source time zone is unspecified
Sys.setenv(TZ = "UTC")

# packages
library(tidyverse)
library(arrow)

# paths

dataset_dir <- "data/encounters"
processed_dir <- file.path(dataset_dir, "processed")

encounters_final_all_cols_path <- file.path(
  processed_dir,
  "encounters-final-all-cols.parquet"
)

encounters_final_path <- file.path(
  processed_dir,
  "encounters-latest.parquet"
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

encounters_final <- read_parquet(
  encounters_final_all_cols_path,
  col_select = all_of(final_columns),
  mmap = FALSE
)

# label datetime columns as UTC before writing
encounters_final <- encounters_final |>
  mutate(
    across(
      where(is.POSIXct),
      ~ lubridate::with_tz(.x, "UTC")
    )
  )

write_parquet(
  encounters_final,
  encounters_final_path
)

# END
