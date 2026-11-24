# use UTC for source clock times, source time zone is unspecified
Sys.setenv(TZ = "UTC")

# packages
library(tidyverse)
library(arrow)

# source("code/functions/write_xlsx_by_fy.R")

# paths

dataset_dir <- "data/apprehensions"
processed_dir <- file.path(dataset_dir, "processed")
intermediate_dir <- file.path(processed_dir, "intermediate")

apprehensions_all_cols_path <- file.path(
  intermediate_dir,
  "usbp-apprehensions-final-all-cols.parquet"
)

apprehensions_latest_path <- file.path(
  processed_dir,
  "usbp-apprehensions-latest.parquet"
)

# select final cols

final_columns <- c(
  # event timing
  "apprehension_datetime",
  "entry_date",
  "final_bookout_date",
  
  # arrest information
  "sector",
  "arrest_method",
  "arrest_at_checkpoint_indicator",

  # encounter history
  "number_of_previous_encounters",
  
  # demographic information
  "age",
  "gender",
  "citizenship",
  "marital_status",
  
  # family / child information
  "adult_or_juvenile",
  "subject_group_classification",
  "number_of_children_and_nationality",
  "fmua_indication",
  "unaccompanied_child_indicator",
  
  # immigration / processing
  "entry_status",
  "credible_fear_indicator",
  "cds_program",
  "prosecution_indicator",
  
  # charges / disposition
  "statute_charge",
  "charge_code",
  "disposition",
  "transfer_to_group",
  "removal_type",
  
  # source information
  "source_file",
  "source_sheet"
)

apprehensions_selected <- read_parquet(
  apprehensions_all_cols_path,
  col_select = all_of(final_columns),
  mmap = FALSE
)

write_parquet(
  apprehensions_selected,
  apprehensions_latest_path
)

# write_xlsx_by_fy(apprehensions_selected,
                 # "data/apprehensions/processed/usbp-apprehensions-latest.xlsx",
                 # date_col = apprehension_datetime,
                 # label = "USBP Apprehensions")

# END
