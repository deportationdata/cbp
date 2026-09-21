# packages
library(tidyverse)
library(arrow)

# paths

dataset_dir <- "data/apprehensions"
processed_dir <- file.path(dataset_dir, "processed")

apprehensions_final_all_cols_path <- file.path(
  processed_dir,
  "apprehensions-final-all-cols.parquet"
)

apprehensions_final_path <- file.path(
  processed_dir,
  "apprehensions-final.parquet"
)

# select final cols

final_columns <- c(
  # event timing
  "apprehension_datetime",
  "entry_date",
  "final_bookout_date",
  
  # arrest information
  "arrest_sector",
  "arrest_method",
  
  # demographic information
  "age",
  "gender",
  "citizenship",
  "state",
  "marital_status",
  
  # family / child information
  "number_of_children_and_nationality",
  "fmua_indication",
  "unaccompanied_child_indicator",
  
  # immigration / processing
  "entry_status",
  "credible_fear_indicator",
  "cds_program",
  "subject_prosecution_indicator",
  
  # charges / disposition
  "statute_charge",
  "charge_code",
  "disposition",
  "removal_type",
  
  # source information
  "source_file",
  "source_sheet"
)

apprehensions_final <- read_parquet(
  apprehensions_final_all_cols_path,
  col_select = all_of(final_columns),
  mmap = FALSE
)

write_parquet(
  apprehensions_final,
  apprehensions_final_path
)

# END