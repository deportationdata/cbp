# load packages
library(tidyverse)
library(arrow)

#### Write Crosswalk ####

# paths
metadata_dir <- "data/encounters/metadata"

# outputs 
distinct_columns <- read_parquet(
  file.path(metadata_dir, "distinct_columns.parquet")
)

# build crosswalk
crosswalk <- distinct_columns |>
  mutate(
    canonical_name = case_when(
      # apprehension/arrest datetime
      clean_column %in% c(
        "app_dt_time",
        "appr_dt_time"
      ) ~ "apprehension_datetime",
      
      clean_column %in% c(
        "encounter_dt_time"
      ) ~ "encounter_datetime",
      
      clean_column %in% c(
        "arrest_at_checkpoint_y_n"
      ) ~ "arrest_at_checkpoint_indicator",
      
      # programs
      clean_column %in% c(
        "refferred_for_prosecution_under_8_usc_1325_or_8_usc_1326"
      ) ~ "referred_for_prosecution_under_8usc1325_or_8usc1326",
      
      clean_column %in% c(
        "mpp_indicator_y_n"
      ) ~ "mpp_indicator",
      
      clean_column %in% c(
        "ces_y_n"
      ) ~ "ces_indicator",
      
      # transfer
      clean_column %in% c(
        "transfer_to_group",
        "transferred_to_group"
      ) ~ "transfer_to_group",
      
      # charge/statute
      clean_column %in% c(
        "statue_charge",
        "statute_charge_s"
      ) ~ "statute_charges",
      
      # lpr 
      clean_column %in% c(
        "lpr"
      ) ~ "lpr_indicator",
      
      clean_column %in% c(
        "entry_status_includes_lpr"
      ) ~ "entry_status_includes_lpr_indicator",
      
      # location 
      clean_column %in% c(
        "latitude"
      ) ~ "apprehension_latitude",
      
      # demographic 
      clean_column %in% c(
        "demographic"
      ) ~ "subject_group_classification",
      
      # landmark withheld 
      clean_column %in% c(
        "landmark_withheld"
      ) ~ "landmark_withheld_indicator",
      
      # family/minors
      clean_column %in% c(
        "number_children_and_nationality"
      ) ~ "number_children_and_nationality",
      
      clean_column %in% c(
        "unaccompanied_child"
      ) ~ "unaccompanied_child_indicator",
      
      clean_column %in% c(
        "juvenile_18"
      ) ~ "juvenile_18_indicator",
      
      clean_column %in% c(
        "uc_indicator_y_n"
      ) ~ "uc_indicator",
      
      # identifiers
      clean_column %in% c(
        "sector_of_booked_out"
      ) ~ "sector_of_bookout",
      
      # credible fear
      clean_column %in% c(
        "credible_fear"
      ) ~ "credible_fear_indicator",
      
      # previous / earliest / most recent dates
      clean_column %in% c(
        "earliest_app_date"
      ) ~ "earliest_apprehension_date",
      
      clean_column %in% c(
        "earliest_app_date_time"
      ) ~ "earliest_apprehension_datetime",
      
      clean_column %in% c(
        "number_of_previous_apprehension",
        "number_of_previous_apps"
      ) ~ "number_of_previous_apprehensions",
      
      clean_column %in% c(
        "number_of_previous_encounter"
      ) ~ "number_of_previous_encounters",
      
      clean_column %in% c(
        "most_recent_app_date"
      ) ~ "most_recent_apprehension_date",
      
      # NCIC
      clean_column %in% c(
        "ncic_charge_code_s"
      ) ~ "ncic_charge_code",
      
      # criminal conviction
      clean_column %in% c(
        "criminal_conviction"
      ) ~ "criminal_conviction_indicator",
      
      # drugs seized
      clean_column %in% c(
        "drugs_seized_during_apprehension_y_n",
        "drugs_seized_during_apprehension"
      ) ~ "drugs_seized_during_apprehension_indicator",
      
      # suspected gang member
      clean_column %in% c(
        "suspected_gang_member"
      ) ~ "suspected_gang_member_indicator",
      
      # currency/drugs
      clean_column %in% c(
        "currency_seiz_during_app",
        "currency_seiz_during_app_value"
      ) ~ "currency_seized_during_app_value",
      
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

#### Audits ####

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

