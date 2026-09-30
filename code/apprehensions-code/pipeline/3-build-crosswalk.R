# load packages
library(tidyverse)
library(arrow)

#### Write Crosswalk ####

# paths
dataset_dir <- "data/apprehensions"
metadata_dir <- file.path(dataset_dir, "metadata")
dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)

# outputs 
distinct_columns <- read_parquet(
  file.path(metadata_dir, "distinct-columns.parquet")
)

# build crosswalk
crosswalk <- distinct_columns |>
  mutate(
    canonical_name = case_when(
      # standardize NCIC names
      clean_column %in% c(
        "ncic_charge_code",
        "ncic_charge_code_s",
        "ncic_code",
        "ncic_charge_code_defer_to_doj",
        "ncic_charge_code_owned_by_doj_not_cbp",
        "ncic_charge_code_owned_by_doj"
      ) ~ "ncic_charge_code",
      
      clean_column %in% c(
        "ncic_description",
        "ncic_desc",
        "ncic_desc_s",
        "ncic_desc_defer_to_doj",
        "ncic_desc_owned_by_doj_not_cbp",
        "ncic_desc_owned_by_doj",
        "ncic_description_owned_by_doj"
      ) ~ "ncic_description",
      
      
      # standardize birth and residence names
      clean_column %in% c(
        "birth_city",
        "city_of_birth"
      ) ~ "birth_city",
      
      clean_column %in% c(
        "birth_state",
        "state_of_birth"
      ) ~ "birth_state",
      
      clean_column %in% c(
        "birth_country",
        "country_of_birth"
      ) ~ "birth_country",
      
      clean_column %in% c(
        "birth_country_cd",
        "country_of_birth_cd"
      ) ~ "birth_country_cd",
      
      clean_column %in% c(
        "residence_country",
        "country_of_residence"
      ) ~ "residence_country",
      
      clean_column %in% c(
        "residence_country_cd",
        "country_of_residence_cd",
        "country_of_res_cd"
      ) ~ "residence_country_cd",
      
      clean_column %in% c(
        "residence_city",
        "city_of_residence"
      ) ~ "residence_city",
      
      clean_column %in% c(
        "residence_city_state",
        "city_state_of_residence"
      ) ~ "residence_city_state",
      
      clean_column %in% c(
        "first_residence_country",
        "first_country_of_residence",
        "first_country_of_residence_foreign"
      ) ~ "first_residence_country",
      
      
      # age
      clean_column %in% c(
        "age",
        "app_age",
        "subject_age"
      ) ~ "age",
      
      clean_column %in% c(
        "juvenile_18"
      ) ~ "juvenile_18_indicator",
      
      # gender
      clean_column %in% c(
        "gender",
        "subject_gender"
      ) ~ "gender",
      
      # subject demographic
      clean_column %in% c(
        "demographic",
        "subject_demographic",
        "subject_demographic_sdi"
      ) ~ "subject_group_classification",
      
      # apprehension/arrest datetime
      clean_column %in% c(
        "app_dt_time",
        "appr_dt_time",
        "apprehension_datetime",
        "encounter_dt_time",
        "arrest_date_time",
        "arrest_datetime"
      ) ~ "apprehension_datetime",
      
      clean_column %in% c(
        "apprehension_date",
        "apprehension_dt"
      ) ~ "apprehension_date",
      
      # birth / residence location
      
      
      
      
      
      
      clean_column %in% c(
        "subject_ethnicity_cd"
      ) ~ "ethnicity_cd",
      
      # final bookout
      clean_column %in% c(
        "final_bookout",
        "final_bookout_date",
        "final_bookout_date_time",
        "final_bookout_datetime"
      ) ~ "final_bookout_date",
      
      # programs
      clean_column %in% c(
        "cds_program",
        "cds_programs",
        "cds_program_s",
        "cds_program_code"
      ) ~ "cds_program",
      
      clean_column %in% c(
        "mpp_indicator_y_n"
      ) ~ "mpp_indicator",
      
      clean_column %in% c(
        "spp_program_s"
      ) ~ "spp_program",
      
      clean_column %in% c(
        "ces_criminal_indicator",
        "ces_indicator"
      ) ~ "ces_indicator",
      
      # charge/statute
      clean_column %in% c(
        "charge_code_s",
        "charge_code_statue_charge",
        "charge_code_statue_code"
      ) ~ "charge_code",
      
      clean_column %in% c(
        "statue_charge",
        "statute_charge",
        "statute_charge_s",
        "highest_statute_charge",
        "highest_statue_charge"
      ) ~ "statute_charge",
      
      
      
      
      
      # arrest method
      clean_column %in% c(
        "arrest_method_desc"
      ) ~ "arrest_method",
      
      clean_column %in% c(
        "arrest_sl_checkpoint_indicator"
      ) ~ "arrest_at_checkpoint_indicator",
      
      clean_column %in% c(
        "arresting_agent_hash_number"
      ) ~ "agent_hash_id",
      
      # credible fear
      clean_column %in% c(
        "credible_fear",
        "credible_fear_ind",
        "credible_fear_indicator"
      ) ~ "credible_fear_indicator",
      
      # entry / lpr stuff
      clean_column %in% c(
        "entry_date",
        "entry_dt"
      ) ~ "entry_date",
      
      clean_column %in% c(
        "entry_status",
        "status_at_entry",
        "entry_status_includes_lpr",
        "status_at_entry_lpr_status"
      ) ~ "entry_status",
      
      clean_column %in% c(
        "status_at_entry_cd"
      ) ~ "entry_status_cd",
      
      clean_column %in% c(
        "subject_immigration_status_code_lpr"
      ) ~ "immigration_status_code_lpr",
      
      clean_column %in% c(
        "subject_disposition_code_lpr",
        "disposition_code_lpr"
      ) ~ "disposition_code",
      
      clean_column %in% c(
        "lpr"
      ) ~ "lpr_indicator",
      
      # family/minors
      clean_column %in% c(
        "family_unit_group_number",
        "fmu_group_number",
        "fmu_number_s"
      ) ~ "fmu_number",
      
      clean_column %in% c(
        "fmua_indicator",
        "fmua_ind"
      ) ~ "fmua_indication",
      
      clean_column %in% c(
        "family_unit_type",
        "family_unit_type_demographic"
      ) ~ "fmu_type",
      
      clean_column %in% c(
        "uc_indicator",
        "uc_indicator_y_n",
        "unaccompanied_child",
        "uac_ind"
      ) ~ "unaccompanied_child_indicator",
      
      clean_column %in% c(
        "number_children_and_nationality",
        "number_children_nationality"
      ) ~ "number_of_children_and_nationality",
      
      #  identifiers
      clean_column %in% c(
        "number_of_people_per_event",
        "number_of_subjects_per_event"
      ) ~ "number_of_people_in_event",
      
      clean_column %in% c(
        "civ_id",
        "civ_id_subject_key",
        "civ_id_subject_key_unique_identifier",
        "subject_key",
        "subject_id"
      ) ~ "subject_key",
      
      clean_column %in% c(
        "unique_person_identifier",
        "unique_person_id_we_dont_provide"
      ) ~ "unique_person_id",
      
      clean_column %in% c(
        "alien_number"
      ) ~ "subject_afile_number",
      
      # case info
      clean_column %in% c(
        "case_file_date",
        "case_filing_date",
        "case_filing_date_s"
      ) ~ "case_file_date",
      
      clean_column %in% c(
        "unique_case_id_we_dont_provide",
        "unique_case_identifier",
        "case_id_number"
      ) ~ "case_id",
      
      # sector/location
      clean_column %in% c(
        "arrest_sector",
        "sector",
        "sector_district"
      ) ~ "arrest_sector",
      
      clean_column %in% c(
        "sector_of_booked_out",
        "sector_of_bookout"
      ) ~ "bookout_sector",
      
      clean_column %in% c(
        "apprehension_latitude",
        "arrest_latitude",
        "latitude"
      ) ~ "latitude",
      
      clean_column %in% c(
        "border",
        "arrest_border"
      ) ~ "border",
      
      clean_column %in% c(
        "landmark",
        "landmark_name_for_latest_apprehension",
        "landmark_name_for_latest_apprehension_withheld_in_full_b_7_e"
      ) ~ "landmark_withheld_indicator",
      
      clean_column %in% c(
        "arrest_at_checkpoint_y_n"
      ) ~ "arrest_at_checkpoint_indicator",
      
      # marital status
      clean_column %in% c(
        "marital_status",
        "subject_marital_status"
      ) ~ "marital_status",
      
      # other dates (not apprehension)
      clean_column %in% c(
        "earliest_encounter_date_time",
        "earliest_app_date_time",
        "earliest_apprehension_date_time",
        "earliest_apprehension_datetime"
      ) ~ "earliest_encounter_datetime",
      
      clean_column %in% c(
        "earliest_app_date",
        "earliest_apprehension_date",
        "earliest_encounter_date"
      ) ~ "earliest_encounter_date",
      
      clean_column %in% c(
        "most_recent_app_date",
        "most_recent_apprehension_date",
        "most_recent_encounter_date"
      ) ~ "most_recent_encounter_date",
      
      clean_column %in% c(
        "most_recent_prior_ent_date_time",
        "most_recent_prior_entry_date_time"
      ) ~ "most_recent_prior_entry_datetime",
      
      clean_column %in% c(
        "most_recent_prior_encounter_date_time",
        "most_recent_encounter_date_time",
        "most_recent_app_date_time",
        "most_recent_apprehension_date_time",
        "most_recent_apprehension_datetime"
      ) ~ "most_recent_encounter_datetime",
      
      clean_column %in% c(
        "number_of_previous_apprehension",
        "number_of_previous_apprehensions",
        "number_of_previous_apps",
        "number_of_previous_apprehensions",
        "number_of_previous_encounter"
      ) ~ "number_of_previous_encounters",
      
      # criminal indication
      clean_column %in% c(
        "criminal_conviction"
      ) ~ "criminal_conviction_indicator",
      
      # gang info
      clean_column %in% c(
        "suspected_gang_member"
      ) ~ "suspected_gang_member_indicator",
      
      # prosecution
      clean_column %in% c(
        "prosecution_indicator",
        "subject_prosecution_indicator",
        "referred_prosecution"
      ) ~ "subject_prosecution_indicator",
      
      clean_column %in% c(
        "referred_for_prosecution_under_8_usc_1325_or_8_usc_1326"
      ) ~ "referred_for_prosecution_under_8usc1325_or_8usc1326",
      
      # transfer
      clean_column %in% c(
        "transfer_to_group",
        "transferred_to_group",
        "transferred_to_group_removal_type_ero",
        "transfer_to_ero",
        "ero_transfer"
      ) ~ "transfer_to_group",
      
      # currency/drugs
      clean_column %in% c(
        "curency_seized_during_app_y_n",
        "currency_seized_during_app_y_n",
        "currency_seiz_during_app"
      ) ~ "currency_seized_indicator",
      
      clean_column %in% c(
        "currency_seiz_during_app_value",
        "currency_seized_during_app_value"
      ) ~ "currency_seized_value",
      
      clean_column %in% c(
        "drugs_seized_during_app_y_n",
        "drugs_seized_during_apprehension_y_n"
      ) ~ "drugs_seized_indicator",
      
      clean_column %in% c(
        "type_of_drugs_seized_during_app",
        "type_of_drugs_seized_during_apprehension"
      ) ~ "type_of_drugs_seized",
      
      # smuggling
      clean_column %in% c(
        "smuggled_cost",
        "subject_smuggle_cost"
      ) ~ "smuggled_cost",
      
      # time in US
      clean_column %in% c(
        "time_in_us",
        "subject_time_in_us",
        "time_in_us_cd"
      ) ~ "time_in_us",
      
      # disposition 
      clean_column %in% c(
        "subject_disposition_code",
        "disposition"
      ) ~ "disposition",
      
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

# END
