library(tidyverse)
library(pdftools)
library(pointblank)

files <- list.files(
    here::here("data/apprehensions/raw/"),
    pattern = "*.pdf",
    full.names = TRUE
  )

pdf_data_list <- lapply(files, function(file) {
  pdftools::pdf_data(file)
})

names(pdf_data_list) <- basename(files)

pdf_df <- imap_dfr(pdf_data_list, ~ bind_rows(.x, .id = "page") %>%
                     mutate(page = as.numeric(page),
                            file = .y))

cols <- c(
  "CITIZENSHIP",
  "GENDER",
  "APP_AGE",
  "UAC_IND",
  "FMUA_IND", # redacted
  "CREDIBLE_FEAR_IND",
  "NUMBER_CHILDREN_AND_NATIONALITY", # redacted
  "MARITAL_STATUS",
  "ENTRY_DT",
  "STATUS_AT_ENTRY",
  "DHS_STATUS_CD",
  "APP_DT_TIME",
  "SECTOR",
  "CDS PROGRAMS",
  "ARREST_METHOD",
  "DISPOSITION",
  "FMU_NUMBER", # redacted
  "CASE_FILING_DATE",
  "ERO_TRANSFER",
  "ERO_TRANSFER_DT",
  "SUBJECT_ID" # redacted
)

CITIZENSHIP_max <- 77
GENDER_max <- 111
APP_AGE_max <- 117
UAC_IND_max <- 136
FMUA_IND_max <- 162
CREDIBLE_FEAR_IND_max <- 200
NUMBER_CHILDREN_AND_NATIONALITY_max <- 262
MARITAL_STATUS_max <- 300
ENTRY_DT_max <- 333
STATUS_AT_ENTRY_max <- 382
DHS_STATUS_CD_max <- 420
APP_DT_TIME_max <- 454
SECTOR_max <- 473
CDS_PROGRAMS_max <- 511
ARREST_METHOD_max <- 581
DISPOSITION_max <- 610
FMU_NUMBER_max <- 645
CASE_FILING_DATE_max <- 674
ERO_TRANSFER_max <- 707
ERO_TRANSFER_DT_max <- 739
SUBJECT_ID_max <- 800

page_1_header_height <- 91
other_pages_header_height <- 63

pdf_df_cols <- pdf_df |>
  group_by(page) |>
  mutate(
    header = case_when(
      page == 1 & y <= page_1_header_height ~ TRUE,
      page > 1 & y <= other_pages_header_height ~ TRUE,
      TRUE ~ FALSE
    ),
    footer = case_when(
      y == max(y) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  ungroup() |>
  filter(header == FALSE,     # Currently we lose these, figure out a better way if we need to keep.
         footer == FALSE) |>  # Or use here to validate page no, make sure no unexpected vals in header/footer
  mutate(
    column_name = case_when(
      x <= CITIZENSHIP_max ~ "CITIZENSHIP",
      x >= CITIZENSHIP_max & x <= GENDER_max ~ "GENDER",
      x >= GENDER_max & x <= APP_AGE_max ~ "APP_AGE",
      x >= APP_AGE_max & x <= UAC_IND_max ~ "UAC_IND",
      x >= UAC_IND_max & x <= FMUA_IND_max ~ "FMUA_IND",
      x >= FMUA_IND_max & x <= CREDIBLE_FEAR_IND_max ~ "CREDIBLE_FEAR_IND",
      x >= CREDIBLE_FEAR_IND_max & x <= NUMBER_CHILDREN_AND_NATIONALITY_max ~ "NUMBER_CHILDREN_AND_NATIONALITY",
      x >= NUMBER_CHILDREN_AND_NATIONALITY_max & x <= MARITAL_STATUS_max ~ "MARITAL_STATUS",
      x >= MARITAL_STATUS_max & x <= ENTRY_DT_max ~ "ENTRY_DT",
      x >= ENTRY_DT_max & x <= STATUS_AT_ENTRY_max ~ "STATUS_AT_ENTRY",
      x >= STATUS_AT_ENTRY_max & x <= DHS_STATUS_CD_max ~ "DHS_STATUS_CD",
      x >= DHS_STATUS_CD_max & x <= APP_DT_TIME_max ~ "APP_DT_TIME",
      x >= APP_DT_TIME_max & x <= SECTOR_max ~ "SECTOR",
      x >= SECTOR_max & x <= CDS_PROGRAMS_max ~ "CDS_PROGRAMS",
      x >= CDS_PROGRAMS_max & x <= ARREST_METHOD_max ~ "ARREST_METHOD",
      x >= ARREST_METHOD_max & x <= DISPOSITION_max ~ "DISPOSITION",
      x >= DISPOSITION_max & x <= FMU_NUMBER_max ~ "FMU_NUMBER",
      x >= FMU_NUMBER_max & x <= CASE_FILING_DATE_max ~ "CASE_FILING_DATE",
      x >= CASE_FILING_DATE_max & x <= ERO_TRANSFER_max ~ "ERO_TRANSFER",
      x >= ERO_TRANSFER_max & x <= ERO_TRANSFER_DT_max ~ "ERO_TRANSFER_DT",
      x >= ERO_TRANSFER_DT_max & x <= SUBJECT_ID_max ~ "SUBJECT_ID",
      .default = NA_character_)
    )

y_tolerance <- 3

structured_table <- pdf_df_cols %>%
  arrange(file, page, y, x) |>
  group_by(page) |>
  mutate(
    new_line = y - lag(y, default = first(y)) > y_tolerance,
    line_id = cumsum(new_line)
  ) %>%
  group_by(file, page, line_id, column_name) |>
  summarize(cell_text = paste(text, collapse = " "), .groups = "drop") |>
  pivot_wider(
    names_from = column_name, 
    values_from = cell_text,
    values_fill = NA
  ) |>
  relocate(file, page, line_id, any_of(cols))
  
structured_table_clean <- structured_table |>
  mutate(
    GENDER = case_when(
      CITIZENSHIP == "CHINA, PEOPLES REPUBLICMale" ~ "Male",
      CITIZENSHIP == "CHINA, PEOPLES REPUBLICFemale" ~ "Female",
      CITIZENSHIP == "ST. VINCENT-GRENADINESMale" ~ "Male",
      CITIZENSHIP == "ST. VINCENT-GRENADINESFemale" ~ "Female",
      TRUE ~ GENDER
    ),
    CITIZENSHIP = case_when(
      CITIZENSHIP %in% list("CHINA, PEOPLES REPUBLICMale",
                         "CHINA, PEOPLES REPUBLICFemale") ~
        "CHINA, PEOPLES REPUBLIC",
      CITIZENSHIP %in% list("ST. VINCENT-GRENADINESMale",
                         "ST. VINCENT-GRENADINESFemale") ~
        "ST. VINCENT-GRENADINES",
      TRUE ~ CITIZENSHIP
    ),
    ARREST_METHOD = case_when(
      CDS_PROGRAMS == "OASISS, STR_PROSPatrol" ~ "Patrol Border", # Checked for file 1, but could be "Patrol Interior"
      CDS_PROGRAMS == "OASISS, STR_PROSTraffic" ~ "Traffic Chec",
      ARREST_METHOD == "Law Enforcement Agency Response Uni" ~ "Law Enforcement Agency Response Unit",
      ARREST_METHOD == "Organized Crime Drug Enforcement Tas" ~ "Organized Crime Drug Enforcement Task Force",
      TRUE ~ ARREST_METHOD
    ),
    CDS_PROGRAMS = case_when(
      CDS_PROGRAMS == "OASISS, STR_PROSPatrol" ~ "OASISS, STR_PROS",
      CDS_PROGRAMS == "OASISS, STR_PROSTraffic" ~ "OASISS, STR_PROS",
      TRUE ~ CDS_PROGRAMS
    ),
    ENTRY_DT = mdy(ENTRY_DT),
    APP_DT_TIME = mdy(APP_DT_TIME),
    CASE_FILING_DATE = mdy(CASE_FILING_DATE),
    APP_AGE = as.numeric(APP_AGE)
  )

# unique(structured_table_clean$CITIZENSHIP)
# unique(structured_table_clean$GENDER)
# unique(structured_table_clean$APP_AGE)
# unique(structured_table_clean$UAC_IND)
# unique(structured_table_clean$FMUA_IND)
# unique(structured_table_clean$CREDIBLE_FEAR_IND)
# unique(structured_table_clean$NUMBER_CHILDREN_AND_NATIONALITY)
# unique(structured_table_clean$MARITAL_STATUS)
# unique(structured_table_clean$ENTRY_DT)
# unique(structured_table_clean$STATUS_AT_ENTRY)
# unique(structured_table_clean$DHS_STATUS_CD)
# unique(structured_table_clean$APP_DT_TIME)
# unique(structured_table_clean$SECTOR)
# unique(structured_table_clean$CDS_PROGRAMS)
# unique(structured_table_clean$ARREST_METHOD)
# unique(structured_table_clean$DISPOSITION)
# unique(structured_table_clean$FMU_NUMBER)
# unique(structured_table_clean$CASE_FILING_DATE) # Includes multiple dates, additional dates unparseable
# unique(structured_table_clean$ERO_TRANSFER)
# unique(structured_table_clean$ERO_TRANSFER_DT) # Both literal "N/A" and missingness
# unique(structured_table_clean$SUBJECT_ID)

# ---- Pointblank Validation ----

structured_table_clean |>
  # -- Date range checks --
  col_vals_between(
    APP_DT_TIME,
    as.Date("2013-09-01"),
    Sys.Date(),
    na_pass = TRUE,
    actions = action_levels(warn_at = 0.001, stop_at = 0.01)
  ) |>
  # -- Age range --
  col_vals_between(
    APP_AGE,
    0,
    99,
    na_pass = TRUE,
    actions = action_levels(warn_at = 0.001, stop_at = 0.01)
  ) |>
  # -- Categorical value checks --
  col_vals_in_set(
    GENDER,
    c("Male", "Female", "Unknown", NA),
    actions = action_levels(warn_at = 0.0001, stop_at = 0.001)
  )

# Write out--what format?

# END.
