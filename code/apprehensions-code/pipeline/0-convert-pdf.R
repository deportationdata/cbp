# Setup
library(tidyverse)
library(pdftools)
library(pointblank)

files <- list.files(
    here::here("data/apprehensions/raw/"),
    pattern = "*.pdf",
    full.names = TRUE
  )

# Read files in pdf_data format (x,y coordinates per word token)
pdf_data_list <- lapply(files, function(file) {
  pdftools::pdf_data(file)
})

# Identify files by name
names(pdf_data_list) <- basename(files)

# ...and page no. within each file. All files contain 500 pp (except last, 518 pp)
pdf_df <- imap_dfr(pdf_data_list, ~ bind_rows(.x, .id = "page") %>%
                     mutate(page = as.numeric(page),
                            file = .y))

# Column names
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

# Column max x coordinate values for sorting
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

# File 1 page 1 has additional header info
file_1_page_1_header_height <- 91
other_pages_header_height <- 63

# Flag header and footer rows based on y values
pdf_df_cols <- pdf_df |>
  group_by(page) |>
  mutate(
    header = case_when(
      file == "fy14-1.pdf" & page == 1 & y <= file_1_page_1_header_height ~ TRUE,
      y <= other_pages_header_height ~ TRUE,
      TRUE ~ FALSE
    ),
    footer = case_when(
      y == max(y) ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  ungroup()

# To check whether contains non-header data
header_and_footer_cols <- pdf_df_cols |>
  filter(header == TRUE | footer == TRUE)

header_and_footer_text <- as_tibble(header_and_footer_cols$text)

header_footer_expected_text <- c(
  "U.S.",
  "Border",
  "Patrol",
  "Nationwide",
  "Apprehensions",
  "FY2014",
  "Data",
  "includes",
  "Deportable",
  "Aliens",
  "Only",
  "Source:",
  "(b)(7)(E",
  ")",
  "(Unofficial)",
  "as",
  "of",
  "End",
  "Year",
  "Date;",
  "Prosecution",
  "8/16/19",
  "CITIZENSHIP",
  "GENDER",
  "APP_AGE",
  "UAC_IND",
  "FMUA_IND",
  "NUMBER_CHILDREN_",
  "CREDIBLE_FEAR_IND",
  "AND_NATIONALITY",
  "MARITAL_STATUS",
  "ENTRY_DT",
  "STATUS_AT_ENTRY",
  "DHS_STATUS_CD",
  "Page",
  "APP_DT_TIME",
  "SECTOR",
  "CDS",
  "PROGRAM(S)",
  "ARREST_METHOD",
  "DISPOSITION",
  "FMU",
  "NUMBER",
  "CASE",
  "FILING",
  "ERO",
  "DATE",
  "TRANSFER",
  "DT",
  "SUBJECT_ID"
)

page_nos <- as.character(seq(1:5018))

header_footer_expected_values <- c(header_footer_expected_text, page_nos)

header_and_footer_text |>
  col_vals_in_set(
    value,
    header_footer_expected_values,
    actions = action_levels(warn_at = 0.0001, stop_at = 0.001)
  )

# Assign columns based on x boundary values
pdf_df_cols <- pdf_df_cols |>
  filter(header == FALSE,
         footer == FALSE) |>
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

# Tolerance for slight differences in y heights between lines
y_tolerance <- 3

# Split columns into rows and columns
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
  
# Clean some messy values created by overlapping x coordinates
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
      # Checked for file 1, but could conceivably be "Patrol Interior"
      CDS_PROGRAMS == "OASISS, STR_PROSPatrol" ~ "Patrol Border", 
      CDS_PROGRAMS == "OASISS, STR_PROSTraffic" ~ "Traffic Check",
      # Inferred value for these fields, better to leave truncated?
      ARREST_METHOD == "Law Enforcement Agency Response Uni" ~ "Law Enforcement Agency Response Unit",
      ARREST_METHOD == "Organized Crime Drug Enforcement Tas" ~ "Organized Crime Drug Enforcement Task Force",
      TRUE ~ ARREST_METHOD
    ),
    CDS_PROGRAMS = case_when(
      CDS_PROGRAMS == "OASISS, STR_PROSPatrol" ~ "OASISS, STR_PROS",
      CDS_PROGRAMS == "OASISS, STR_PROSTraffic" ~ "OASISS, STR_PROS",
      TRUE ~ CDS_PROGRAMS
    ),
    DHS_STATUS_CD = case_when(
      STATUS_AT_ENTRY == "False Claim with Valid DocumNO" ~ "NO",
      STATUS_AT_ENTRY == "ORAL FALSE CLAIMS TO ONO" ~ "NO",
      TRUE ~ DHS_STATUS_CD
    ),
    STATUS_AT_ENTRY = case_when(
      STATUS_AT_ENTRY == "False Claim with Valid DocumNO" ~ "False Claim with Valid Docum...",
      STATUS_AT_ENTRY == "ORAL FALSE CLAIMS TO ONO" ~ "ORAL FALSE CLAIMS TO O...",
      TRUE ~ STATUS_AT_ENTRY
    ),
    # Convert to desired data types
    ENTRY_DT = mdy(ENTRY_DT),
    APP_DT_TIME = mdy(APP_DT_TIME),
    CASE_FILING_DATE = mdy(CASE_FILING_DATE), # Some cells contain list of multiple dates which are truncated and unparseable
    APP_AGE = as.numeric(APP_AGE)
  )

# ---- Pointblank Validation ----

# Expect a total of 5018 pages read in (9 * 500 + 518)
pages_read <- structured_table_clean |>
  group_by(file) |>
  summarize(pages = max(page))

stop_if_not(sum(pages_read$pages) == 5018)

structured_table_clean |>
  # -- Date range checks --
  col_vals_between(
    APP_DT_TIME,
    as.Date("2013-10-01"),
    as.Date("2014-09-30"),
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
  ) |>
  col_vals_in_set(
    UAC_IND,
    c("NO", "YES"),
    actions = action_levels(warn_at = 0.0001, stop_at = 0.001)
  ) |>
  col_vals_in_set(
    STATUS_AT_ENTRY,
    c(
      "PWA Mexico",
      "Parolee",
      "Immigrant",
      "Border Crossing Card",
      "Visitor",
      "Non-Immigrant",
      "PWA Other",
      "Legal Permanent Resident",
      "Temporary Worker Other",
      "US Citizen",
      "PWA Canada",
      "Other",
      "Imposter",
      "Temporary Work Agriculture",
      "Student",
      "False Claim with Valid Docum...",
      "False Claim with Counterfeit",
      "ORAL FALSE CLAIMS TO O...",
      "Crew",
      "Asylum",
      "Refugee",
      "Smuggler",
      "Stowaway",
      "Conditional Resident",
      "Not Applicable",
      "Not in Custody",
      "Temporary Resident",
      NA
    ),
    actions = action_levels(warn_at = 0.0001, stop_at = 0.001)
  ) |>
  col_vals_in_set(
    DHS_STATUS_CD,
    c("NO", "LPR"),
    actions = action_levels(warn_at = 0.0001, stop_at = 0.001)
  ) |>
  col_vals_in_set(
    SECTOR,
    c(
      "BBT",
      "BLW",
      "BUN",
      "DRT",
      "DTM",
      "ELC",
      "EPT",
      "GFN",
      "HLT",
      "HVM",
      "LRT",
      "MIP",
      "NLL",
      "RGV",
      "RMY",
      "SDC",
      "SPW",
      "SWB",
      "TCA",
      "YUM"
    ),
    actions = action_levels(warn_at = 0.0001, stop_at = 0.001)
  ) |>
  col_vals_in_set(
    CDS_PROGRAMS,
    c(
      "ATEP",
      "ATEP, OASISS",
      "ATEP, OASISS, STR",
      "ATEP, STR_PROS",
      "ATEP, STRMLINE",
      "OASISS",
      "OASISS, STR_PROS",
      "OASISS, STRMLINE",
      "STR_PROS",
      "STR_PROS, STRML",
      "STRMLINE",
      NA
    ),
    actions = action_levels(warn_at = 0.0001, stop_at = 0.001)
  ) |>
  col_vals_in_set(
    ARREST_METHOD,
    c(
      "Anti-Smuggling",
      "Boat Patrol",
      "CAP Federal Incarceration",
      "CAP Local Incarceration",
      "CAP State Incarceration",
      "Crewman/Stowaway",
      "Inspections",
      "Law Enforcement Agency Response Unit",
      "Located",
      "Organized Crime Drug Enforcement Task Force",
      "Other Agency (turned over to INS)",
      "Other efforts",
      "Other Task Force",
      "Patrol Border",
      "Patrol Interior",
      "Traffic Check",
      "Transportation Check Aircraft",
      "Transportation Check Bus",
      "Transportation Check Freight Train",
      "Transportation Check Passenger Train",
      "Worksite Enforcement"
    ),
    actions = action_levels(warn_at = 0.0001, stop_at = 0.001)
  ) |>
  col_vals_in_set(
    DISPOSITION,
    c(
      "B",
      "ER",
      "ER/CF",
      "I",
      "NTA",
      "P",
      "REINST",
      "REL",
      "T",
      "TOT",
      "V",
      "VWPPRM",
      "WA/NTA"
    ),
    actions = action_levels(warn_at = 0.0001, stop_at = 0.001)
  )

# Write out--what format? Parquet, Excel, 1 Excel per original file, 1 Excel per month?

# END.
