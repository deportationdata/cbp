library(tidyverse)
library(pdftools)

pdf1 <- pdf_data("data/apprehensions/raw/fy14-1.pdf")

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
  "CDS PROGRAM(S)",
  "ARREST_METHOD",
  "DISPOSITION",
  "FMU_NUMBER",
  "CASE_FILING_DATE",
  "ERO_TRANSFER",
  "ERO_TRANSFER_DT",
  "SUBJECT_ID" # redacted
)

structured_page <- pdf1[[1]] |>
  filter(y > 91) |>
  group_by(y) |>
  arrange(x) |>
  summarize(line_text = paste(text, collapse = "|"), .groups = "drop")

head(structured_data)
