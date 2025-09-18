# ---- Packages ----
library(tidyverse)
library(tidylog)

# ---- Functions ----
# source("code/functions/check_dttm_and_convert_to_date.R")
# source("code/functions/is_not_blank_or_redacted.R")

# ---- Read in to temporary file ----
url <- "https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1967490095971"
f1 <- tempfile(fileext = ".xlsx")
download.file(url, f1, mode = "wb")

url <- "https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1905008519753"
f2 <- tempfile(fileext = ".xlsx")
download.file(url, f2, mode = "wb")

url <- "https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1947452927480"
f3 <- tempfile(fileext = ".xlsx")
download.file(url, f3, mode = "wb")

url <- "https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1981826641425"
f4 <- tempfile(fileext = ".xlsx")
download.file(url, f4, mode = "wb")

apprehensions_h1 <- readxl::read_excel(path = f1, sheet = 1, skip = 2)
apprehensions_h2 <- readxl::read_excel(path = f2, sheet = 1, skip = 3)
apprehensions_h3 <- readxl::read_excel(path = f3, sheet = 1, skip = 5)
apprehensions_h4 <- readxl::read_excel(path = f4, sheet = 1, skip = 5)

apprehensions <-
  bind_rows(
    "usbp_nationwide_apprehensions_fy2025_dec_1_2025_through_march_31_2025_raw.xlsx" = apprehensions_h1 |>
      mutate(across(
        c(`Entry Date`, `Case File Date`),
        ~ as.Date(.x, format = "%m/%d/%Y")
      )),
    "usbp_nationwide_apprehensions_april_2025_raw.xlsx" = apprehensions_h2 |>
      mutate(across(
        c(`Arrest Time`),
        hms::as_hms
      )),
    "usbp_nationwide_apprehensions_may_2025_raw" = apprehensions_h3 |>
      mutate(across(
        c(`Arrest Time`),
        hms::as_hms
      )),
    "usbp_nationwide_apprehensions_june_2025_raw" = apprehensions_h4 |>
      mutate(across(
        c(`Arrest Time`),
        hms::as_hms
      )),
    .id = "file"
  ) |>
  janitor::clean_names()

apprehensions |>
  arrow::write_feather("data/apprehensions.feather")

apprehensions |>
  writexl::write_xlsx("data/apprehensions.xlsx")
