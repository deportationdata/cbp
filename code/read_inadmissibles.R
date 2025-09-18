# ---- Packages ----
library(tidyverse)
library(tidylog)

# ---- Functions ----
# source("code/functions/check_dttm_and_convert_to_date.R")
# source("code/functions/is_not_blank_or_redacted.R")

process_inadmissibles <- function(df) {
  df |>
    janitor::clean_names() |>
    group_by(across(-c(charge))) |> # Group by all except these two
    mutate(identifier = cur_group_id()) |> # Create group identifier
    # mutate(dupe = n() > 1, n_dupes = n()) |> # Create duplicate indicator
    ungroup() |>
    group_by(identifier) |>
    summarize(
      across(c(everything(), -charge), first), # Keep first value of other columns
      charge_list = list(charge),
      .groups = "drop"
    )
}

# ---- Read in to temporary file ----
url <- "https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1842173428525"
f1 <- tempfile(fileext = ".xlsx")
download.file(url, f1, mode = "wb")

url <- "https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1842164368478"
f2 <- tempfile(fileext = ".xlsx")
download.file(url, f2, mode = "wb")

url <- "https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1988918411932"
f3 <- tempfile(fileext = ".xlsx")
download.file(url, f3, mode = "wb")

url <- "https://ucla.app.box.com/index.php?rm=box_download_shared_file&shared_name=9d8qnnduhus4bd5mwqt7l95kz34fic2v&file_id=f_1988922011366"
f4 <- tempfile(fileext = ".xlsx")
download.file(url, f4, mode = "wb")

inadmissibles_h1 <- readxl::read_excel(path = f1, sheet = 1, skip = 2)
inadmissibles_h2 <- readxl::read_excel(path = f2, sheet = 1, skip = 2)

inadmissibles_h3 <- readxl::read_excel(path = f3, sheet = 1)
inadmissibles_h4 <- readxl::read_excel(path = f4, sheet = 1) |> slice(-1)

inadmissibles <-
  list(
    "ofo_case-by-case_nationwide_inadmissable_oct_2025_through_march_19_2025_part1_raw.xlsx" = inadmissibles_h1 |>
      filter(`Event Created Datetime` <= as.Date("2024-12-31")),
    "ofo_case-by-case_nationwide_inadmissable_oct_2025_through_march_19_2025_part2_raw.xlsx" = inadmissibles_h2 |>
      filter(`Event Created Datetime` <= as.Date("2024-12-31")),
    "ofo_detailed_inadmissable_data_january_1_2025_to_may_31_2025_raw.xlsx" = inadmissibles_h3,
    "ofo_detailed_inadmissable_data_june_1_2025_to_september_6_2025_raw" = inadmissibles_h4
  )

inadmissibles <- map_dfr(inadmissibles, process_inadmissibles, .id = "file")

inadmissibles |>
  arrow::write_feather("data/inadmissibles.feather")

inadmissibles |>
  # turn charge_list into a comma-separated string
  mutate(charge = map_chr(charge_list, ~ paste(unlist(.x), collapse = ", "))) |>
  select(-charge_list) |>
  writexl::write_xlsx("data/inadmissibles.xlsx")
