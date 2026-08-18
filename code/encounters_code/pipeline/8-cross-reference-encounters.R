# load packages
library(tidyverse)
library(lubridate)
library(arrow)
library(httr2)
library(DBI)
library(duckdb)

# paths
encounters_dir <- "data/encounters"
processed_dir <- file.path(encounters_dir, "processed")
validation_dir <- file.path(encounters_dir, "validation")

dir.create(
  validation_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

encounters_final_path <- file.path(
  processed_dir,
  "encounters_final.parquet"
)

cross_reference_csv_path <- file.path(
  validation_dir,
  "encounters_monthly_cross_reference.csv"
)

cross_reference_parquet_path <- file.path(
  validation_dir,
  "encounters_monthly_cross_reference.parquet"
)

#### CBP DASHBOARD: manually check pending updates ####

# area-of-responsibility tribble, 2021-2026
cbp_benchmarks <- tribble(
  ~benchmark_id, ~release_date, ~url,
  "fy20_fy23", as.Date("2023-11-01"),
  "https://www.cbp.gov/sites/default/files/assets/documents/2023-Nov/nationwide-encounters-fy20-fy23-aor.csv",
  
  "fy21_fy24", as.Date("2024-10-01"),
  "https://www.cbp.gov/sites/default/files/2024-10/nationwide-encounters-fy21-fy24-aor.csv",
  
  "fy22_fy25", as.Date("2025-11-01"),
  "https://www.cbp.gov/sites/default/files/2025-11/nationwide-encounters-fy22-fy25-aor.csv",
  
  "fy23_fy26_july", as.Date("2026-08-01"),
  "https://www.cbp.gov/sites/default/files/2026-08/nationwide-encounters-fy23-fy26-jul-aor.csv"
) |>
  mutate(
    local_path = file.path(
      validation_dir,
      basename(url)
    )
  )

# download local copies
walk2(
  cbp_benchmarks$url,
  cbp_benchmarks$local_path,
  \(url, local_path) {
    request(url) |>
      req_user_agent("Mozilla/5.0") |>
      req_options(http_version = 1) |>
      req_perform(path = local_path)
  }
)

cbp_raw <- pmap_dfr(
  cbp_benchmarks,
  \(benchmark_id, release_date, url, local_path) {
    read_csv(
      local_path,
      col_types = cols(
        `Fiscal Year` = col_character()
      ),
      show_col_types = FALSE
    ) |>
      mutate(
        benchmark_id = benchmark_id,
        benchmark_release_date = release_date
      )
  }
)

required_cbp_columns <- c(
  "Fiscal Year",
  "Month (abbv)",
  "Component",
  "Title of Authority",
  "Encounter Type",
  "Encounter Count"
)

missing_cbp_columns <- setdiff(
  required_cbp_columns,
  names(cbp_raw)
)

if (length(missing_cbp_columns) > 0) {
  stop(
    "The CBP benchmark is missing required columns: ",
    paste(missing_cbp_columns, collapse = ", ")
  )
}

# encounters_final contains USBP records only (not OFO inads)
# retain only USBP encounter types (T8 apps + T42 expulsions)
cbp_usb_monthly <- cbp_raw |>
  filter(
    Component == "U.S. Border Patrol"
  ) |>
  mutate(
    fiscal_year = parse_number(`Fiscal Year`),
    month_abbreviation = str_to_upper(`Month (abbv)`),
    fiscal_month = match(
      month_abbreviation,
      str_to_upper(month.abb)
    )
  )

if (any(is.na(cbp_usb_monthly$fiscal_month))) {
  stop("The CBP benchmark contains an unrecognized month abbreviation.")
}

cbp_usb_monthly <- cbp_usb_monthly |>
  mutate(
    calendar_year = if_else(
      fiscal_month >= 10L,
      fiscal_year - 1L,
      fiscal_year
    ),
    month_start = make_date(
      calendar_year,
      fiscal_month,
      1L
    )
  ) |>
  group_by(
    benchmark_id,
    benchmark_release_date,
    fiscal_year,
    fiscal_month,
    month_start
  ) |>
  summarize(
    cbp_usb_encounter_count = sum(
      `Encounter Count`,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  group_by(month_start) |>
  slice_max(
    order_by = benchmark_release_date,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  arrange(month_start)

if (nrow(cbp_usb_monthly) == 0) {
  stop("No U.S. Border Patrol rows were found in the CBP benchmark.")
}

con <- dbConnect(
  duckdb()
)

dbExecute(
  con,
  "SET threads = 2"
)

# encounters final monthly counts
encounters_final_sql <- as.character(
  dbQuoteString(
    con,
    encounters_final_path
  )
)

benchmark_min_date_sql <- as.character(
  dbQuoteString(
    con,
    as.character(min(cbp_usb_monthly$month_start))
  )
)

benchmark_max_date_sql <- as.character(
  dbQuoteString(
    con,
    as.character(
      max(cbp_usb_monthly$month_start) %m+% months(1) - days(1)
    )
  )
)

# event date fields
event_date_sql <- paste0(
  "COALESCE(",
  "CAST(encounter_datetime AS DATE), ",
  "CAST(apprehension_datetime AS DATE), ",
  "apprehension_date",
  ")"
)

encounters_monthly_query <- sprintf(
  paste0(
    "SELECT ",
    "DATE_TRUNC('month', %1$s)::DATE AS month_start, ",
    "COUNT(*) AS encounters_final_count ",
    "FROM read_parquet(%2$s) ",
    "WHERE %1$s BETWEEN %3$s::DATE AND %4$s::DATE ",
    "GROUP BY 1 ",
    "ORDER BY 1"
  ),
  event_date_sql,
  encounters_final_sql,
  benchmark_min_date_sql,
  benchmark_max_date_sql
)

encounters_monthly <- dbGetQuery(
  con,
  encounters_monthly_query
) |>
  as_tibble() |>
  mutate(
    month_start = as.Date(month_start)
  )

dbDisconnect(
  con,
  shutdown = TRUE
)

#### Cross Reference ####

cross_reference <- cbp_usb_monthly |>
  full_join(
    encounters_monthly,
    by = "month_start"
  ) |>
  mutate(
    fiscal_year = coalesce(
      fiscal_year,
      if_else(
        month(month_start) >= 10L,
        year(month_start) + 1L,
        year(month_start)
      )
    ),
    fiscal_month = coalesce(
      fiscal_month,
      month(month_start)
    ),
    difference = encounters_final_count - cbp_usb_encounter_count,
    absolute_difference = abs(difference),
    percent_difference = if_else(
      cbp_usb_encounter_count == 0,
      NA_real_,
      100 * difference / cbp_usb_encounter_count
    ),
    exact_match = difference == 0,
    status = case_when(
      is.na(encounters_final_count) ~ "missing from encounters_final",
      is.na(cbp_usb_encounter_count) ~ "missing from CBP benchmark",
      exact_match ~ "exact match",
      TRUE ~ "difference"
    )
  ) |>
  arrange(month_start) |>
  select(
    fiscal_year,
    fiscal_month,
    month_start,
    encounters_final_count,
    cbp_usb_encounter_count,
    difference,
    absolute_difference,
    percent_difference,
    exact_match,
    status
  )

write_csv(
  cross_reference,
  cross_reference_csv_path,
  na = ""
)

write_parquet(
  cross_reference,
  cross_reference_parquet_path
)

#### Summary ####

comparison_summary <- cross_reference |>
  summarize(
    months_compared = n(),
    exact_matches = sum(exact_match, na.rm = TRUE),
    differing_months = sum(status == "difference"),
    missing_months = sum(
      status %in% c(
        "missing from encounters_final",
        "missing from CBP benchmark"
      )
    ),
    largest_absolute_difference = max(
      absolute_difference,
      na.rm = TRUE
    )
  )

print(
  cross_reference,
  n = Inf
)

print(
  comparison_summary
)

if (any(cross_reference$status != "exact match")) {
  warning(
    "The encounter cross-reference contains differences or missing months. ",
    "Review: ",
    cross_reference_csv_path
  )
}