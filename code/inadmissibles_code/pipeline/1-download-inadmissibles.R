# load packages 
library(tidyverse)
library(rvest)
library(httr2)
library(xml2)
library(arrow)

# store URL
inads_url <- "https://www.cbp.gov/document/foia-record/cbp-office-field-operations-statistics"

# set paths
download_dir <- "data/inadmissibles"
raw_dir <- file.path(download_dir, "raw")
manual_review_dir <- file.path(download_dir, "manual_review")
metadata_dir <- file.path(download_dir, "metadata")

# outputs 
link_inventory_path <- file.path(metadata_dir, "inadmissibles_links.parquet")

# create folders
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(manual_review_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)

# CBP requests
cbp_request <- function(url) {
  request(url) |>
    req_user_agent("Mozilla/5.0") |>
    req_options(http_version = 1)
}

resp <- cbp_request(inads_url) |>
  req_perform()

html <- resp_body_string(resp)
cbp_page <- read_html(html)

# extract hyperlinks and build table
links <- tibble(
  text = cbp_page |> html_elements("a") |> html_text2(),
  href = cbp_page |> html_elements("a") |> html_attr("href")
) |>
  filter(!is.na(href)) |>
  mutate(full_url = url_absolute(href, inads_url))

# classify downloadable files
inadmissibles_links <- links |>
  filter(str_detect(full_url, "\\.xlsx|\\.xls|\\.csv|\\.zip")) |>
  mutate(
    text_lower = str_to_lower(text),
    # files to include 
    is_inadmissibles =
      str_detect(
        text_lower,
        "inadmissible|inadmissable|inadmissble"
      ) ,
    # files for manual review 
    is_too_specific =
      str_detect(
        text_lower,
        paste(
          c(
            "cuban", "field", "42"
          ),
          collapse = "|"
        )
      ),
    
    download_class = case_when(
      # files we definitely want 
      (is_inadmissibles) & !is_too_specific ~ "include_inadmissibles",
      # manual review, anything that failed the above condition but still
      # includes apprehension, apprehenion, or t8
      str_detect(text_lower, "inadmissible|inadmissable|inadmissble") ~ "manual_review",
      # exclude the rest 
      TRUE ~ "exclude"
    )
  ) |>
  filter(download_class != "exclude") |>
  distinct(full_url, .keep_all = TRUE)

print(inadmissibles_links, n = Inf)

# read previous link inventory if it exists
old_inadmissibles_links <- if (file.exists(link_inventory_path)) {
  read_parquet(link_inventory_path)
} else {
  tibble(
    text = character(),
    href = character(),
    full_url = character(),
    text_lower = character(),
    is_inadmissibles = logical(),
    is_too_specific = logical(),
    download_class = character()
  )
}

# keep only links not already downloaded/recorded
new_inadmissibles_links <- inadmissibles_links |>
  anti_join(old_inadmissibles_links, by = "full_url")

# flag if manual review triggered
manual_count <- sum(new_inadmissibles_links$download_class == "manual_review")

if (manual_count > 0) {
  warning(
    paste0(
      manual_count,
      "WARNING: File(s) flagged for manual review. Check data/inadmissibles/manual_review/"))
  }

print(new_inadmissibles_links, n = Inf)

# function to download files
download_cbp_file <- function(url, dest_dir) {
  
  dest <- file.path(dest_dir, basename(url))
  
  cbp_request(url) |>
    req_perform(path = dest)
  
  dest
}

# download included inadmissibles files into raw/
downloaded_inadmissibles_files <- new_inadmissibles_links |>
  filter(download_class == "include_inadmissibles") |>
  pull(full_url) |>
  map_chr(download_cbp_file, dest_dir = raw_dir)

# download manual review files into manual_review/
downloaded_manual_review_files <- new_inadmissibles_links |>
  filter(download_class == "manual_review") |>
  pull(full_url) |>
  map_chr(download_cbp_file, dest_dir = manual_review_dir)

print(downloaded_inadmissibles_files)
print(downloaded_manual_review_files)

# update link inventory
updated_inadmissibles_links <- bind_rows(
  old_inadmissibles_links,
  inadmissibles_links
) |>
  distinct(full_url, .keep_all = TRUE)

write_parquet(
  updated_inadmissibles_links,
  link_inventory_path
)

cat("New inadmissibles files:", length(downloaded_inadmissibles_files), "\n")
cat("New manual-review files:", length(downloaded_manual_review_files), "\n")

