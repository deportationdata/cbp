# load packages 
library(tidyverse)
library(rvest)
library(httr2)
library(xml2)
library(arrow)

# store URL
cbp_apps_url <- "https://www.cbp.gov/document/foia-record/customs-and-border-protection-border-patrol-statistics"

# set paths
download_dir <- "data/apprehensions"
raw_dir <- file.path(download_dir, "raw")
manual_review_dir <- file.path(download_dir, "manual_review")
metadata_dir <- file.path(download_dir, "metadata")

# outputs 
link_inventory_path <- file.path(metadata_dir, "apprehension_links.parquet")

# create folders
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(manual_review_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)

# helper function for CBP requests
cbp_request <- function(url) {
  request(url) |>
    req_user_agent("Mozilla/5.0") |>
    req_options(http_version = 1)
}

# request page
resp <- cbp_request(cbp_apps_url) |>
  req_perform()

html <- resp_body_string(resp)
cbp_page <- read_html(html)

# extract hyperlinks and build table
links <- tibble(
  text = cbp_page |> html_elements("a") |> html_text2(),
  href = cbp_page |> html_elements("a") |> html_attr("href")
) |>
  filter(!is.na(href)) |>
  mutate(full_url = url_absolute(href, cbp_apps_url))

# classify downloadable files
apprehension_links <- links |>
  filter(str_detect(full_url, "\\.xlsx|\\.xls|\\.csv|\\.zip")) |>
  mutate(
    text_lower = str_to_lower(text),
    # files to include 
    is_nationwide_apprehensions =
      str_detect(
        text_lower,
        "nationwide apprehension|nationwide apprehensions"
      ) |
      str_detect(
        text_lower,
        "^usbp apprehensions fy"
      ),
    # is_subject_details =
      # str_detect(
        # text_lower,
        # "^usbp encounter subject details"
      # ),
    # files to exclude 
    is_too_specific =
      str_detect(
        text_lower,
        paste(
          c(
            "gotaway", "gotaways", "turnback", "turnbacks",
            "dna", "ketamine", "seizure", "seizures",
            "pregnancy", "pregnant","death", "deaths",
            "drug", "fentanyl", "heroin", "smugglers", 
            "smuggler", "smuggling","texas national guard", "department of public safety",
            "ramsey sector", "dominican", "haitian", "return to sender",
            "title 8", "usc 1304", "t8", "height weight",
            "demographic", "demo", "citizenship and sector",
            "sector only", "unaccompanied children", "family units",
            "migrant deaths", "borstar", "weapon", "rescue",
            "special interest aliens", "place of origin",
            "subject type", "t42", "separated children"
          ),
          collapse = "|"
        )
      ),
    
    download_class = case_when(
      (is_nationwide_apprehensions #| is_subject_details) 
       ) & !is_too_specific ~ "include_apprehensions",
      str_detect(text_lower, "apprehension|apprehensions|encounter subject details") ~ "manual_review",
      TRUE ~ "exclude"
    )
  ) |>
  filter(download_class != "exclude") |>
  distinct(full_url, .keep_all = TRUE)

print(apprehension_links, n = Inf)

# read previous link inventory if it exists
old_apprehension_links <- if (file.exists(link_inventory_path)) {
  read_parquet(link_inventory_path)
} else {
  tibble(
    text = character(),
    href = character(),
    full_url = character(),
    text_lower = character(),
    is_nationwide_apprehensions = logical(),
    # is_subject_details = logical(),
    is_too_specific = logical(),
    download_class = character()
  )
}

# keep only links not already downloaded/recorded
new_apprehension_links <- apprehension_links |>
  anti_join(old_apprehension_links, by = "full_url")

# flag if manual review triggered
manual_count <- sum(new_apprehension_links$download_class == "manual_review")

if (manual_count > 0) {
  warning(
    paste0(
      manual_count,
      " WARNING: File(s) flagged for manual review. Check data/apprehensions/manual_review/"
    )
  )
}

print(new_apprehension_links, n = Inf)

# function to download files
download_cbp_file <- function(url, dest_dir) {
  
  dest <- file.path(dest_dir, basename(url))
  
  cbp_request(url) |>
    req_perform(path = dest)
  
  return(dest)
}

# download included apprehension files into raw/
downloaded_apprehension_files <- new_apprehension_links |>
  filter(download_class == "include_apprehensions") |>
  pull(full_url) |>
  map_chr(download_cbp_file, dest_dir = raw_dir)

# download manual review files into manual_review/
downloaded_manual_review_files <- new_apprehension_links |>
  filter(download_class == "manual_review") |>
  pull(full_url) |>
  map_chr(download_cbp_file, dest_dir = manual_review_dir)

print(downloaded_apprehension_files)
print(downloaded_manual_review_files)

# update link inventory
updated_apprehension_links <- bind_rows(
  old_apprehension_links,
  apprehension_links
) |>
  distinct(full_url, .keep_all = TRUE)

write_parquet(
  updated_apprehension_links,
  link_inventory_path
)

cat("New apprehension files:", length(downloaded_apprehension_files), "\n")
cat("New manual-review files:", length(downloaded_manual_review_files), "\n")



