# load packages 
library(tidyverse)
library(rvest)
library(httr2)
library(xml2)

# store URL
cbp_apps_url <- "https://www.cbp.gov/document/foia-record/customs-and-border-protection-border-patrol-statistics"

# set paths
download_dir <- "data/apprehensions"
link_inventory_path <- file.path(download_dir, "apprehension_links.csv")

# request page
resp <- request(cbp_apps_url) |>
  req_user_agent("Mozilla/5.0") |>
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

# keep only downloadable apprehensions files
apprehension_links <- links |>
  filter(str_detect(full_url, "\\.xlsx|\\.xls|\\.csv|\\.zip")) |>
  filter(str_detect(str_to_lower(text), "apprehension")) |>
  distinct(full_url, .keep_all = TRUE)

print(apprehension_links, n = Inf)

# create folder for downloads
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

# read previous link inventory if it exists
old_apprehension_links <- if (file.exists(link_inventory_path)) {
  read_csv(link_inventory_path, show_col_types = FALSE)
} else {
  tibble(text = character(), href = character(), full_url = character())
}

# keep only links not already downloaded/recorded
new_apprehension_links <- apprehension_links |>
  anti_join(old_apprehension_links, by = "full_url")

print(new_apprehension_links, n = Inf)

# function to download files
download_cbp_file <- function(url, dest_dir = download_dir) {
  
  dest <- file.path(dest_dir, basename(url))
  
  request(url) |>
    req_user_agent("Mozilla/5.0") |>
    req_perform(path = dest)
  
  return(dest)
}

# download new apprehensions files only
downloaded_apprehension_files <- map_chr(
  new_apprehension_links$full_url,
  download_cbp_file
)

print(downloaded_apprehension_files)

# update link inventory
updated_apprehension_links <- bind_rows(
  old_apprehension_links,
  apprehension_links
) |>
  distinct(full_url, .keep_all = TRUE)

write_csv(
  updated_apprehension_links,
  link_inventory_path
)