# load packages 
library(tidyverse)
library(rvest)
library(httr2)
library(xml2)

# store URL
cbp_inads_url <- "https://www.cbp.gov/document/foia-record/cbp-office-field-operations-statistics"

# set paths
download_dir <- "data/inadmissibles"
link_inventory_path <- file.path(download_dir, "inadmissible_links.csv")

# request page
resp <- request(cbp_inads_url) |>
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
  mutate(full_url = url_absolute(href, cbp_inads_url))

# keep only downloadable inadmissibles files
inadmissible_links <- links |>
  filter(str_detect(full_url, "\\.xlsx|\\.xls|\\.csv|\\.zip")) |>
  filter(str_detect(str_to_lower(text), "inadmissible")) |>
  distinct(full_url, .keep_all = TRUE)

print(inadmissible_links, n = Inf)

# create folder for downloads
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

# read previous link inventory if it exists
old_inadmissible_links <- if (file.exists(link_inventory_path)) {
  read_csv(link_inventory_path, show_col_types = FALSE)
} else {
  tibble(text = character(), href = character(), full_url = character())
}

# keep only links not already downloaded/recorded
new_inadmissible_links <- inadmissible_links |>
  anti_join(old_inadmissible_links, by = "full_url")

print(new_inadmissible_links, n = Inf)

# function to download files
download_cbp_file <- function(url, dest_dir = download_dir) {
  
  dest <- file.path(dest_dir, basename(url))
  
  request(url) |>
    req_user_agent("Mozilla/5.0") |>
    req_perform(path = dest)
  
  return(dest)
}

# download new inadmissibles files only
downloaded_inadmissible_files <- map_chr(
  new_inadmissible_links$full_url,
  download_cbp_file
)

print(downloaded_inadmissible_files)

# update link inventory
updated_inadmissible_links <- bind_rows(
  old_inadmissible_links,
  inadmissible_links
) |>
  distinct(full_url, .keep_all = TRUE)

write_csv(
  updated_inadmissible_links,
  link_inventory_path
)