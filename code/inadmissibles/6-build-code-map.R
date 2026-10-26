# use UTC for source clock times, source time zone is unspecified
Sys.setenv(TZ = "UTC")

# load packages
library(tidyverse)
library(arrow)

# paths
dataset_dir <- "data/inadmissibles"
metadata_dir <- file.path(dataset_dir, "metadata")
code_map_path <- file.path(metadata_dir,"code-map.parquet")
dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)


#### Write Map ####

# country mappings
country_mapping <- tribble(
  ~code, ~full_name,
  "METROPOLITAN, FRANCE", "FRANCE",
  "MYANMAR (BURMA)", "MYANMAR",
  "TAIWAN - REPUBLIC OF CHINA", "TAIWAN",
  "TURKIYE", "TÜRKIYE",
  "UNION OF SOVIET SOCIALIST REPS", "SOVIET UNION"
)

# state mappings
state_mapping <- tribble(
  ~code, ~full_name,
  # states 
  "AL", "Alabama",
  "AK", "Alaska",
  "AZ", "Arizona",
  "AR", "Arkansas",
  "CA", "California",
  "CO", "Colorado",
  "CT", "Connecticut",
  "DE", "Delaware",
  "FL", "Florida",
  "GA", "Georgia",
  "HI", "Hawaii",
  "ID", "Idaho",
  "IL", "Illinois",
  "IN", "Indiana",
  "IA", "Iowa",
  "KS", "Kansas",
  "KY", "Kentucky",
  "LA", "Louisiana",
  "ME", "Maine",
  "MD", "Maryland",
  "MA", "Massachusetts",
  "MI", "Michigan",
  "MN", "Minnesota",
  "MS", "Mississippi",
  "MO", "Missouri",
  "MT", "Montana",
  "NE", "Nebraska",
  "NV", "Nevada",
  "NH", "New Hampshire",
  "NJ", "New Jersey",
  "NM", "New Mexico",
  "NY", "New York",
  "NC", "North Carolina",
  "ND", "North Dakota",
  "OH", "Ohio",
  "OK", "Oklahoma",
  "OR", "Oregon",
  "PA", "Pennsylvania",
  "RI", "Rhode Island",
  "SC", "South Carolina",
  "SD", "South Dakota",
  "TN", "Tennessee",
  "TX", "Texas",
  "UT", "Utah",
  "VT", "Vermont",
  "VA", "Virginia",
  "WA", "Washington",
  "WV", "West Virginia",
  "WI", "Wisconsin",
  "WY", "Wyoming",
  
  # territories / other
  "DC", "District of Columbia",
  "GU", "Guam",
  "MP", "Northern Mariana Islands",
  "PR", "Puerto Rico",
  "VI", "U.S. Virgin Islands"
)

# date mappings
year_mapping <- tribble(
  ~code, ~full_name,
  "CY25", "2025"
)

# subject group classification mappings
group_mapping <- tribble(
  ~code, ~full_name,
  "FMUA", "Family Unit Alien",
  "FMUA AAC", "FMUA Accompanied Alien Children",
  "Single Adults", "Single Adult",
  "UC", "Unaccompanied Child",
  "UAC", "Unaccompanied Child",
  "YES", "Unaccompanied Child",
  "AM", "Accompanied Minor"
)

# gender mappings
gender_mapping <- tribble(
  ~code, ~full_name,
  "F - FEMALE", "Female",
  "M - MALE", "Male",
  "U - UNKNOWN", "Unknown"
)

# transfer group mappings
transfer_group_mapping <- tribble(
  ~code, ~full_name,
  "ERO", "Enforcement and Removal Operations",
  "ICE-ERO", "Enforcement and Removal Operations",
  "ORR", "Office of Refugee Resettlement",
  "POE", "Port of Entry",
  "TOT", "Turned Over To",
  "OFO", "Office of Field Operations",
  "USBP", "United States Border Patrol",
  "ICE-HSI", "Homeland Security Invesigations"
)

# duplicate mappings
duplicate_mapping <- tribble(
  ~code, ~full_name,
  "Duplicate subject to previous row of data.", "Y",
  "Duplicate subject to previous row of data", "Y",
  "Duplicate Subject to Previous Row", "Y",
  "#MULTIVALUE", "Y"
)

# combine mappings
code_map <- bind_rows(
  country_mapping |>
    mutate(field = "birth_country"),
  
  country_mapping |>
    mutate(field = "citizenship"),
  
  country_mapping |>
    mutate(field = "country"),
  
  state_mapping |>
    mutate(field = "state"),
  
  year_mapping |>
    mutate(field = "calendar_year"),
  
  group_mapping |>
    mutate(field = "fmua_uac_indicator"),
  
  gender_mapping |>
    mutate(field = "gender"),
  
  transfer_group_mapping |>
    mutate(field = "transfer_to_group"),
  
  duplicate_mapping |>
    mutate(field = "duplicate_subject_indicator")
) |>
  select(
    field,
    code,
    full_name
  )

#### Audits #### 

# check for missing or blank entries
invalid_mappings <- code_map |>
  filter(
    if_any(
      everything(),
      ~ is.na(.x) | str_squish(.x) == ""
    )
  )

if (nrow(invalid_mappings) > 0) {
  print(invalid_mappings, n = Inf)
  stop("Fix missing or blank mapping entries.")
}

# check duplicate keys 
duplicate_mappings <- code_map |>
  mutate(
    code_key = str_to_upper(str_squish(code))
  ) |>
  count(
    field,
    code_key,
    name = "n_mappings"
  ) |>
  filter(n_mappings > 1)

if (nrow(duplicate_mappings) > 0) {
  print(duplicate_mappings, n = Inf)
  stop("Resolve duplicate mapping keys before saving.")
}

# save
code_map <- code_map |>
  arrange(field, code)

write_parquet(
  code_map,
  code_map_path
)

cat(
  "\nMapping rows:", nrow(code_map),
  "\nSaved to:", code_map_path,
  "\n"
)

# END