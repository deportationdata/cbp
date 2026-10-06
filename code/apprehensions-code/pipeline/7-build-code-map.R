# use UTC for source clock times, source time zone is unspecified
Sys.setenv(TZ = "UTC")

# load packages
library(tidyverse)
library(arrow)

# paths
dataset_dir <- "data/apprehensions"
metadata_dir <- file.path(dataset_dir, "metadata")
code_map_path <- file.path(metadata_dir,"code-map.parquet")
dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)


#### Write Map ####

# country mappings
country_mapping <- tribble(
  ~code, ~full_name,
  "AFGHA", "Afghanistan",
  "ALBAN", "Albania",
  "ALGER", "Algeria",
  "ANGOL", "Angola",
  "ANGUI", "Anguilla",
  "ANTIG", "Antigua and Barbuda",
  "ARGEN", "Argentina",
  "ARMEN", "Armenia",
  "AZERB", "Azerbaijan",
  "BAHAM", "Bahamas",
  "BAHRA", "Bahrain",
  "BANGL", "Bangladesh",
  "BELAR", "Belarus",
  "BELGI", "Belgium",
  "BELIZ", "Belize",
  "BENIN", "Benin",
  "BHUTA", "Bhutan",
  "BISSA", "Guinea-Bissau",
  "BOLIV", "Bolivia",
  "BOSNI", "Bosnia and Herzegovina",
  "BOTSW", "Botswana",
  "BRAZI", "Brazil",
  "BULGA", "Bulgaria",
  "BURKI", "Burkina Faso",
  "BURMA", "Myanmar",
  "BURUN", "Burundi",
  "BVI", "British Virgin Islands",
  "CAFRI", "Central African Republic",
  "CAMBO", "Cambodia",
  "CAMER", "Cameroon",
  "CANAD", "Canada",
  "CAPEV", "Cabo Verde",
  "CAPE VERDE", "Cabo Verde",
  "CHAD", "Chad",
  "CHILE", "Chile",
  "CHINA", "China",
  "CHINA, PEOPLES REPUBLIC OF", "China",
  "COLOM", "Colombia",
  "COMOR", "Comoros",
  "COSTA", "Costa Rica",
  "COTED", "Côte d'Ivoire",
  "CROAT", "Croatia",
  "CUBA", "Cuba",
  "CZREP", "Czech Republic",
  "DECON", "Democratic Republic of the Congo",
  "DJIBO", "Djibouti",
  "DR", "Dominican Republic",
  "ECUAD", "Ecuador",
  "EGYPT", "Egypt",
  "ELSAL", "El Salvador",
  "EQUAT", "Equatorial Guinea",
  "ERITR", "Eritrea",
  "ETHIO", "Ethiopia",
  "FIJI", "Fiji",
  "FRANC", "France",
  "GABON", "Gabon",
  "GAMBI", "Gambia",
  "GEORG", "Georgia",
  "GERMA", "Germany",
  "GHANA", "Ghana",
  "GREEC", "Greece",
  "GUADE", "Guadeloupe",
  "GUAM", "Guam",
  "GUATE", "Guatemala",
  "GUYAN", "Guyana",
  "HAITI", "Haiti",
  "HONDU", "Honduras",
  "HONGK", "Hong Kong",
  "HUNGA", "Hungary",
  "INDIA", "India",
  "INDON", "Indonesia",
  "IRAN", "Iran",
  "IRAQ", "Iraq",
  "IRELA", "Ireland",
  "ISRAE", "Israel",
  "ITALY", "Italy",
  "JAMAI", "Jamaica",
  "JAPAN", "Japan",
  "JORDA", "Jordan",
  "KAZAK", "Kazakhstan",
  "KENYA", "Kenya",
  "KOREA", "Korea",
  "KOSOV", "Kosovo",
  "KUWAI", "Kuwait",
  "KYRGY", "Kyrgyzstan",
  "LAOS", "Laos",
  "LATVI", "Latvia",
  "LEBAN", "Lebanon",
  "LIBER", "Liberia",
  "LIBYA", "Libya",
  "LITHU", "Lithuania",
  "MACED", "North Macedonia",
  "MACEDONIA", "North Macedonia",
  "MALAY", "Malaysia",
  "MALI", "Mali",
  "MAUTA", "Mauritania",
  "MEXIC", "Mexico",
  "MOLDO", "Moldova",
  "MONGO", "Mongolia",
  "MONTE", "Montenegro",
  "MOROC", "Morocco",
  "MOZAM", "Mozambique",
  "NAMIB", "Namibia",
  "NEPAL", "Nepal",
  "NETHE", "Netherlands",
  "NEWZE", "New Zealand",
  "NICAR", "Nicaragua",
  "NIGE", "Niger",
  "NIGIA", "Nigeria",
  "OMAN", "Oman",
  "PAKIS", "Pakistan",
  "PANAM", "Panama",
  "PARAG", "Paraguay",
  "PERU", "Peru",
  "PHILI", "Philippines",
  "POLAN", "Poland",
  "PORTU", "Portugal",
  "QATAR", "Qatar",
  "ROMAN", "Romania",
  "RUSSI", "Russia",
  "RWAND", "Rwanda",
  "SAFRI", "South Africa",
  "SAUDI", "Saudi Arabia",
  "SENEG", "Senegal",
  "SIERR", "Sierra Leone",
  "SINGA", "Singapore",
  "SKORE", "South Korea",
  "SLOVA", "Slovakia",
  "SOMAL", "Somalia",
  "SOSUD", "South Sudan",
  "SPAIN", "Spain",
  "SRBIA", "Serbia",
  "SRILA", "Sri Lanka",
  "STLUC", "Saint Lucia",
  "STVIN", "Saint Vincent and the Grenadines",
  "SUDAN", "Sudan",
  "SWAZI", "Eswatini",
  "SWAZILAND", "Eswatini",
  "SWEDE", "Sweden",
  "SYRIA", "Syria",
  "TAIWA", "Taiwan",
  "TAJIK", "Tajikistan",
  "TANZA", "Tanzania",
  "THAIL", "Thailand",
  "TOGO", "Togo",
  "TRINI", "Trinidad and Tobago",
  "TUNIS", "Tunisia",
  "TURKE", "Türkiye",
  "TURKIYE", "Türkiye",
  "TURKEY", "Türkiye",
  "TURKM", "Turkmenistan",
  "UAE", "United Arab Emirates",
  "UGAND", "Uganda",
  "UINEA", "Guinea",
  "UK", "United Kingdom",
  "UKRAI", "Ukraine",
  "UNKNO", "Unknown",
  "URUGU", "Uruguay",
  "US", "United States",
  "USSR", "Soviet Union",
  "UZBEK", "Uzbekistan",
  "VENEZ", "Venezuela",
  "VIETN", "Vietnam",
  "WSAHA", "Western Sahara",
  "YEMEN", "Yemen",
  "ZAMBI", "Zambia",
  "ZIMBA", "Zimbabwe",
  
  # miscellaneous / unsure = left as is
  "CONGO", "CONGO",
  "FSTMA", "FSTMA",
  "INICA", "INICA",
  "RALIA", "RALIA",
  "STATE", "STATE",
  "STRIA", "STRIA",
  "STMAR", "STMAR"
)

# ethnicity mappings
ethnicity_mapping <- tribble(
  ~code, ~full_name,
  "H",   "Hispanic",
  "N",   "Not Hispanic",
  "U",   "Unknown"
)

# marital mappings
marital_mapping <- tribble(
  ~code,       ~full_name,
  "S",         "Single",
  "M",         "Married",
  "U",         "Unknown",
  "D",         "Divorced",
  "W",         "Widowed",
  
  # unsure = left as is
  "X",   "X"
)

# entry mappings
entry_mapping <- tribble(
  ~code, ~full_name,
  
  # codes
  "PWAM", "PWA Mexico",
  "TWA",  "Temporary Work Agriculture",
  "O",    "Other",
  "PWAC", "PWA Canada",
  "PWAO", "PWA Other",
  "BCC",  "Border Crossing Card",
  "PAR",  "Parolee",
  "NIM",  "Non-Immigrant",
  "V",    "Visitor",
  "LPR",  "Legal Permanent Resident",
  "STUD", "Student",
  "TWO",  "Temporary Worker Other",
  "IMP",  "Imposter",
  "ALT",  "False Claim with Altered Document",
  "CFT",  "False Claim with Counterfeit Document",
  "TR",   "Temporary Resident",
  "CR",   "Conditional Resident",
  "STOW", "Stowaway",
  "PWA",  "Present Without Admission",
  "NIC",  "Not in Custody",
  "VWP",  "Visa Waiver Program",
  "OFC", "Oral False Claim", # NIEM slight mismatch but consistent here
  "DFC", "Documented False Claim", # NIEM slight mismatch but consistent here
  "R",    "Refugee",
  "324",  "Smuggler",
  "A",    "Asylum",
  "C",    "Crew",
  "I",    "Immigrant",
  
  # truncated labels
  "False Claim with Counterfeit D", "False Claim with Counterfeit Document",
  "False Claim with Valid Documen", "False Claim with Valid Document",
  "False Claim with Altered Docum", "False Claim with Altered Document",
  "ORAL FALSE CLAIMS TO OTHER THA", "Oral False Claim to Other Than CBP",
  "Oral False Claim to U.S. Citiz", "Oral False Claim to U.S. Citizen"
)

# border mappings
border_mapping <- tribble(
  ~code, ~full_name,
  "SBO",  "Southwest Border",
  "CBO",  "Coastal Border",
  "NBO",  "Northern Border",
  "SB",   "Southwest Border",
  "CB",   "Coastal Border",
  "NB",   "Northern Border"
)

# sector mappings
sector_mapping <- tribble(
  ~code, ~full_name,
  "BBT", "Big Bend Sector",
  "BLW", "Blaine Sector",
  "BUN", "Buffalo Sector",
  "DRT", "Del Rio Sector",
  "DTM", "Detroit Sector",
  "ELC", "El Centro Sector",
  "EPT", "El Paso Sector",
  "GFN", "Grand Forks Sector",
  "HLT", "Houlton Sector",
  "HVM", "Havre Sector",
  "LRT", "Laredo Sector",
  "MIP", "Miami Sector",
  "NLL", "New Orleans Sector",
  "RGV", "Rio Grande Valley Sector",
  "RMY", "Ramey Sector",
  "SDC", "San Diego Sector",
  "SPW", "Spokane Sector",
  "SWB", "Swanton Sector",
  "TCA", "Tucson Sector",
  "YUM", "Yuma Sector"
)

method_mapping <- tribble(
  ~code, ~full_name,
  "Organized Crime Drug Enforcement Task Fo", "Organized Crime Drug Enforcement Task Force"
)

# gender mappings
gender_mapping <- tribble(
  ~code, ~full_name,
  "F", "Female",
  "M", "Male"
)

# disposition mappings
disposition_mapping <- tribble(
  ~code, ~full_name,
  "ADMDPT", "Administrative Deportation I-851/I-851A",
  "ADMIT", "Admitted",
  "B", "Bag and Baggage",
  "DTNR", "Detainer",
  "ER", "Expedited Removal (I-860)",
  "ER/CF", "Expedited Removal with Credible Fear",
  "ER/LR", "Expedited Removal Limited Review",
  "EX", "Exclusion",
  "I", "I-210",
  "NAR", "Not Amenable to Removal",
  "NTA", "Notice to Appear Released (I-862)",
  "NTA/DT", "Notice to Appear Detained (I-862)",
  "P", "Paroled",
  "REINRF", "Reinstatement of Deportation Reasonable Fear",
  "REINST", "Reinstatement of Deportation Order I-871",
  "REL", "Release",
  "STOW", "Stowaway",
  "T", "Other",
  "TOT", "Turned Over To",
  "V", "Voluntary Return",
  "VWP/CF", "VWP with I-863 Notice to Refer (Refer to IJ)",
  "VWPPRM", "VWPP Removal",
  "VWPRM", "VWP Removal",
  "WA/NTA", "Warrant of Arrest/Notice to Appear"
)

# charge code mappings

# combine mappings
code_map <- bind_rows(
  country_mapping |>
    mutate(field = "citizenship"),
  
  country_mapping |>
    mutate(field = "birth_country"),
  
  country_mapping |>
    mutate(field = "residence_country"),
  
  marital_mapping |>
    mutate(field = "marital_status"),
  
  entry_mapping |>
    mutate(field = "entry_status"),
  
  ethnicity_mapping |>
    mutate(field = "ethnicity"),
  
  border_mapping |>
    mutate(field = "border"),
  
  sector_mapping |>
    mutate(field = "arrest_sector"),
  
  sector_mapping |>
    mutate(field = "bookout_sector"),
  
  method_mapping |>
    mutate(field = "arrest_method"),
  
  gender_mapping |>
    mutate(field = "gender_mapping"),
  
  disposition_mapping |>
    mutate(field = "disposition")
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
