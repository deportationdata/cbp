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
  "COCOS", "COCOS",
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
  "PWAM", "Present Without Admission Mexico",
  "TWA",  "Temporary Work Agriculture",
  "O",    "Other",
  "PWAC", "Present Without Admission Canada",
  "PWAO", "Present Without Admission Other",
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
  "OFC",  "Oral False Claim",       # NIEM slight mismatch but consistent here
  "DFC",  "Documented False Claim", # NIEM slight mismatch but consistent here
  "R",    "Refugee",
  "324",  "Smuggler",
  "A",    "Asylum",
  "C",    "Crew",
  "I",    "Immigrant",
  "TWOV", "Transit Without Visa",
  
  # alternate and truncated labels
  "ABSCONDER",                     "Absconder",
  "False Claim with Counterfeit",  "False Claim with Counterfeit Document",
  "False Claim with Counterfeit D", "False Claim with Counterfeit Document",
  "False Claim with Valid Documen", "False Claim with Valid Document",
  "False Claim with Valid Docum...", "False Claim with Valid Document",
  "False Claim with Altered Docum", "False Claim with Altered Document",
  "ORAL FALSE CLAIMS TO OTHER THA", "Oral False Claim to Other Than CBP",
  "ORAL FALSE CLAIMS TO O...",      "Oral False Claim to Other Than CBP",
  "Oral False Claim to U.S. Citiz", "Oral False Claim to U.S. Citizen",
  "PWA Canada",                    "Present Without Admission Canada",
  "PWA Mexico",                    "Present Without Admission Mexico",
  "PWA Other",                     "Present Without Admission Other",
  "US CITIZEN",                    "US Citizen"
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
  "ADMDPT", "Administrative Deportation (I-851/I-851A)",
  "ADMIT", "Admitted",
  "B", "Bag and Baggage",
  "DTNR", "Detainer",
  "ER", "Expedited Removal (I-860)",
  "Expedited Removal - Securing the Border", "Expedited Removal (I-860)",
  "ER/CF", "Expedited Removal with Credible Fear",
  "Expedited Removal/Credible Fear - Securing the Border", "Expedited Removal with Credible Fear",
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

# CDS program mappings
cds_mapping <- tribble(
  ~code,      ~full_name,
  "ATEP",     "Alien Transfer Exit Program",
  "MIRP",     "Mexican Interior Repatriation Program",
  "OASISS",   "Operation against Smugglers Initiative on Safety and Security",
  "IRI",      "Interior Repatriation Initiative",
  "QUICK",    "Quick Court",
  "STRMLINE", "Streamline"
)

# transfer group mappings
transfer_group_mapping <- tribble(
  ~code, ~full_name,
  "ERO", "Enforcement and Removal Operations",
  "ORR", "Office of Refugee Resettlement",
  "POE", "Port of Entry",
  "TOT", "Turned Over To"
)

juvenile_mapping <- tribble(
  ~code, ~full_name,
  "Child",  "Juvenile"
)

time_in_us_mapping <- tribble(
  ~code, ~full_name,
  "1-1", "1 month to 1 year",
  "4-30", "4 to 30 days",
  "AE", "At Entry",
  "OVER1", "Over 1 year",
  "WI72", "Within 72 hours"
)

group_mapping <- tribble(
  ~code, ~full_name,
  "FMUA", "Family Unit Alien",
  "FMUA AAC", "FMUA Accompanied Alien Children",
  "Single Adults", "Single Adult",
  "UC", "Unaccompanied Child"
)

prosecution_mapping <- tribble(
  ~code, ~full_name,
  "Y", "Referred"
)

# format statute charge values before building lookup
format_statute_charge <- function(charge) {
  charges_tbl <- tibble(charge)
  
  charges_tbl <- charges_tbl |>
    mutate(
      # remove punctuation and whitespace before parsing charge code
      charge_str = str_remove_all(charge, "[[:punct:]\\s]+"),
      # extract numeric INA section prefix (e.g. "212", "237").
      section = str_extract(charge_str, "^\\d+"),
      remainder = str_remove(charge_str, "^\\d+") |>
        # lowercase first letter followed by a digit
        str_replace("^[A-Z](?=[0-9])", "a") |>
        # split remainder at each case or type boundary
        str_replace_all(
          "(?<=[a-z])(?=[A-Z0-9])|(?<=[A-Z])(?=[a-z0-9])|(?<=[0-9])(?=[A-Za-z])|\\s+",
          ")("
        ) |>
        # wrap remainder in parentheses
        str_replace("^(.+)$", "(\\1)") |>
        # strip leading zeros from numeric subsections
        str_replace_all("\\(0+(\\d)", "(\\1") |>
        # drop empty parentheses
        str_replace_all("\\(\\)", ""),
      # combine section and parsed remainder into final citation string
      charge_str = if_else(
        is.na(section),
        NA_character_,
        glue::glue("{section}{remainder}")
      )
    )
  
  charges_tbl <- charges_tbl |>
    mutate(
      charge_str = case_when(
        charge == "212a03F" ~ "212(a)(3)(F)",
        charge == "215a" ~ "215(a)",
        charge == "215b" ~ "215(b)",
        charge == "237s02AiI" ~ "237(a)(2)(A)(i)(I)",
        charge == "2153g" ~ "215(g)",
        charge == "2153h" ~ "215(h)",
        TRUE ~ charge_str
      )
    )
  
  as.character(charges_tbl$charge_str)
}

# translate each distinct value once, preserving case for subsection boundaries
write_statute_charge_lookup <- function(con) {
  charges_tbl <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT statute_charge AS code
     FROM combined_input
     WHERE statute_charge IS NOT NULL"
  ) |>
    mutate(full_name = format_statute_charge(code))
  
  DBI::dbWriteTable(
    con,
    "statute_charge_lookup",
    charges_tbl,
    temporary = TRUE,
    overwrite = TRUE
  )
}

# combine mappings
code_map <- bind_rows(
  cds_mapping |>
    mutate(field = "cds_program"),
  
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
    mutate(field = "sector"),
  
  sector_mapping |>
    mutate(field = "bookout_sector"),
  
  method_mapping |>
    mutate(field = "arrest_method"),
  
  gender_mapping |>
    mutate(field = "gender"),
  
  disposition_mapping |>
    mutate(field = "disposition"),
  
  transfer_group_mapping |>
    mutate(field = "transfer_to_group"),
  
  transfer_group_mapping |>
    mutate(field = "removal_type"),
  
  juvenile_mapping |>
    mutate(field = "adult_or_juvenile"),
  
  time_in_us_mapping |>
    mutate(field = "time_in_us"),
  
  group_mapping |>
    mutate(field = "subject_group_classification"),
  
  prosecution_mapping |>
    mutate(field = "referred_for_prosecution_under_8usc1325_or_8usc1326")
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
  stop("Resolve duplicate mapping keys before cleaning.")
}

code_map <- code_map |>
  arrange(field, code)

# END
