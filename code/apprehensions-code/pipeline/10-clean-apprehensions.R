# load packages
library(tidyverse)
library(DBI)
library(duckdb)

# paths
processed_dir <- "data/apprehensions/processed"

apprehensions_stacked_path <- file.path(
  processed_dir,
  "apprehensions-stacked.parquet"
)

apprehensions_cleaned_path <- file.path(
  processed_dir,
  "apprehensions-cleaned.parquet"
)

code_map_path <- "data/apprehensions/metadata/code-map.parquet"

 
# connect to duckDB
con <- dbConnect(
  duckdb()
)

# configuration
dbExecute(con, "SET threads = 1")
dbExecute(con, "SET preserve_insertion_order = false")
dbExecute(con, "SET max_temp_directory_size = '10GiB'")

# SQL file paths
stacked_sql <- as.character(
  dbQuoteString(
    con,
    apprehensions_stacked_path
  )
)

final_sql <- as.character(
  dbQuoteString(
    con,
    apprehensions_cleaned_path
  )
)

# get schema 
stacked_schema <- dbGetQuery(
  con,
  paste0(
    "DESCRIBE SELECT * ",
    "FROM read_parquet(",
    stacked_sql,
    ")"
  )
) |>
  as_tibble()

stacked_columns <- stacked_schema$column_name

# get row count 
stacked_rows <- dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) AS n_rows ",
    "FROM read_parquet(",
    stacked_sql,
    ")"
  )
) |>
  pull(n_rows)

cat(
  "\nRows:",
  stacked_rows,
  "\nColumns:",
  length(stacked_columns),
  "\n"
)

#### Desired Column Order #### 

column_order <- c(
  
  # event timing
  "apprehension_datetime",
  "apprehension_date",
  "apprehension_time",
  "arrest_datetime",
  "arrest_date",
  "arrest_time",
  "entry_date",
  
  # encounter / entry history
  "earliest_apprehension_datetime",
  "earliest_encounter_datetime",
  "most_recent_encounter_datetime",
  "most_recent_encounter_date",
  "most_recent_prior_entry_datetime",
  "most_recent_prior_entry_date",
  "most_recent_prior_entry_time",
  "number_of_previous_apprehensions",
  
  # case / custody timing
  "case_file_date",
  "final_bookout_datetime",
  "final_bookout_date",
  
  # record / person identifiers
  "event_number",
  "case_id",
  "subject_key",
  "unique_person_id",
  "subject_afile_number",
  "alien_number",
  
  # event counts
  "number_of_people_in_event",
  "number_of_children_in_event",
  
  # location / arrest information
  "border",
  "arrest_sector",
  "bookout_sector",
  "arrest_state",
  "state",
  "latitude",
  "arrest_landmark_desc",
  "landmark_withheld_indicator",
  "arrest_at_checkpoint_indicator",
  "arrest_method",
  "agent_hash_id",
  "arresting_agent_hash_number",
  
  # demographic information
  "age",
  "adult_or_juvenile",
  "gender",
  "subject_group_classification",
  "marital_status",
  "marital_status_cd",
  "ethnicity",
  "ethnicity_cd",
  "language",
  "birth_date",
  "city_of_birth",
  "state_of_birth",
  "country_of_birth",
  "country_of_birth_cd",
  "citizenship",
  "citizenship_cd",
  "nationality",
  
  # residence information
  "city_state_of_residence",
  "country_of_residence",
  "country_of_residence_cd",
  "first_country_of_residence",
  
  # family / child information
  "fmu_number",
  "fmu_type",
  "fmua_indication",
  "number_of_children_and_nationality",
  "juvenile_18_indicator",
  "unaccompanied_child_indicator",
  
  # immigration / entry status
  "entry_status",
  "entry_status_cd",
  "dhs_status_code_lpr",
  "immigration_status_code_lpr",
  "lpr",
  "time_in_us",
  
  # processing programs / indicators
  "general_processing_code",
  "cds_program",
  "ces_indicator",
  "mpp_indicator",
  "spp_program",
  "credible_fear_indicator",
  
  # disposition / removal
  "disposition",
  "disposition_code_lpr",
  "removal_type",
  
  # custody transfer
  "transfer_to",
  "transfer_to_group",
  
  # prosecution / referrals
  "subject_prosecution_indicator",
  "referred_prosecution",
  "referred_for_prosecution_under_8usc1325_or_8usc1326",
  
  # charges / criminal history
  "statute_charge",
  "highest_statute_charge",
  "charge_code",
  "criminal_conviction_indicator",
  "ncic_charge_code",
  "ncic_charge_code_owned_by_doj",
  "ncic_description",
  "ncic_desc_owned_by_doj",
  
  # gang information
  "suspected_gang_member_indicator",
  "gang_code",
  "gang_name",
  
  # smuggling / seizures
  "smuggled_cost",
  "currency_seized_indicator",
  "currency_seized_value",
  "drugs_seized_indicator",
  "type_of_drugs_seized",
  
  # source information
  "source_file",
  "source_sheet"
)

# missing cols?
missing_columns <- setdiff(
  column_order,
  stacked_columns
)

if (length(missing_columns) > 0) {
  warning(
    "These desired columns are missing from the stacked dataset: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

remaining_columns <- setdiff(
  stacked_columns,
  column_order
)

if (length(remaining_columns) > 0) {
  warning(
    "These stacked columns are not included in column_order: ",
    paste(
      remaining_columns,
      collapse = ", "
    )
  )
}

# desired columns first, remaining columns last
final_columns <- c(
  intersect(
    column_order,
    stacked_columns
  ),
  remaining_columns
)

#### Standardize Redaction Codes #### 
# helpers for safely constructing SQL
sql_identifier <- function(x) {
  as.character(
    dbQuoteIdentifier(
      con,
      x
    )
  )
}

sql_string <- function(x) {
  as.character(
    dbQuoteString(
      con,
      x
    )
  )
}

# SQL redaction / whitespace patterns 
redaction_e_pattern <- "\\(b\\)\\s*\\(7\\)\\s*\\(\\s*E\\s*\\)"
redaction_c_pattern <- "\\(b\\)\\s*\\(6\\)\\s*\\(b\\)\\s*\\(7\\)\\s*\\(\\s*C\\s*\\)"
whitespace_pattern <- "\\s+"

redaction_e_pattern_sql <- sql_string(redaction_e_pattern)
redaction_c_pattern_sql <- sql_string(redaction_c_pattern)
whitespace_pattern_sql <- sql_string(whitespace_pattern)

# standardize redaction code and whitespace
clean_string_sql <- function(column) {
  
  column_sql <- sql_identifier(
    column
  )
  
  sprintf(
    paste0(
      "NULLIF(",
      "TRIM(",
      "regexp_replace(",
      "regexp_replace(",
      "regexp_replace(",
      "CAST(%s AS VARCHAR), ",
      "%s, '(b)(7)(E)', 'gi'",
      "), ",
      "%s, '(b)(6)(b)(7)(C)', 'gi'",
      "), ",
      "%s, ' ', 'g'",
      ")",
      "), ",
      "''",
      ")"
    ),
    column_sql,
    redaction_e_pattern_sql,
    redaction_c_pattern_sql,
    whitespace_pattern_sql
  )
}

#### Inspect Date and Time Cols #### 

datetime_columns <- c(
  "apprehension_datetime",
  "arrest_datetime",
  "earliest_apprehension_datetime",
  "earliest_encounter_datetime",
  "final_bookout_datetime",
  "most_recent_encounter_datetime",
  "most_recent_prior_entry_datetime"
)

date_columns <- c(
  "apprehension_date",
  "arrest_date",
  "birth_date",
  # "case_file_date", multiple so leave out for now
  "entry_date",
  "final_bookout_date",
  "most_recent_encounter_date",
  "most_recent_prior_entry_date"
)

time_columns <- c(
  "apprehension_time",
  "arrest_time",
  "most_recent_prior_entry_time"
)

# keep only columns that exist
datetime_columns <- intersect(
  datetime_columns,
  stacked_columns
)

date_columns <- intersect(
  date_columns,
  stacked_columns
)

time_columns <- intersect(
  time_columns,
  stacked_columns
)

# find values that cannot be converted
audit_cast <- function(
    columns,
    sql_type
) {
  
  if (length(columns) == 0) {
    return(
      tibble(
        column = character(),
        raw_value = character(),
        n = integer()
      )
    )
  }
  
  queries <- map_chr(
    columns,
    \(column) {
      
      value_sql <- clean_string_sql(
        column
      )
      
      sprintf(
        paste0(
          "SELECT ",
          "%s AS column, ",
          "%s AS raw_value, ",
          "COUNT(*) AS n ",
          "FROM read_parquet(%s) ",
          "WHERE %s IS NOT NULL ",
          "AND UPPER(%s) NOT IN ('NA', 'N/A', 'NULL') ",
          "AND TRY_CAST(%s AS %s) IS NULL ",
          "GROUP BY %s"
        ),
        sql_string(column),
        value_sql,
        stacked_sql,
        value_sql,
        value_sql,
        value_sql,
        sql_type,
        value_sql
      )
    }
  )
  
  dbGetQuery(
    con,
    paste(
      queries,
      collapse = "\nUNION ALL\n"
    )
  ) |>
    as_tibble() |>
    arrange(
      desc(n)
    )
}

# inspect unrecognized datetime values
datetime_unrecognized <- map_dfr(
  datetime_columns,
  \(column) {
    
    value_sql <- clean_string_sql(
      column
    )
    
    dbGetQuery(
      con,
      sprintf(
        paste0(
          "SELECT ",
          "%s AS column, ",
          "%s AS raw_value, ",
          "COUNT(*) AS n ",
          "FROM read_parquet(%s) ",
          "WHERE %s IS NOT NULL ",
          "AND UPPER(%s) NOT IN ('NA', 'N/A', 'NULL') ",
          "AND TRY_CAST(%s AS TIMESTAMP) IS NULL ",
          "AND TRY_CAST(%s AS DOUBLE) IS NULL ",
          "GROUP BY %s"
        ),
        sql_string(column),
        value_sql,
        stacked_sql,
        value_sql,
        value_sql,
        value_sql,
        value_sql,
        value_sql
      )
    )
  }
)

datetime_unrecognized

# inspect unrecognized date values
date_unrecognized <- map_dfr(
  date_columns,
  \(column) {
    
    value_sql <- clean_string_sql(
      column
    )
    
    dbGetQuery(
      con,
      sprintf(
        paste0(
          "SELECT ",
          "%s AS column, ",
          "%s AS raw_value, ",
          "COUNT(*) AS n ",
          "FROM read_parquet(%s) ",
          "WHERE %s IS NOT NULL ",
          "AND UPPER(%s) NOT IN ('NA', 'N/A', 'NULL') ",
          "AND TRY_CAST(%s AS DATE) IS NULL ",
          "AND TRY_STRPTIME(%s, '%%m/%%d/%%Y') IS NULL ",
          "AND TRY_CAST(%s AS DOUBLE) IS NULL ",
          "GROUP BY %s"
        ),
        sql_string(column),
        value_sql,
        stacked_sql,
        value_sql,
        value_sql,
        value_sql,
        value_sql,
        value_sql,
        value_sql
      )
    )
  }
)

date_unrecognized

# inspect unrecognized time values
time_unrecognized <- map_dfr(
  time_columns,
  \(column) {
    
    value_sql <- clean_string_sql(
      column
    )
    
    dbGetQuery(
      con,
      sprintf(
        paste0(
          "SELECT ",
          "%s AS column, ",
          "%s AS raw_value, ",
          "COUNT(*) AS n ",
          "FROM read_parquet(%s) ",
          "WHERE %s IS NOT NULL ",
          "AND UPPER(%s) NOT IN ('NA', 'N/A', 'NULL') ",
          "AND TRY_CAST(%s AS TIME) IS NULL ",
          "AND TRY_CAST(%s AS DOUBLE) IS NULL ",
          "GROUP BY %s"
        ),
        sql_string(column),
        value_sql,
        stacked_sql,
        value_sql,
        value_sql,
        value_sql,
        value_sql,
        value_sql
      )
    )
  }
)

time_unrecognized

#### Inspect Logical Cols #### 

logical_columns <- c(
  "arrest_sl_checkpoint_indicator",
  "ces_indicator",
  "credible_fear_indicator",
  "criminal_conviction_indicator",
  "currency_seized_indicator",
  "drugs_seized_indicator",
  "juvenile_18_indicator",
  "mpp_indicator",
  "landmark_withheld_indicator",
  "suspected_gang_member_indicator",
  "subject_prosecution_indicator",
  "unaccompanied_child_indicator"
)

logical_columns <- intersect(
  logical_columns,
  stacked_columns
)

true_values <- c(
  "Y",
  "YES",
  "TRUE",
  "T",
  "X",
  "1"
)

false_values <- c(
  "N",
  "NO",
  "FALSE",
  "F",
  "0"
)

missing_values <- c(
  "NA",
  "N/A",
  "NULL"
)

valid_logical_values <- c(
  true_values,
  false_values,
  missing_values
)

valid_logical_sql <- paste(
  sql_string(
    valid_logical_values
  ),
  collapse = ", "
)

true_values_sql <- paste(
  sql_string(
    true_values
  ),
  collapse = ", "
)

false_values_sql <- paste(
  sql_string(
    false_values
  ),
  collapse = ", "
)

# check before converting
if (length(logical_columns) > 0) {
  
  logical_queries <- map_chr(
    logical_columns,
    \(column) {
      
      value_sql <- clean_string_sql(
        column
      )
      
      sprintf(
        paste0(
          "SELECT ",
          "%s AS column, ",
          "%s AS raw_value, ",
          "COUNT(*) AS n ",
          "FROM read_parquet(%s) ",
          "WHERE %s IS NOT NULL ",
          "AND UPPER(%s) NOT IN (%s) ",
          "GROUP BY %s"
        ),
        sql_string(column),
        value_sql,
        stacked_sql,
        value_sql,
        value_sql,
        valid_logical_sql,
        value_sql
      )
    }
  )
  
  logical_unrecognized <- dbGetQuery(
    con,
    paste(
      logical_queries,
      collapse = "\nUNION ALL\n"
    )
  ) |>
    as_tibble() |>
    arrange(
      desc(n)
    )
  
} else {
  
  logical_unrecognized <- tibble(
    column = character(),
    raw_value = character(),
    n = integer()
  )
}

logical_unrecognized

#### Inspect Numeric Cols #### 

integer_columns <- c(
  "age",
  "number_of_children_in_event",
  "number_of_people_in_event",
  "number_of_previous_apprehensions"
)

double_columns <- c(
  "currency_seized_value",
  "latitude",
  "smuggled_cost"
)

integer_columns <- intersect(
  integer_columns,
  stacked_columns
)

double_columns <- intersect(
  double_columns,
  stacked_columns
)

# check before converting
integer_unrecognized <- audit_cast(
  integer_columns,
  "INTEGER"
)

double_unrecognized <- audit_cast(
  double_columns,
  "DOUBLE"
)

numeric_unrecognized <- bind_rows(
  integer_unrecognized,
  double_unrecognized
) |>
  arrange(
    desc(n)
  )

numeric_unrecognized

#### Redaction Flag #### 

# redaction flag: cols whose intended types converted redacted codes to NULL
redaction_flag_columns <- intersect(
  c(
    "birth_date",
    "latitude",
    "suspected_gang_member_indicator"
  ),
  stacked_columns
)

# detect any FOIA exemption marker beginning with (b)(number)
redaction_marker_pattern_sql <- sql_string(
  "\\(b\\)\\s*\\(\\s*[0-9]+\\s*\\)"
)

# build flag to distinguish redacted from true missing 
redaction_flag_expression <- function(column) {
  sprintf(
    paste0(
      "COALESCE(",
      "regexp_matches(CAST(%s AS VARCHAR), %s, 'i'), ",
      "FALSE",
      ") AS %s"
    ),
    sql_identifier(column),
    redaction_marker_pattern_sql,
    sql_identifier(
      paste0(column, "_redacted")
    )
  )
}

#### Column Conversion #### 

# convert each column
final_expression <- function(column) {
  
  column_sql <- sql_identifier(
    column
  )
  
  value_sql <- clean_string_sql(
    column
  )
  
  # datetime
  if (column %in% datetime_columns) {
    
    return(
      sprintf(
        paste0(
          "CASE ",
          "WHEN TRY_CAST(%1$s AS TIMESTAMP) IS NOT NULL ",
          "THEN TRY_CAST(%1$s AS TIMESTAMP) ",
          "WHEN TRY_CAST(%1$s AS DOUBLE) IS NOT NULL ",
          "THEN TIMESTAMP '1899-12-30' + ",
          "to_microseconds(",
          "CAST(",
          "ROUND(",
          "TRY_CAST(%1$s AS DOUBLE) ",
          "* 86400000000",
          ") ",
          "AS BIGINT",
          ")",
          ") ",
          "ELSE NULL ",
          "END AS %2$s"
        ),
        value_sql,
        column_sql
      )
    )
  }
  
  # date
  if (column %in% date_columns) {
    
    return(
      sprintf(
        paste0(
          "CASE ",
          "WHEN TRY_CAST(%1$s AS DATE) IS NOT NULL ",
          "THEN TRY_CAST(%1$s AS DATE) ",
          "WHEN TRY_STRPTIME(%1$s, '%%m/%%d/%%Y') IS NOT NULL ",
          "THEN CAST(TRY_STRPTIME(%1$s, '%%m/%%d/%%Y') AS DATE) ",
          "WHEN TRY_CAST(%1$s AS DOUBLE) IS NOT NULL ",
          "THEN DATE '1899-12-30' + ",
          "CAST(",
          "TRUNC(",
          "TRY_CAST(%1$s AS DOUBLE)",
          ") ",
          "AS INTEGER",
          ") ",
          "ELSE NULL ",
          "END AS %2$s"
        ),
        value_sql,
        column_sql
      )
    )
  }
  
  # time
  if (column %in% time_columns) {
    
    return(
      sprintf(
        paste0(
          "CASE ",
          "WHEN TRY_CAST(%1$s AS TIME) IS NOT NULL ",
          "THEN TRY_CAST(%1$s AS TIME) ",
          "WHEN TRY_CAST(%1$s AS DOUBLE) IS NOT NULL ",
          "THEN CAST(",
          "TIME '00:00:00' + ",
          "to_microseconds(",
          "CAST(",
          "ROUND(",
          "(",
          "TRY_CAST(%1$s AS DOUBLE) - ",
          "FLOOR(TRY_CAST(%1$s AS DOUBLE))",
          ") * 86400000000",
          ") ",
          "AS BIGINT",
          ")",
          ") ",
          "AS TIME",
          ") ",
          "ELSE NULL ",
          "END AS %2$s"
        ),
        value_sql,
        column_sql
      )
    )
  }
  
  # logical
  if (column %in% logical_columns) {
    
    return(
      sprintf(
        paste0(
          "CASE ",
          "WHEN UPPER(%1$s) IN (%2$s) ",
          "THEN TRUE ",
          "WHEN UPPER(%1$s) IN (%3$s) ",
          "THEN FALSE ",
          "ELSE NULL ",
          "END AS %4$s"
        ),
        value_sql,
        true_values_sql,
        false_values_sql,
        column_sql
      )
    )
  }
  
  # integer
  if (column %in% integer_columns) {
    
    return(
      sprintf(
        "TRY_CAST(%s AS INTEGER) AS %s",
        value_sql,
        column_sql
      )
    )
  }
  
  # double
  if (column %in% double_columns) {
    
    return(
      sprintf(
        "TRY_CAST(%s AS DOUBLE) AS %s",
        value_sql,
        column_sql
      )
    )
  }
  
  # everything else becomes character
  sprintf(
    "%s AS %s",
    value_sql,
    column_sql
  )
}

final_select <- map(
  final_columns,
  \(column) {
    expressions <- final_expression(column)
    
    if (column %in% redaction_flag_columns) {
      expressions <- c(
        expressions,
        redaction_flag_expression(column)
      )
    }
    
    expressions
  }
) |>
  flatten_chr() |>
  paste(
    collapse = ",\n    "
  )


#### Consolidate Code Columns #### 

# access code map in SQL
code_map <- dbGetQuery(
  con,
  sprintf(
    "SELECT field, code, full_name FROM read_parquet(%s)",
    sql_string(code_map_path)
  )
) |>
  as_tibble()

# check for blank entries in map
if (
  anyNA(code_map) ||
  any(str_squish(unlist(code_map, use.names = FALSE)) == "")
) {
  stop("The code map contains missing or blank entries.")
}

# keep full names as they are 
lookup <- bind_rows(
  code_map,
  code_map |>
    transmute(
      field,
      code = full_name,
      full_name
    )
) |>
  mutate(
    code = str_squish(code),
    full_name = str_squish(full_name)
  ) |>
  distinct()

dbWriteTable(
  con,
  "code_lookup_raw",
  lookup,
  temporary = TRUE,
  overwrite = TRUE
)

# apply capitalization rule
dbExecute(
  con,
  "CREATE OR REPLACE TEMP VIEW code_lookup AS
   SELECT DISTINCT
     field,
     UPPER(code) AS code_key,
     full_name
   FROM code_lookup_raw"
)

# one translation early per map entry
mapping_conflicts <- dbGetQuery(
  con,
  "SELECT field, code_key, COUNT(*) AS n
   FROM code_lookup
   GROUP BY field, code_key
   HAVING COUNT(*) > 1"
)

if (nrow(mapping_conflicts) > 0) {
  print(mapping_conflicts)
  stop("Resolve conflicting code mappings before continuing.")
}

# use existing cleaning query 
dbExecute(
  con,
  sprintf(
    "CREATE OR REPLACE TEMP VIEW cleaned_input AS
     SELECT %s
     FROM read_parquet(%s)",
    final_select,
    stacked_sql
  )
)

cleaned_columns <- dbGetQuery(
  con,
  "DESCRIBE cleaned_input"
)$column_name

# column pairing
code_pairs <- list(
  citizenship = "citizenship_cd",
  country_of_birth = "country_of_birth_cd",
  country_of_residence = "country_of_residence_cd",
  marital_status = "marital_status_cd",
  entry_status = "entry_status_cd",
  ethnicity = "ethnicity_cd"
  )

# retain existing code columns only 
code_pairs <- map(
  code_pairs,
  ~ intersect(.x, cleaned_columns)
)

# remove mapped code cols from output
output_columns <- setdiff(
  cleaned_columns,
  unlist(code_pairs, use.names = FALSE)
)

# destination only if code col exists
needed_full_columns <- names(code_pairs)[lengths(code_pairs) > 0]

output_columns <- union(
  output_columns,
  needed_full_columns
)

# source info
source_columns <- intersect(
  c("source_file", "source_sheet"),
  output_columns
)

output_columns <- c(
  setdiff(output_columns, source_columns),
  source_columns
)

# build lookup joins and output expressions
lookup_joins <- character()
output_expressions <- character()


for (column in output_columns) {
  
  # regular columns pass through unchanged
  if (!column %in% names(code_pairs)) {
    output_expressions <- c(
      output_expressions,
      paste0("s.", sql_identifier(column))
    )
    
    next
  }
  
  # full name cols first
  candidates <- c(
    intersect(column, cleaned_columns),
    code_pairs[[column]]
  )
  
  translated_values <- character()
  
  for (candidate in candidates) {
    
    lookup_alias <- paste0(
      "lookup_",
      length(lookup_joins) + 1
    )
    
    value_sql <- sprintf(
      "NULLIF(TRIM(CAST(s.%s AS VARCHAR)), '')",
      sql_identifier(candidate)
    )
    
    # match value to translation
    lookup_joins <- c(
      lookup_joins,
      sprintf(
        paste0(
          "LEFT JOIN ",
          "(SELECT code_key, full_name ",
          "FROM code_lookup WHERE field = %s) AS %s ",
          "ON %s.code_key = UPPER(%s)"
        ),
        sql_string(column),
        lookup_alias,
        lookup_alias,
        value_sql
      )
    )
    
    # use translation when found, otherwise leave as is
    translated_values <- c(
      translated_values,
      sprintf(
        "COALESCE(%s.full_name, %s)",
        lookup_alias,
        value_sql
      )
    )
  }
  
  # prioritize cleaned full name
  combined_sql <- if (length(translated_values) == 1) {
    translated_values[[1]]
  } else {
    paste0(
      "COALESCE(",
      paste(translated_values, collapse = ", "),
      ")"
    )
  }
  
  output_expressions <- c(
    output_expressions,
    sprintf(
      "%s AS %s",
      combined_sql,
      sql_identifier(column)
    )
  )
}

consolidated_select <- paste(
  output_expressions,
  collapse = ",\n    "
)

lookup_joins_sql <- paste(
  lookup_joins,
  collapse = "\n  "
)

# write final parquet
final_query <- sprintf(
  paste0(
    "COPY (",
    "\n  SELECT",
    "\n    %s",
    "\n  FROM cleaned_input s",
    "\n  %s",
    "\n)",
    "\nTO %s (",
    "\n  FORMAT parquet,",
    "\n  COMPRESSION snappy,",
    "\n  USE_TMP_FILE true",
    "\n)"
  ),
  consolidated_select,
  lookup_joins_sql,
  final_sql
)

dbExecute(
  con,
  final_query
)

cat(
  "\nCleaned dataset saved to:",
  apprehensions_cleaned_path,
  "\n"
)

#### Final Col Types #### 

final_schema <- dbGetQuery(
  con,
  paste0(
    "DESCRIBE SELECT * ",
    "FROM read_parquet(",
    final_sql,
    ")"
  )
) |>
  as_tibble()

column_types <- final_schema |>
  transmute(
    column = column_name,
    type = column_type
  )

print(
  column_types,
  n = Inf
)

# how many cols of each type?
column_types |>
  summarise(
    character_columns = sum(
      type == "VARCHAR"
    ),
    logical_columns = sum(
      type == "BOOLEAN"
    ),
    integer_columns = sum(
      type == "INTEGER"
    ),
    double_columns = sum(
      type == "DOUBLE"
    ),
    datetime_columns = sum(
      type == "TIMESTAMP"
    ),
    date_columns = sum(
      type == "DATE"
    ),
    time_columns = sum(
      type == "TIME"
    )
  ) |>
  print()


# disconnect 
dbDisconnect(
  con,
  shutdown = TRUE
)
