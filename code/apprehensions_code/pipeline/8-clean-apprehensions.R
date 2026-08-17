# load packages
library(tidyverse)
library(DBI)
library(duckdb)

# paths
processed_dir <- "data/apprehensions/processed"

stacked_path <- file.path(
  processed_dir,
  "apprehensions_stacked.parquet"
)

final_path <- file.path(
  processed_dir,
  "apprehensions_final.parquet"
)

 
# connect to duckDB
con <- dbConnect(
  duckdb()
)

dbExecute(
  con,
  "SET threads = 2"
)

dbExecute(
  con,
  "SET preserve_insertion_order = false"
)

# quote file paths for SQL
stacked_sql <- as.character(
  dbQuoteString(
    con,
    stacked_path
  )
)

final_sql <- as.character(
  dbQuoteString(
    con,
    final_path
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

# preview first 5 rows
stacked_preview <- dbGetQuery(
  con,
  paste0(
    "SELECT * ",
    "FROM read_parquet(",
    stacked_sql,
    ") ",
    "LIMIT 5"
  )
)

glimpse(
  stacked_preview
)

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
  "earliest_app_date_time",
  "most_recent_encounter_date",
  "most_recent_prior_entry_datetime",
  "most_recent_prior_entry_date",
  "most_recent_prior_entry_time",
  "entry_date",
  "case_file_date",
  "final_bookout_date",
  
  # record identifiers
  "event_number",
  "case_id",
  "unique_case_identifier",
  "subject_key",
  "person_id",
  "subject_afile_number",
  
  # counts
  "number_of_children_in_event",
  "number_of_people_per_event",
  "number_of_previous_apprehensions",
  "number_of_subjects_per_event",
  
  # location / arrest information
  "arrest_border",
  "arrest_sector",
  "sector_district",
  "sector_of_bookout",
  "state",
  "latitude",
  "landmark",
  "landmark_withheld_indicator",
  "arrest_sl_checkpoint_indicator",
  "arrest_method",
  "arresting_agent_hash_number",
  
  # demographic information
  "age",
  "adult_or_juvenile",
  "gender",
  "subject_group_classification",
  "marital_status",
  "ethnicity",
  "language",
  "birth_date",
  "birth_city",
  "birth_state",
  "country_of_birth",
  "country_of_birth_cd",
  "citizenship",
  "citizenship_cd",
  "nationality",
  
  # residence information
  "first_country_of_residence",
  "first_country_of_residence_foreign",
  "country_of_res_cd",
  
  # family / child information
  "fmu_number",
  "fmu_type",
  "fmua_indicator",
  "number_of_children_and_nationality",
  "juvenile_18_indicator",
  "unaccompanied_child_indicator",
  
  # immigration / entry information
  "entry_status",
  "status_at_entry_cd",
  "dhs_status_code_lpr",
  "entry_status_includes_lpr",
  "lpr",
  "time_in_us",
  "cds_program",
  "general_processing_code",
  "ces_indicator",
  "mpp_indicator",
  "spp_program",
  "credible_fear_indicator",
  
  # event / disposition information
  "disposition",
  "removal_type",
  
  # custody transfer
  "transfer_to",
  "transfer_to_group",
  
  # prosecution / charges
  "prosecution_indicator",
  "statute_charge",
  "highest_statute_charge",
  "charge_code",
  "criminal_conviction_indicator",
  "ncic_charge_code",
  "ncic_charge_code_defer_to_doj",
  "ncic_charge_code_owned_by_doj_not_cbp",
  "ncic_description",
  "ncic_desc_defer_to_doj",
  "ncic_desc_owned_by_doj_not_cbp",
  
  # gang information
  "suspected_gang_member_indicator",
  "gang_code",
  "gang_name",
  
  # smuggling / seizures
  "smuggled_cost",
  "currency_seized_during_app_indicator",
  "currency_seized_during_app_value",
  "drugs_seized_during_apprehension_indicator",
  "type_of_drugs_seized_during_apprehension",
  
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

redaction_pattern <- "\\(b\\)\\s*\\(7\\)\\s*\\(\\s*E\\s*\\)"

whitespace_pattern <- "\\s+"

redaction_pattern_sql <- sql_string(
  redaction_pattern
)

whitespace_pattern_sql <- sql_string(
  whitespace_pattern
)

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
      "CAST(%s AS VARCHAR), ",
      "%s, ",
      "'(b)(7)(E)', ",
      "'gi'",
      "), ",
      "%s, ",
      "' ', ",
      "'g'",
      ")",
      "), ",
      "''",
      ")"
    ),
    column_sql,
    redaction_pattern_sql,
    whitespace_pattern_sql
  )
}

#### Handle Date and Time Cols #### 

datetime_columns <- c(
  "apprehension_datetime",
  "arrest_datetime",
  "earliest_app_date_time",
  "most_recent_prior_entry_datetime"
)

date_columns <- c(
  "apprehension_date",
  "arrest_date",
  "birth_date",
  "case_file_date",
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

#### Handle Logical Cols #### 

logical_columns <- c(
  "arrest_sl_checkpoint_indicator",
  "ces_indicator",
  "credible_fear_indicator",
  "criminal_conviction_indicator",
  "currency_seized_during_app_indicator",
  "drugs_seized_during_apprehension_indicator",
  "juvenile_18_indicator",
  "landmark_withheld_indicator",
  "suspected_gang_member_indicator",
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

#### Handle Numeric Cols #### 

integer_columns <- c(
  "age",
  "number_of_children_in_event",
  "number_of_people_per_event",
  "number_of_previous_apprehensions",
  "number_of_subjects_per_event"
)

double_columns <- c(
  "latitude",
  "smuggled_cost",
  "currency_seized_during_app_value"
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

#### Convert Rest to Chr #### 

protected_columns <- c(
  datetime_columns,
  date_columns,
  time_columns,
  logical_columns,
  integer_columns,
  double_columns
)

character_columns <- setdiff(
  stacked_columns,
  protected_columns
)

#### Final Dataset #### 

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

# typed columns whose redaction markers need to be preserved
# in a companion logical column
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

# construct correct conversion for each column
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


# write final parquet 
final_query <- sprintf(
  paste0(
    "COPY (",
    "\n  SELECT",
    "\n    %s",
    "\n  FROM read_parquet(%s)",
    "\n)",
    "\nTO %s (",
    "\n  FORMAT parquet,",
    "\n  COMPRESSION snappy,",
    "\n  USE_TMP_FILE true",
    "\n)"
  ),
  final_select,
  stacked_sql,
  final_sql
)

dbExecute(
  con,
  final_query
)

cat(
  "\nFinal dataset saved to:",
  final_path,
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
