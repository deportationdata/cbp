# use UTC for source clock times, source time zone is unspecified
Sys.setenv(TZ = "UTC")

# load packages
library(tidyverse)
library(DBI)
library(duckdb)

# paths
dataset_dir <- "data/inadmissibles"
processed_dir <- file.path(dataset_dir, "processed")

inadmissibles_stacked_path <- file.path(
  processed_dir,
  "inadmissibles-stacked.parquet"
)

inadmissibles_cleaned_path <- file.path(
  processed_dir,
  "inadmissibles-cleaned.parquet"
)

code_map_path <- file.path(dataset_dir, "metadata", "code-map.parquet")

# connect to duckDB
con <- dbConnect(
  duckdb()
)

# use UTC for database timestamps
dbExecute(con, "SET TimeZone = \'UTC\'")

# configuration
dbExecute(con, "SET threads = 1")
dbExecute(con, "SET preserve_insertion_order = false")
dbExecute(con, "SET max_temp_directory_size = '10GiB'")

# SQL file paths
stacked_sql <- as.character(
  dbQuoteString(
    con,
    inadmissibles_stacked_path
  )
)

final_sql <- as.character(
  dbQuoteString(
    con,
    inadmissibles_cleaned_path
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
  "event_datetime",
  "calendar_month_year",
  "calendar_year",
  "calendar_month_name",
  "final_bookout_datetime",
  
  # counts
  "event_subject_count",
  "inadmissibles_subject_count",
  "person_count",
  
  # record identifiers
  "event_id",
  "unique_person_id",
  "subject_afile_number",
  
  # location
  "field_office_name",
  "city",
  "state",
  "usa_state_code",
  "country",
  "street_name",
  "postal_zip_code",
  "north_south_indicator",
  
  # transportation
  "mode_of_transportation",
  
  # demographic information
  "age",
  "gender",
  "marital_status",
  "birth_country",
  "citizenship",
  
  # family / child information
  "fmua_indicator",
  "fmua_uac_indicator",
  
  # immigration / entry information
  "entry_status",
  "us_destination_address",
  "credible_fear_indicator",
  
  # event / disposition information
  "event_reason",
  "disposition",
  "disposition_name",
  "duplicate_subject_indicator",
  
  # custody transfer
  "transfer_to_group",
  
  # prosecution / charges
  "presented_to_ausa",
  "statute_charge",
  
  # gang information
  "suspected_gang_affiliation",
  "gang_affiliation_comments",
  
  # record administration
  "created_by_hash_id",
  "approving_supervisor_hash_id",
  
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

#### Standardize Values #### 
## standardize redaction codes 

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

redaction_b6_pattern <- "\\(\\s*b\\s*\\)\\s*\\(\\s*6\\s*\\)"
redaction_b7c_pattern <- "\\(\\s*b\\s*\\)\\s*\\(\\s*7\\s*\\)\\s*\\(\\s*C\\s*\\)"
redaction_b7e_pattern <- "\\(\\s*b\\s*\\)\\s*\\(\\s*7\\s*\\)\\s*\\(\\s*E\\s*\\)"

# boundary between one complete code and the next (b)
redaction_separator_pattern <- "\\)\\s*[,;]*\\s*\\(b\\)"

whitespace_pattern <- "\\s+"

# standardize redaction codes, separators, and whitespace

clean_string_sql <- function(column) {
  
  value_sql <- sprintf(
    "CAST(%s AS VARCHAR)",
    sql_identifier(column)
  )
  
  # standardize each distinct code
  patterns <- c(
    redaction_b6_pattern,
    redaction_b7c_pattern,
    redaction_b7e_pattern
  )
  
  replacements <- c(
    "(b)(6)",
    "(b)(7)(C)",
    "(b)(7)(E)"
  )
  
  for (i in seq_along(patterns)) {
    value_sql <- sprintf(
      "regexp_replace(%s, %s, %s, 'gi')",
      value_sql,
      sql_string(patterns[i]),
      sql_string(replacements[i])
    )
  }
  
  # separate adjacent codes with a comma and space
  value_sql <- sprintf(
    "regexp_replace(%s, %s, %s, 'g')",
    value_sql,
    sql_string(redaction_separator_pattern),
    sql_string("), (b)")
  )
  
  # collapse whitespace, trim, and convert empty strings to NULL
  sprintf(
    "NULLIF(TRIM(regexp_replace(%s, %s, ' ', 'g')), '')",
    value_sql,
    sql_string(whitespace_pattern)
  )
}

## standardize statute charge

#### Inspect Date and Time Cols #### 

datetime_columns <- c(
  "event_created_datetime",
  "final_bookout_datetime"
)

date_columns <- c(
)

time_columns <- c(
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

#### Handle Logical Cols #### 

logical_columns <- c(
  "credible_fear_indicator",
  "fmua_indicator",
  "duplicate_subject_indicator"
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
      
      value_sql <- clean_string_sql(column)
      
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
  "event_subject_count"
)

double_columns <- c(
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

code_map <- dbGetQuery(
  con,
  sprintf(
    "SELECT field, code, full_name FROM read_parquet(%s)",
    sql_string(code_map_path)
  )
) |>
  as_tibble()

if (
  anyNA(code_map) ||
  any(str_squish(unlist(code_map, use.names = FALSE)) == "")
) {
  stop("The code map contains missing or blank entries.")
}

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

dbExecute(
  con,
  "CREATE OR REPLACE TEMP VIEW code_lookup AS
   SELECT DISTINCT
     field,
     UPPER(code) AS code_key,
     full_name
   FROM code_lookup_raw"
)

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

# Compile the small lookup into scalar expressions so translation can stream
# without joining or buffering the full input dataset.
lookup_values <- dbGetQuery(con, "SELECT * FROM code_lookup")

translate_value_sql <- function(field_name, value_sql) {
  entries <- lookup_values[lookup_values$field == field_name, , drop = FALSE]
  if (nrow(entries) == 0) return(value_sql)

  branches <- sprintf(
    "WHEN %s THEN %s",
    sql_string(entries$code_key),
    sql_string(entries$full_name)
  )
  sprintf(
    "CASE UPPER(%s) %s ELSE %s END",
    value_sql,
    paste(branches, collapse = " "),
    value_sql
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

  if (column == "duplicate_subject_indicator") {
    value_sql <- translate_value_sql(column, value_sql)
  }
  
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

final_select <- map_chr(
  final_columns,
  final_expression
) |>
  paste(collapse = ",\n    ")


#### Consolidate Code Columns #### 

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
  "DESCRIBE SELECT * FROM cleaned_input"
)$column_name

#### Code map pairs #### 
code_pairs <- list(
  birth_country = character(),
  citizenship = character(),
  country = character(),
  state = character(),
  calendar_year = character(),
  fmua_uac_indicator = character(),
  gender = character(),
  transfer_to_group = character()
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

output_expressions <- character()

for (column in output_columns) {
  
  # store source clock times with an explicit UTC label
  if (column %in% datetime_columns) {
    output_expressions <- c(
      output_expressions,
      sprintf(
        "CAST(CAST(s.%s AS VARCHAR) || '+00:00' AS TIMESTAMPTZ) AS %s",
        sql_identifier(column),
        sql_identifier(column)
      )
    )
    next
  }
  
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
    
    value_sql <- sprintf(
      "NULLIF(TRIM(CAST(s.%s AS VARCHAR)), '')",
      sql_identifier(candidate)
    )
    
    # use translation when found, otherwise leave as is
    translated_values <- c(
      translated_values,
      translate_value_sql(column, value_sql)
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
  
  # uppercase citizenship after translating and combining values
  if (column == "citizenship") {
    combined_sql <- sprintf("UPPER(%s)", combined_sql)
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

# write final parquet
final_query <- sprintf(
  paste0(
    "COPY (",
    "\n  SELECT",
    "\n    %s",
    "\n  FROM cleaned_input s",
    "\n)",
    "\nTO %s (",
    "\n  FORMAT parquet,",
    "\n  COMPRESSION snappy,",
    "\n  USE_TMP_FILE true",
    "\n)"
  ),
  consolidated_select,
  final_sql
)

dbExecute(
  con,
  final_query
)

cat(
  "\nCleaned dataset saved to:",
  inadmissibles_cleaned_path,
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
      type %in% c("TIMESTAMP", "TIMESTAMP WITH TIME ZONE")
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

# END
