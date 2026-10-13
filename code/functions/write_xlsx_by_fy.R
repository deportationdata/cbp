# Writes a data frame to one Excel workbook with a sheet per federal fiscal
# year (Oct 1 - Sep 30) of `date_col`. Rows with a missing date land on a
# trailing "FY unknown" sheet. Excel caps a sheet at 1,048,576 rows including
# the header, so a fiscal year larger than that is spread across numbered
# sheets ("FY2024 (2)", ...).
write_xlsx_by_fy <- function(df, path, date_col, label = NULL) {
  max_rows <- 1048575L

  grouped <-
    df |>
    mutate(
      .fy = as.integer(year({{ date_col }})) +
        as.integer(month({{ date_col }}) >= 10L)
    ) |>
    mutate(.chunk = ceiling(row_number() / max_rows), .by = .fy) |>
    group_by(.fy, .chunk)

  keys <- group_keys(grouped)

  sheets <- group_split(grouped, .keep = FALSE)
  names(sheets) <- str_c(
    if (is.null(label)) "" else str_c(label, " "),
    if_else(is.na(keys$.fy), "FY unknown", str_c("FY", keys$.fy)),
    if_else(keys$.chunk > 1, str_c(" (", keys$.chunk, ")"), "")
  )

  writexl::write_xlsx(sheets, path)
  invisible(names(sheets))
}
