# CBP Data Processing

## Intro

We provide code for collecting and processing three CBP datasets: USBP apprehensions, OFO inadmissibles, and USBP encounters. The encounters data include Title 8 apprehensions and Title 42 expulsions and exclude Title 8 inadmissibles recorded by the Office of Field Operations.

Each pipeline collects files published by CBP under the Freedom of Information Act, standardizes their differences, identifies and removes overlapping coverage, and combines the records into usable datasets. Field definitions and more information about the data can be found in the `cbp-data-codebook.qmd`.

**Note: The OFO inadmissibles pipeline and dataset are still in progress and will be updated upon completion.**

## Running the Pipelines

To reproduce our final datasets, run each type of script in numeric order. Several scripts have functions that require manual inputs and are explained below. Later scripts generally assume that the outputs from earlier scripts already exist.

When CBP publishes new files, run the pipeline again, beginning with the first download script. Review individual script instructions to understand whether later scripts can be run without having run earlier ones. Review all warnings and audit tables before proceeding.

## Repository Organization

In the `cbp` repository, there are three main folders through which you can navigate. Throughout this document, `<dataset>` indicates the relevant dataset type among apprehensions, inadmissibles, or encounters.

-   The `code/` folder contains all project code. This includes:

    -   Code used to create final datasets in `code/<dataset>-code/pipeline/`

    -   Some preliminary analysis in `code/<dataset>-code/analysis/`

    -   Additional code used for general analysis in `code/miscellaneous/`

-   The `data/<dataset>/` folder contains all data extracted through the pipelines in `cbp/data/<dataset>/`, which all follow the same internal organization:

    -   `~/manual-review/`: files that require manual review before inclusion in the final dataset

    -   `~/metadata/`: inventories, crosswalks, metadata, and other information describing source files and processed parts

    -   `~/processed/`: any transformed raw files

    -   `~/raw/`: any raw files collected from CBP to be considered for inclusion in the final dataset

    -   `~/validation/`: validation files and outputs used to compare counts obtained through the pipeline with CBP dashboards

-   The `analysis/` folder contains outputs that are not part of the pipeline, whose code can be found in either `cbp/<dataset>-code/analysis` if the outputs are specific to certain datasets or `cbp/code/miscellaneous/`

### 1. Download Source Files

Scripts: `1-download-apprehensions.R`, `1-download-inadmissibles.R`, and `1-download-encounters.R`.

These scripts scrape the relevant CBP records page, identify downloadable links, differentiate between certain matches and files intended for manual review, and download links that are not already recorded in the existing link inventory.

Outputs:

-   `data/<dataset>/raw/`: contains files that meet certain match criteria
-   `data/<dataset>/manual-review/`: contains ambiguous files meant for manual review
-   `<dataset>-links.parquet`: contains links that the script interacted with and their match classifications in the corresponding `metadata/` directory, along with their href, full URL, and lowercase text

Any new files that appear in `manual-review/` should be manually reviewed before proceeding to the next script. Upon examination, move any files that should be included in the final dataset to `raw/`. The inclusion and exclusion conditions should be reviewed if CBP changes its labels or naming conventions.

Because the link inventory identifies files by URL, the script will not recognize if CBP replaces an existing link with a different file. Thus, existing URLs should be confirmed if revisions are suspected.

### 2. Profile Worksheets and Headers

Scripts: `2-profile-apprehensions.R`, `2-profile-inadmissibles.R`, and `2-profile-encounters.R`.

These scripts identify the likely header row in each worksheet and inventory the columns found in all `raw/` files.

Outputs in `data/<dataset>/metadata/`:

-   `sheet-inventory.parquet`: displays file name, file path, and sheet name for all `raw/` file sheets
-   `column-inventory.parquet`: displays file name, file path, sheet name, header row placement, number of rows skipped to detect header row, number of columns in that sheet, raw column name, clean column name (after applying `janitor::make_clean_names()`, but before canonical standardization), and column position for every column in `raw/`
-   `distinct-columns.parquet`: identifies all unique cleaned columns from `column-inventory.parquet` and displays their clean column and raw column names, number of files each unique column appears in, and example files containing each column
-   `failed-sheets.parquet`: created only when a sheet fails profiling. If this happens, a good place to check is the header row detection logic in the `find_header_row` function.

By default, `force_reprofile <- FALSE`, so only files that are not already represented in the sheet inventory are profiled. Set `force_reprofile <- TRUE` when the profiling rules change, an existing raw file is replaced, or all metadata must be rebuilt. If it is set to `TRUE`, set `force_rebuild <- TRUE` in the later `4-create-parts.R` script. Review `failed-sheets.parquet` whenever it is created.

### 3. Build the Column Crosswalk

Scripts: `3-build-crosswalk.R`

This script maps variant raw headers to canonical column names and writes `data/<dataset>/metadata/crosswalk.parquet`.

Outputs in `data/<dataset>/metadata/`:

-   `crosswalk.parquet`: displays clean column names, raw column names, number of files the clean column can be found in, and canonical names. This script acts as a map, assigning canonical names to each distinct clean column.

The crosswalk definitions are written directly in each script and require manual maintenance. Whenever `distinct-columns.parquet` contains a new raw column, assign it an appropriate canonical name or merge it with its canonical counterpart. By default, this script considers cleaned column names as canonical unless they are specifically renamed or merged. Review the printed missing mappings and collapsed groups to ensure that no raw field was omitted or combined incorrectly. After changing a crosswalk, rerun the script and set `force_rebuild <- TRUE` in the next step.

### 4. Create Standardized Parts

Script: `4-create-parts.R`

This script rereads every successfully profiled worksheet, applies its canonical column names, removes repeated header rows, adds `source_file` and `source_sheet`, and writes one Parquet part to `data/<dataset>/processed/parts-to-stack/` per sheet. "Parts" refers to raw file sheets that have had canonical headers applied.

Outputs:

-   `data/<dataset>/processed/`: contains all data that has been processed in some way (including parts to stack and final datasets), whereas `raw/` contains raw files only
-   `data/<dataset>/processed/parts-to-stack`: contains processed sheets with canonical column names to eventually be stacked to create final datasets

By default `force_rebuild <- FALSE`, so only new sheets will be converted into stackable parts. Set `force_rebuild <- TRUE` if:

-   `force_reprofile <- TRUE` was used in `2-profile-<dataset>.R` (and thus part building logic was changed)
-   changes were made to the crosswalk
-   an existing raw file was replaced

When `force_rebuild <- TRUE`, the existing parts are removed and reconstructed. Return it to `FALSE` for ordinary incremental part building.

### 5. Summarize Date Coverage

Script: `5-date-info.R`

These scripts determine the preferred available date field for each part, output its minimum and maximum date, and identify parts with the same date range to gather information used to identify duplicate datasets.

Outputs in `data/<dataset>/metadata/`:

-   `parts-metadata.parquet`: displays each part's numbered name, source file, source sheet, number of rows, first date column match from a list of preferred date fields, minimum and maximum dates, and names and counts of non-empty columns
-   `same-date-ranges.parquet`: outputs parts that share equal minimum and maximum date values and their number of rows

Review parts with missing date columns and pairs with the same date range. The list of possible date fields in each script should be updated if a new canonical date field is introduced.

### 6. Explore Matching Date Ranges

Scripts: `6-explore-mismatches.R` for apprehensions and inadmissibles.

These scripts compare pairs identified in `same-date-ranges.parquet`. They record rows found in only one part and compare non-empty columns across the pair.

Outputs are written to `data/<dataset>/metadata/unique-rows/`:

-   `pair-<n>-rows-only-in-a.parquet`: part containing rows that are unique to part `a`
-   `pair-<n>-rows-only-in-b.parquet`: part containing rows that are unique to part `b`
-   `pair-<n>-column-differences.parquet`: examines diagnostics among columns shared between same date range files, listing the shared column name, values only in each part, and flagging columns that contain unique values

These files are diagnostic only and should be used to explore whether two parts are duplicates, complementary, or entirely different datasets. Any part to be excluded from the final dataset must be manually added by name to `parts_to_delete` in `<n>-stack-<dataset>.R`. The encounters pipeline does not currently have a mismatch exploration step because there were no exact date range matches among files.

### 7. Validate Broader Overlaps

Scripts: `7-validate-apprehension-overlaps.R` and `9-validate-inadmissible-overlaps.R`.

These scripts identify parts whose full date ranges are contained within other parts. They then compare daily record counts between each pair.

Outputs in `data/<dataset>/validation/`:

-   `<dataset>-contained-pairs.parquet`: lists parts whose dates are contained within others, including part names, paths, and minimum and maximum dates
-   `<dataset>-overlap-daily-comparison.parquet`: examines individual days covered by overlapping datasets, displaying part names and paths, minimum and maximum dates, daily apprehension or inadmissible count, daily count difference between each part, and a logical flag indicating whether daily counts match
-   `<dataset>-overlap-pair-summary.parquet`: summarizes overlapping pairs, including counts for number of days compared, number of matching and differing days, percentage of days matching, number of records in each part's overlapping time period, total count difference, maximum absolute daily difference, and a logical flag indicating whether all daily counts match

The overlapping pair summary and daily comparison should be reviewed before deciding how overlap should be resolved. This script does not remove records and is merely exploratory. Overlap resolution occurs later in the pipeline. The encounters pipeline does not currently validate overlaps because it has no overlapping files.

### 8. Stack the Parts

Scripts: `8-stack-apprehensions.R`, `7-stack-inadmissibles.R`, and `6-stack-encounters.R`.

These scripts combine the remaining parts by column name and verify that the final row count equals the sum of the included parts.

Outputs in `data/<dataset>/processed/`:

-   `<dataset>-stacked.parquet`: stacked dataset containing all parts that are not exact duplicates or erroneous, still containing overlapping parts and not yet cleaned
-   `<dataset>-audit.parquet`: displays individual part name, path, and number of rows

Before running the apprehensions or inadmissibles stack, review `parts_to_delete` near the beginning of the script. This list is maintained manually based on the overlap review and removes known faulty or duplicate parts only. The script deletes listed files from `parts-to-stack/`, so the parts must be fully rebuilt if an exclusion is to be reversed. Encounters currently has no duplicate or erroneous parts to remove.

### 9. Build Code Column Map

Scripts: `9-build-code-map.R` for apprehensions and `7-build-code-map.R` for encounters.

These scripts create lookup tables (or "maps") that translate coded values into readable labels. Each mapping identifies the field, original code, and corresponding full name. Uncertain codes are retained as they are originally written.

Outputs:

-   `data/apprehensions/metadata/code-map.parquet`: apprehensions code mapping, including field, code name, and full name

-   `data/encounters/metadata/code-map.parquet`: encounters code mappings, including field, code name, and full name

The mapping tables are maintained manually and should be reviewed when new coded columns or values appear in the stacked data.

### 10. Clean the Combined Data

Scripts: `10-clean-apprehensions.R`, `8-clean-inadmissibles.R`, and `8-clean-encounters.R`.

These scripts standardize redaction codes, convert fields to their intended data types, order columns, and write the cleaned data.

Outputs:

-   `data/<dataset>/processed/<dataset>-cleaned.parquet`: cleaned dataset in final format, still containing overlapping parts
-   `data/encounters/processed/encounters-final.parquet`: final encounters dataset

The apprehensions and inadmissibles scripts call this output `<dataset>-cleaned.parquet` because overlap resolution occurs in the next step. For encounters, this output is the final dataset.

The following objects are maintained manually in each cleaning script and should be reviewed when the crosswalk gains new fields: `column_order`, `datetime_columns`, `date_columns`, `time_columns`, `logical_columns`, `integer_columns`, `double_columns`, and the accepted logical and missing-value codes.

The apprehensions and encounters cleaning scripts use the code maps generated in the previous scripts to translate coded values into full names and consolidate paired fields into a single column. Existing full-name values take priority and values without a matching translation are retained as written.

Review all warnings about unrecognized values or columns absent from `column_order`. Columns not assigned another type remain character fields by default. Parts that overlap in date are retained in this step. In using this dataset, note that the same entry may be represented multiple times in overlapping datasets.

### 11. Resolve Apprehension Overlaps

Script: `11-resolve-apprehension-overlaps.R`.

This step resolves overlapping parts by prioritizing broader source parts, using more granular parts to fill gaps in dates not already represented. Rows without a usable event date are excluded with a warning.

Outputs:

-   `data/apprehensions/processed/apprehensions-final.parquet`: final apprehensions dataset
-   `data/apprehensions/validation/apprehensions-overlap-resolution.parquet`: displays part file, source, and sheet, minimum and maximum dates, number of days spanned per part, number of days included in the final dataset per part, and a logical flag to indicate whether the part was retained or not

Review the resolution audit to confirm which dates were selected from each source.

### 12. Audit the Final Apprehensions Data

Script: `12-audit-apprehensions-final.R`.

This script compares column population across the stacked, cleaned, and final apprehensions files.

Outputs:

-   `data/apprehensions/validation/apprehensions-cols-empty-before-cleaning.parquet`: displays columns empty before cleaning
-   `data/apprehensions/validation/apprehensions-cols-redacted-to-null.parquet`: displays columns that contained redaction codes in the stacked data and were converted to `NULL` when converted to non-character types
-   `data/apprehensions/validation/apprehensions-cols-excluded-by-overlap-resolution.parquet`: displays columns that were populated in the cleaned dataset but became empty after overlap resolution, including the number of populated values excluded with the removed rows
-   `data/apprehensions/metadata/final-column-inventory.parquet`: inventory of columns retained in the final dataset, including column position and name, data type, non-missing rows in the stacked, cleaned and final stages, total rows, number of missing rows, percent of missing rows in the final dataset, logical flags to indicate whether the column is empty or is/has a redaction flag, and the number of non-missing rows removed in overlap resolution and elsewhere

Consider excluded columns before distributing final datasets.

### 13. Cross-Reference Datasets

Scripts: `9-cross-reference-encounters.R` and `13-cross-reference-apprehensions.R`.

These scripts compare monthly counts in `<dataset>-final.parquet` with published CBP dashboard extracts.

Outputs in `data/<dataset>/validation/`:

-   `nationwide-encounters-*-aor.csv`: CBP benchmark files downloaded from dashboards
-   `<dataset>-monthly-cross-reference.parquet`: displays final dataset monthly counts, CBP dashboard monthly counts, fiscal year and month, monthly, absolute, and percent differences, a logical flag indicating exact match in monthly counts, and a `status` column

The `cbp_benchmarks` table is maintained manually. Before each update, check whether CBP has published a newer benchmark and add its identifier, release date, and URL. The script keeps the most recent available benchmark for each month.

## Analysis Scripts

The `analysis/` directory within each code folder contains optional scripts that describe coverage after the main pipeline has been run.

-   The three `<dataset>-parts-timeline.R` scripts read `parts-metadata.parquet` and produce PDF timelines showing the date range of each part and whether it is contained within another part.

-   The three `monthly-<dataset>-counts.R` scripts read the final datasets and write monthly counts by source file to: `analysis/monthly-<dataset>-by-part.parquet`

These scripts currently limit results to dates beginning January 1, 2020. Change the date in the SQL `WHERE` clause if a different period is required.

## Updating the Data

For an ordinary update after CBP publishes a new file:

1.  Run the download script and review `manual-review/`.
2.  Run the profiling script with `force_reprofile <- FALSE`.
3.  Review new distinct columns and update the crosswalk.
4.  Set `force_rebuild <- TRUE` if the crosswalk or an existing input changed. Otherwise create only the new parts with `force_rebuild <- FALSE`.
5.  Rebuild the date metadata and review overlaps.
6.  Update the manual duplicate or faulty-part exclusions.
7.  Stack the data.
8.  Review columns to collapse in the cleaning stage and rerun the map.
9.  Clean the data.
10. Run the dataset-specific overlap resolution, audit, or cross-reference steps.
11. Review warnings and validation outputs before publishing final files.
