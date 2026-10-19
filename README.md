# CBP Data Processing

## Intro

We provide code for collecting and processing two CBP datasets: USBP apprehensions and USBP encounters. The encounters data include Title 8 apprehensions and Title 42 expulsions and exclude Title 8 inadmissibles recorded by the Office of Field Operations.

Each pipeline collects files published by CBP, standardizes their differences, identifies and removes overlapping coverage, and combines the records into usable datasets. Field definitions and more information about the data can be found in the `cbp-data-codebook.qmd`.

**Note: The OFO inadmissibles pipeline and dataset are still in progress and will be updated upon completion.**

## Running the Pipelines

To reproduce our final datasets, run each type of script in numeric order. Several scripts have functions that require manual inputs and are explained below. When CBP publishes new files, run the pipeline again, beginning with the first download script. Review all warnings and audit tables before proceeding.

## Repository Organization

In the `cbp` repository, there are three main folders through which you can navigate. Throughout this document, `<dataset>` indicates the relevant dataset between apprehensions or encounters.

-   The `code/<dataset>` folder contains all project code needed to reproduce each dataset.

-   The `data/<dataset>/` folder contains all data extracted through the pipeline.

    -   `~/included-sources/`: source files accepted into the pipeline downloaded from CBP's website

    -   `~/excluded-sources/`: downloaded files excluded by the inclusion rules; review before including

    -   `~/metadata/`: inventories, crosswalks, metadata, and other information

    -   `~/processed/`: any transformed raw files

    -   `~/validation/`: validation outputs used to compare pipeline counts with CBP dashboards

-   The `inputs/` folder contains all of the raw PDF files provided by the Deportation Data Project to fill a gap in CBP data found in 2014

### 0. Convert PDF Files

Scripts: `0-convert-pdf.R` in apprehensions

This script converts PDFs used to fill in a gap in information found in 2014 on CBP's website. We used files obtained by Deportation Data Project co-director David Hausman, represented by the Law Office of Amber Qureshi and the National Immigration Project. This script exists only for apprehensions and does not need to be rerun after the PDF files are converted to parquet form.

Input:

-   `inputs/pdfs/`: contains original PDF files

Output:

-   `data/apprehensions/included-sources/usbp_apprehensions_nationwide_fy14.parquet`: combined parquet version of original PDF files

### 1. Download Source Files

Scripts: `1-download-<dataset>.R`

These scripts scrape the relevant CBP records page, identify downloadable links, and download links that are not already recorded in the existing link inventory.

Outputs:

-   `data/<dataset>/included-sources/`: contains files that meet inclusion criteria
-   `data/<dataset>/excluded-sources/`: contains files excluded by the inclusion criteria and retained for review
-   `<dataset>-links.parquet`: contains links that the script interacted with

Any new files that appear in `excluded-sources/` should be manually reviewed before proceeding to the next script. Because the link inventory identifies files by URL, the script will not recognize if CBP replaces an existing link with a different file. Thus, existing URLs should be confirmed if revisions are suspected.

### 2. Profile Worksheets and Headers

Scripts: `2-profile-<dataset>.R`

These scripts identify the header row in each worksheet, inventory the columns in `included-sources/` files, and find all distinct column names.

Outputs in `data/<dataset>/metadata/`:

-   `sheet-inventory.parquet`: displays file name, file path, and sheet name for all `included-sources/` file sheets
-   `column-inventory.parquet`: displays file name, file path, sheet name, header row placement, number of rows skipped to detect header row, number of columns in that sheet, raw column name, clean column name (after applying `janitor::make_clean_names()`, but before canonical standardization), and column position for every column in `included-sources/`
-   `distinct-columns.parquet`: identifies all unique cleaned columns from `column-inventory.parquet` and displays their clean column and raw column names, number of files each unique column appears in, and example files containing each column
-   `failed-sheets.parquet`: created only when a sheet fails profiling

By default, `force_reprofile <- FALSE`, so only files that are not already represented in the sheet inventory are profiled. Set `force_reprofile <- TRUE` when the profiling rules change, an existing raw file is replaced, or all metadata must be rebuilt. If it is set to `TRUE`, set `force_rebuild <- TRUE` in the later `4-create-parts.R` script. Review `failed-sheets.parquet` whenever it is created.

### 3. Build the Column Crosswalk

Scripts: `3-build-crosswalk.R`

This script maps variant raw headers to canonical column names and writes `data/<dataset>/metadata/crosswalk.parquet`.

Outputs in `data/<dataset>/metadata/`:

-   `crosswalk.parquet`: displays clean column names, raw column names, number of files the clean column can be found in, and canonical names

The crosswalk definitions are written directly in each script and require manual maintenance. Update the crosswalk whenever `distinct-columns.parquet` contains a new raw column. By default, this script considers cleaned column names as canonical unless they are otherwise specified. After changing a crosswalk, rerun the script and set `force_rebuild <- TRUE` in the next step.

### 4. Create Standardized Parts

Script: `4-create-parts.R`

This script rereads every successfully profiled worksheet, standardizes headers, and writes one parquet part to `data/<dataset>/processed/parts-to-stack/` per sheet. A "part" refers to a raw file sheet whose headers have been standardized.

Outputs:

-   `data/<dataset>/processed/`: contains all data that has been processed in some way
-   `data/<dataset>/processed/parts-to-stack`: contains processed sheets with canonical column names to eventually be stacked to create final datasets

By default `force_rebuild <- FALSE`, so only new sheets will be converted into stackable parts. Set `force_rebuild <- TRUE` if:

-   `force_reprofile <- TRUE` was used in `2-profile-<dataset>.R`
-   changes were made to the crosswalk
-   an existing raw file was replaced

When `force_rebuild <- TRUE`, the existing parts are removed and reconstructed. Return it to `FALSE` for ordinary incremental part building.

### 5. Stack the Parts

Scripts: `5-stack-apprehensions.R`, `5-stack-encounters.R`.

These scripts combine the remaining parts by column name.

Outputs in `data/<dataset>/metadata/`:

-   `parts-metadata.parquet`: displays each part's numbered name, source file, source sheet, number of rows, first date column match from a list of preferred date fields, minimum and maximum dates, and names and counts of non-empty columns

Outputs in `data/<dataset>/processed/`:

-   `<dataset>-stacked.parquet`: stacked dataset containing all parts that are not exact duplicates or erroneous, still containing overlapping parts and not yet cleaned
-   `<dataset>-audit.parquet`: displays individual part name, path, and number of rows

Before running the apprehensions stack, review `parts_to_delete` near the beginning of the script. This list is maintained manually based on overlap review. The script deletes listed files from `parts-to-stack/`.

### 6. Build Code Column Map

Scripts: `6-build-code-map.R`

These scripts create look-up tables that translate coded or truncated values into readable labels. Uncertain values are retained as they are originally written.

Outputs:

-   `data/apprehensions/metadata/code-map.parquet`: apprehensions code mapping, including field, code name, and full name

-   `data/encounters/metadata/code-map.parquet`: encounters code mappings, including field, code name, and full name

The mapping tables are maintained manually and should be reviewed when new coded columns or values appear in the stacked data.

### 7. Clean the Combined Data

Scripts: `7-clean-<dataset>.R`

These scripts standardize redaction codes, convert fields to their intended data types, order columns, and write the cleaned data.

Outputs:

-   `data/<dataset>/processed/<dataset>-cleaned.parquet`: cleaned dataset in final format, still containing overlapping parts
-   `data/encounters/processed/encounters-final-all-cols.parquet`: final encounters dataset with all columns

For encounters, this output is the final dataset. The apprehensions scripts call this output `<dataset>-cleaned.parquet` because the next script resolves overlapping datasets. Several objects are maintained manually in each cleaning script and should be reviewed when the crosswalk gains new fields.

Review all warnings about unrecognized values or columns absent from `column_order`. Columns not assigned another data type remain character fields by default. Parts that overlap in date are retained in this step. In using this dataset, note that the same entry may be represented multiple times in overlapping datasets.

### 8. Resolve Apprehension Overlaps

Script: `8-resolve-apprehension-overlaps.R`.

This script resolves overlapping parts by prioritizing broader source parts, using more granular parts to fill gaps in dates not already represented. Rows without a usable event date are excluded with a warning.

Outputs:

-   `data/apprehensions/processed/apprehensions-final-all-cols.parquet`: final apprehensions dataset with all columns
-   `data/apprehensions/validation/apprehensions-overlap-resolution.parquet`: displays part file, source, and sheet, minimum and maximum dates, number of days spanned per part, number of days included in the final dataset per part, and a logical flag to indicate whether the part was retained or not

Review the resolution audit to confirm which dates were selected from each source.

### 9. Audit the Final Data

Script: `9-audit-apprehensions.R`, `8-audit-encounters.R`

These scripts compare column population across the stacked, cleaned, and final apprehensions files.

Outputs:

-   `data/<dataset>/metadata/raw-column-inventory.parquet`: inventory of raw columns, including column position and name, data type, non-missing rows in the stacked, cleaned and final stages, total rows, number of missing rows, percent of missing rows in the final dataset, logical flags to indicate whether the column is empty or is/has a redaction flag, and the number of non-missing rows removed in overlap resolution and elsewhere
-   `data/<dataset>/metadata/final-column-inventory.parquet`: inventory of columns retained in the final dataset, including column position and name, data type, non-missing rows in the stacked, cleaned and final stages, total rows, number of missing rows, percent of missing rows in the final dataset, logical flags to indicate whether the column is empty or is/has a redaction flag, and the number of non-missing rows removed in overlap resolution and elsewhere
-   `data/<dataset>/metadata/raw-column-missingness-matrix.parquet`: matrix containing each raw column and its percent missing within each source file sheet
-   `data/<dataset>/metadata/final-column-missingness-matrix.parquet`: matrix containing each final column and its percent missing within each source file sheet

Consider excluded columns before distributing final datasets.

### 10. Cross-Reference Datasets

Scripts: `9-cross-reference-encounters.R` and `10-cross-reference-apprehensions.R`.

These scripts compare monthly counts in `<dataset>-final-all-cols.parquet` with published CBP dashboard extracts.

Outputs in `data/<dataset>/validation/`:

-   `nationwide-encounters-*-aor.csv`: CBP benchmark files downloaded from dashboards
-   `<dataset>-monthly-cross-reference.parquet`: displays final dataset monthly counts, CBP dashboard monthly counts, fiscal year and month, monthly, absolute, and percent differences, a logical flag indicating exact match in monthly counts, and a `status` column

The `cbp_benchmarks` table is maintained manually. Before each update, check whether CBP has published a newer benchmark and add its identifier, release date, and URL.

### 11. Select Final Columns

Scripts: `10-select-final-cols.R` in encounters, `11-select-final-cols.R` in apprehensions.

These scripts select final columns based on usefulness and percent missing, outputting what we believe to be the most useful dataset after the pipeline.

Outputs in `cbp/data/<dataset>/processed`:

-   `apprehensions-latest.parquet`: contains the following columns
    -   `apprehension_datetime`
    -   `entry_date`
    -   `final_bookout_date`
    -   `arrest_sector`
    -   `arrest_method`
    -   `age`
    -   `gender`
    -   `citizenship`
    -   `marital_status`
    -   `number_of_children_and_nationality`
    -   `fmua_indication`
    -   `unaccompanied_child_indicator`
    -   `entry_status`
    -   `credible_fear_indicator`
    -   `cds_program`
    -   `prosecution_indicator`
    -   `statute_charge`
    -   `charge_code`
    -   `disposition`
    -   `removal_type`
    -   `source_file`
    -   `source_sheet`
-   `encounters-latest.parquet`: contains the following columns
    -   `encounter_datetime`
    -   `most_recent_encounter_date`
    -   `earliest_encounter_date`
    -   `number_of_previous_encounters`
    -   `final_bookout_datetime`
    -   `border`
    -   `arrest_sector`
    -   `arrest_state`
    -   `arrest_at_checkpoint_indicator`
    -   `age`
    -   `adult_or_juvenile`
    -   `gender`
    -   `citizenship`
    -   `residence_city`
    -   `residence_country`
    -   `marital_status`
    -   `subject_group_classification`
    -   `number_of_children_and_nationality`
    -   `time_in_us`
    -   `ces_indicator`
    -   `mpp_indicator`
    -   `transfer_to_group`
    -   `drugs_seized_indicator`
    -   `statute_charge`
    -   `disposition`
    -   `referred_for_prosecution_under_8usc1325_or_8usc1326`
    -   `source_file`
    -   `source_sheet`

## Updating the Data

For an ordinary update after CBP publishes a new file:

1.  Run the download script and review `excluded-sources/`.
2.  Run the profiling script with `force_reprofile <- FALSE`.
3.  Review new distinct columns and update the crosswalk.
4.  Set `force_rebuild <- TRUE` if the crosswalk or an existing input changed. Otherwise create only the new parts with `force_rebuild <- FALSE`.
5.  Update the manual part exclusions.
6.  Run the stacking script.
7.  Review columns to collapse in the cleaning stage and rerun the map.
8.  Clean the data.
9.  Run the dataset-specific overlap resolution, audit, cross-reference, and column selection steps.
10. Review warnings and validation outputs before publishing final files.
