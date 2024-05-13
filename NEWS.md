# migbirdHIP (dev version)

## Major changes & new features

-   Edited `shiftCheck()` to return a summary of shift errors rather than just a table of record id values.
-   Edited `issueCheck()`, `issueAssign()`, and `issuePlot()` to accommodate new rules in evaluating if a record is current. All records are now current unless their `issue_date` falls before `issue_start` or after the last day of migratory bird hunting in the record's state.
-   Edited `proof()` and `errorPlot_fields()` to no longer flag and/or plot youth hunters (hunters with birth year < 16 years ago).

## Minor changes / bug fixes

-   Template `dl_report.qmd`
    - Excluded future data line from agenda tab
    - Minor adjustments for edge cases in which summary variables are `NULL`
    - Added clarification to table captions
    - Eliminated irrelevant error and warning message printouts from rendered report
    - Remove inconsequential "bad bags" from Agenda tab; do not report 1s received instead of 0s, or 0s received instead of 1s
    - Edited "bad bags" section of agenda tab to return a message in the edge case of 0% of a file containing bad bag values
    - Edited the Issuance tab to reflect new rules in evaluating if a record is current
- Edited `identicalBags()` function to exclude matching coots_snipe and rails_gallinules from MI in output; this state uses the response from one question to populate both fields.
- Edited `read_hip()` function to exclude "hold" subdirectories when reading season HIP data.
- Updated R dependency to v4.4.0.
- Added programmatic `stopifnot()` to all functions to safeguard against running with incorrect/invalid parameters.
- Reduced variation in parameter names:
    - `distinct` changed to `unique` for `pullErrors()`
    - `output` changed to `return` for `outOfStateHunters()`
    - `assigned_data` changed to `x` for `issuePlot()`
    - `data` changed to `x` for `glyphCheck()`, `glyphFinder()`, `issueAssign()`, `issueCheck()`, and `shiftCheck()`

# migbirdHIP 1.2.8

## Major changes & new features

-   Added a `NEWS.md` file to track changes to the package.
-   Added package documentation page `man/migbirdHIP-package.Rd`
-   New `fileCheck()` function: checks if any files in the input folder have already been written to processed folder.
-   New `shiftCheck()` function: find and print any rows that have a line shift error with number of positions shifted.
-   New `identicalBags()` function: returns output if any columns are exactly the same in a file; does not return "no season" matches.
-   New `glyphCheck()` function: pull and view any non-UTF-8 characters in the raw data; helps guide manual fixes to read in the HIP files without line shifts.
    -   `glyphFinder()` no longer exported, now used internally inside of `glyphCheck()`
-   Added 3 new internal package functions (`errorLevel_errors_field()`, `errorLevel_errors_state()`, and `recordLevel_errors_state()`), which are used inside `redFlags()`, `errorPlot_fields()`, and `errorPlot_states()`. They reduce code redundancy and ensure updates happen universally.
-   Added 2 new internal package functions (`issueAssign()` and `issuePlot`), which are used inside of `issueCheck()` and by the download report (`dl_report.qmd`).
-   Added internal function `strataFix()` to be used inside of `clean()` to resolve false permit labels. This function edits strata values for `band_tailed_pigeon` and `crane` from states that submit permit files for crane and band-tailed pigeons; values changed from `"2"` to `"0"`.
-   Edited `writeReport()` to render quarto documents.
-   Edited `issueCheck()` to place more emphasis on `issue_date` to determine relevancy of a record. The function no longer exports future and past data as `.csv` files. Past data are still filtered out from the returned tibble. Output messages indicate if future data exist.
-   Edited `clean()` function:
    -   Filter out any rows that contain a bag value other than a single digit
    -   Eliminated address cleaning
    -   Moved zip code checking and messaging to `clean()` from `proof()`; now checks on entire zip code, not just prefix. Remove ending `0` when `zip` value is 10 digits long.
    -   Changed Oregon solo permit `hunt_mig_birds` field when it equals `"0"` to `"2"`. For context, a solo permit contains a `"2"` in at least one of the `band_tailed_pigeon`, `brant`, or `seaduck` fields and contains `"0"` in all other bag fields.
-   Edited `correct()` to remove any records with value of `"0"` or `NA` value in every bag field; improved `email` field cleaning and repair.
-   Edited `strataCheck()` to return two additional fields in output; 1) number of bad strata and 2) proportion of bad strata. The function now checks for permit species coming during regular HIP and returns them as erroneous (e.g. NM `band_tailed_pigeon` = `"2"`).
-   Edited `write_hip()` to set any state/species combinations without a season to have strata of `"0"`; bad bag values remain NA.
-   Edited `sumLines()` to improve speed and efficiency. In addition, the function now returns a data table with the sum of lines per file instead of a single number. No longer exported; set as internal function.
-   Edited `read_hip()` to eliminate encoding check and optionally use `sumLines()` function to ensure all lines were read in. Returns a message if any records contain a bag value other than a single digit. In addition, now converts blank strings to `NA`.
-   Edited `validate()` to return `source_file` field and filter out states and species with no season from function output.
-   Edited `investigate()` to no longer be exported; it works inside of `validate()` to return a more detailed output. This replaces the previous workflow of running `investigate()` separately.
-   Removed `manualFix()` function because it is no longer relevant to the package.
-   Removed `shiftFix()` because line shift errors cannot be fixed programmatically on a reliable basis.
-   Templates
    -   New Quarto `dl_report.qmd` replaced RMarkdown `dl_report.Rmd`.
        -   The new Quarto layout allows tabset panels which divides content into sections that can be more easily read and focused on by the user. Tabset panels were also incorporated for before and after plots to show proportion of errors that are corrected during pre-processing.
        -   A new summary section distills the findings of the functions overall for the user to discern the most important issues from the HIP files that were processed. This is partly accomplished with the use of a `catch_messages()` function created only for use in the `dl_report.qmd` and is not exported or contained within the `migbirdHIP` package internally. The `catch_messages()` function wraps around pre-processing functions (such as `read_hip()`, `clean()`, `issueCheck()`, etc) and captures messages in a list so that they can be returned as readable bullet points.
        -   A new map displays time lag of files received from 49 states in a hexagonal representation of the continental US.
        -   Emojis are printed with output text to quickly indicate to readers whether issues ❌ need attention or ✔️ are not concerning.
        -   Sections added as needed to report on new function output (see above for which new functions were added).
        -   A new section lists any states that were excluded from the output when they submitted data for that download (e.g. all records were issued in the past and are not eligible for the current season; perhaps sent by mistake).
    -   Eliminated `season_report.Rmd` template
-   Imports
    -   Removed `magrittr` and `rmarkdown`
    -   Added `quarto` and `sf`
-   Suggests
    -   Added `spelling`
-   Internal package data (`sysdata.rda`)
    -   Added vectors of abbreviated US territories and Canada provinces/territories, both updated to include missing abbreviations from previous versions and remove redundant abbreviations
    -   Added vector of bag field names
    -   Added vector of two-season states
    -   Added vectors of seaduck and brant states, seaduck-only states, and two-season states
    -   Added hexmap grid for download report
    -   Added tibbles of permit file states/species and states/species of permits received inline
    -   Updated zip code reference table, bag reference table, license window reference table, and MS reference dates

## Minor changes / bug fixes

-   License changed to CC0 (previously Public Domain), which was causing a warning in `devtools::check()`
-   Refactored `write_hip()` to eliminate redundancy; replaced repeated `left_join()` with for loop
-   Refactored `findDuplicates()` by throwing an error message for a bad string supplied to the `return` parameter at the start, which reduces wait time for failure.
    -   Investigated replacing `findDuplicates()` redundancy of searching for duplicate fields using a `for` loop or `purrr::map()`, but this change added 20+ seconds of processing time so left the redundancy as-is.
-   Refactored all functions that take a path parameter to add a forward slash to the end each supplied path if not included by the user.
-   Replaced superseded `tidyr::separate()` with `tidyr::separate_wider_delim()` or `tidyr::separate_wider_position()`
-   Replaced `dplyr::summarize()` with `dplyr::reframe()` since returning more than 1 row per group was deprecated in `dplyr 1.1.0`
-   Replaced `ggplot::stat()` with `ggplot::after_stat()`, since the former was deprecated in `ggplot2 3.4.0`
-   Replaced tidy pipes `%>%` and `%<>%` with base R pipe `|>` for increased speed and reduced dependency on tidyverse packages.
-   Edited `DESCRIPTION` file:
    -   Changed package description
    -   Set language to `en-US`
    -   Added a URL to the Harvest Information Program website
-   Incorporated `usethis::use_spell_check()` to package checking workflow, which added an `inst/WORDLIST` file (whitelisted words) to the package.

# migbirdHIP 1.2.7

-   Updated strata reference table in [sysdata.rda](https://github.com/USFWS/migbirdHIP/commit/b716713a96c1aeb25f4a7f32d12b98ecc2ac1b0e)
-   Added kable summary tables to horizontal repetition checks in the [download report template](https://github.com/USFWS/migbirdHIP/commit/46a87cdaa025b7a4f229225a6afe65ee65853b87)
-   Due to new kables, updated [DESCRIPTION](https://github.com/USFWS/migbirdHIP/commit/3bd4354a7769180aefa3743daa116393e7a4497e) to include kableExtra as a Suggests (and added other packages used in the dl_template.Rmd not previously included)
-   Updated [.Rbuildignore](https://github.com/USFWS/migbirdHIP/commit/0f5df5b28c9f66fee264eee44cd4a5a8a5f4628c) to reduce R CMD check notes

# migbirdHIP 1.2.6

-   Added `glyphFinder()` function
-   Revisions to `issueCheck()` and `proof()`
-   Revisions to `dl_report.Rmd` template

# migbirdHIP 1.2.5

-   Operational for 2022-2023 season

# migbirdHIP 1.2.4

-   Final version compatible with 2021-2022 season data

# migbirdHIP 1.2.3

-   Updated `fixDuplicates()` and `validate()`

# migbirdHIP 1.2.2

-   Updated vignette
-   Updated readme
-   Updated description

# migbirdHIP 1.2.1

-   Updated package name to `migbirdHIP` in function documentation

# migbirdHIP 1.2.0

-   Package renamed `migbirdHIP`

# migbirdHarvestData 1.1.0

-   All functions complete for end of 2021-2022 season
-   Fully functional season_report template

# migbirdHarvestData 1.0.0

-   First fully functional version
