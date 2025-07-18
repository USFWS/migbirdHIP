# migbirdHIP (dev version)

## Major changes & new features

-   Import `{ggnewscale}` for use of `ggnewscale::new_scale_color()` in `issue_plot()`

## Minor changes / bug fixes

-   `read_hip()`: add message that reports how long it took to read in `N` number of files
-   `writeReport()`: add more dependency checks
-   `badRegYearMessage()`: summarize by source file because it is more informative
-   `invalidDateMessage()`: created and added to `issueMessages()`
-   `issuePlot()`: improved plot appearance for clarity of interpretation

# migbirdHIP 1.4.0

## Major changes & new features

-   Changed contents of `R/sysdata.rda`
    -   Fewer objects are now stored in `sysdata.rda` (reduced from 14 to 7). This enhances transparency and reduces the number of objects that must be generated outside of the R package itself.
    -   `sysdata.rda` now contains: `REF_ZIP_CODE`, `REF_BAGS`, `REF_DATES`, `REF_STATES_2SEASON`, `REF_STATES_1SEASON`, `REF_EMAIL_TLDS`, and `SF_HEXMAP`
    -   Seven objects were moved to `constants.R`:
        -   `ref_bagfields`, `abbr_usa`, `abbr_canada`, `pmt_inline`, `pmt_files`, `states_sdbr`, and `states_seaducks` were moved and renamed `REF_FIELDS_BAG`, `REF_ABBR_USA`, `REF_ABBR_CANADA`, `REF_PMT_INLINE`, `REF_PMT_FILES`, `REF_STATES_SD_BR`, and `REF_STATES_SD_ONLY`, respectively.
    -   Two objects were dropped entirely. `MS_firstday` and `MS_lastday` are no longer needed by `issueCheck()`.
    -   Five internal data objects were renamed:
        -   `REF_BAGS` (previously `hip_bags_ref`), `REF_DATES` (previously `licenses_ref`), `REF_ZIP_CODE` (previously `zip_code_ref`), `REF_STATES_2SEASON` (previously `states_twoseason`) and `SF_HEXMAP` (previously `hexmap`).
    -   Two new objects were added: `REF_STATES_1SEASON` and `REF_EMAIL_TLDS`
    -   `REF_DATES` was changed to no longer include `last_day_migbird_hunting` and `category` fields.
-   Added test data
    -   Fake HIP test data creation script stored under `data-raw/`
    -   Test data containing fake HIP registrations stored as fixed-width `.txt` files under `inst/extdata/DL0901/`, to be used in testing or simulating `read_hip()`
    -   Miniature and tiny test data stored as exported `.rda` files under `data/`, to make it easier to demonstrate functions and run unit tests
        -   `DF_TEST_MINI` contains 1,606 rows from 7 states (OR records to represent solo permit state, ME records to represent SD-only state, DE records to represent SD and BR state, ND records to represent CR state, UT records to represent BT state, CO records to represent CR and BT state, and IA records to represent non-BT, CR, SD, or BR state) and is formatted as though the data were just read in.
        -   Six tiny test data objects:
            -   `DF_TEST_TINI_READ` is a subset of `DF_TEST_MINI`, and contains 3 rows formatted as though the data were just read in
            -   `DF_TEST_TINI_CLEANED` is the result of running `clean()` on `DF_TEST_TINI_READ`
            -   `DF_TEST_TINI_CURRENT` is the result of running `issueCheck()` on `DF_TEST_TINI_CLEANED`
            -   `DF_TEST_TINI_DEDUPED` is the result of running `duplicateFix()` on `DF_TEST_TINI_CURRENT`
            -   `DF_TEST_TINI_PROOFED` is the result of running `proof()` on `DF_TEST_TINI_DEDUPED`
            -   `DF_TEST_TINI_CORRECTED` is the result of running `correct()` on `DF_TEST_TINI_PROOFED`
-   Created `variables.R` to define seasonally changing variables in a central place.
    -   `REF_CURRENT_SEASON` for current HIP season.
    -   `REF_RELEASES` is a named vector of all `migbirdHIP` package releases and the corresponding season of HIP data that the version was intended for.
-   Created `constants.R` to define variables in a central place and thus evaluate data consistently.
    -   Variables are used across functions (e.g., `inLinePermitDNHMessage()` and `inLinePermitDNHFix()` both use `LOGIC_INLINE_PMT_DNH`) and are shared between functions and `testthat` files.
    -   New naming convention helps users to use and find internal reference data objects more quickly by using uppercase letters and categorical prefixes (`REF_`, `LOGIC_`, `REGEX_`, and `SF_`).
-   New functions
    -   `testRecordMessage()` added to `read_hip()` and `testRecordFilter()` added to `clean()` to find and filter out any testing records mistakenly sent to us by the states.
    -   New `duplicatePlot()` function added; `duplicateFinder()` (previously named `findDuplicates()`) function no longer outputs a plot.
    -   The `zeroBagsMessage()` internal function is a new feature of `read_hip()` that checks for records with all-zero bag values and returns a message to the console if they are detected.
    -   Added `errorTableSummary()` internal function to be used by `errorTable()`.
    -   Added four internal failure functions to reduce maintenance of assertions across other exported functions: `failYear()`, `failProofed()`, `failState()`, and `failTF()`.
-   Refactored and edited functions
    -   In an effort to improve the maintainability of the package code, steps were made toward modularity, clarity, and unit testing in some of the larger functions.
    -   `issueCheck()` and `issueAssign()`
        -   Edited to evaluate registrations differently:
            -   `last_day_migbird_hunting` is no longer used.
            -   Two-season states now use `registration_yr` to assign `decision` as `"overlap"` (new label), and later `"current"` or `"future"`, if the `issue_date` for a registration occurred during the overlap window.
            -   `registration_yr` is not changed for two-season states, only `"future"` assigned one-season states.
        -   `issueCheck()` split into 9 new minor internal functions:
            -   `issuePrint()`
            -   8 functions for messages: `issueMessages()`, `regYearEditMessage()`, `zeroDateMessage()`, `badDateMessage()`, `timeTravelMessage()`, `futureDateMessage()`, `pastDateMessage()`, and `twoSeasonMessage()`
        -   `issueAssign()` split to use an internal `issueDecide()` helper function.
    -   `proof()`
        -   Most logic used to identify and flag errors was moved to internal variables in the `constants.R` file, which are used by `proof()` and `test-proof.R`
        -   First name and last name rules were slightly relaxed
            -   No limit on number of spaces, apostrophes, or hyphens as long as they are not consecutive
            -   For first name, apostrophe or letter allowed as first character
        -   Email proofing rules now more strict
    -   `duplicateFinder()`
        -   New internal function `duplicateFields()` uses `purrr` to significantly reduce redundancy in `duplicateFinder()`; overall, refactoring reduced the function's length from 151 lines to 50 lines and improved processing speed.
    -   `duplicateFix()`
        -   Broken down into 7 new minor internal functions (`duplicateID()`, `duplicateNewest()`, `duplicateAllOnes()`, `duplicateAllOnesGroupSize()`, `duplicateDecide()`, `duplicateRecordType()` and `duplicateSample()`).
        -   Overall, this refactor reduced deduplication run time by an average of 2 seconds on a frame with 270,000 records.
        -   Records are no longer evaluated for having all-zero bags or periods instead of bag values because these records are now filtered out upstream during `clean()`.
    -   `read_hip()`
        -   Broken down into 19 new minor internal functions (`listFiles()`, `ignorePermits()`, `ignoreHolds()`, `ignoreLifetime()`, `idBlankFiles()`, `dropBlankFiles()`, `checkFileNameDateFormat()`, `checkFileNameStateAbbr()`, `readMessages()`, `missingPIIMessage()`, `missingEmailsMessage()`, `testRecordMessage()`, `zeroBagsMessage()`, `naBagsMessage()`, `nonDigitBagsMessage()`, `inLinePermitDNHMessage()`, `badRegYearMessage()`, `dlStateNAMessage()`, and `dlDateNAMessage()`).
        -   More strict requirements must be met for data to be successfully read (e.g. instead of returning a message that file names are incorrectly formatted, this would stop the process).
    -   `clean()`
        -   Broken down into 8 minor internal functions (2 previously used: `strataFix()` split into `cranePermitBagFix()` and `btpiPermitBagFix()`; and 6 new functions: `namesToUppercase()`, `missingPIIFilter()`, `moveSuffixes()`, `formatZip()`, `zipCheck()`, and `inLinePermitDNHFix()`)
        -   Middle initial values are no longer changed (this now happens in `correct()` via `correctMiddleInitial()`)
    -   `correct()`
        -   Broken down into 4 minor internal functions (`correctEmail()`, `correctTitle()`, `correctSuffix()`, `correctMiddleInitial()`))
        -   No longer filters out all-0 bag records (this now happens in `clean()` via `naAndZeroBagsFilter()`)
        -   Changes middle initial values if they are flagged in `proof()` via `correctMiddleInitial()` (this step previously happened in `clean()`)
    -   `write_hip()`
        -   Edited to include more checks before files are written out.
        -   New `type` param conditionally checks `record_type` field and `cranes`, `band_tailed_pigeon`, and `dove_bag` fields depending on the user input.
        -   `.xlsx` and `.xls` file extensions are converted to `.csv`
-   Renamed and moved functions
    -   Renamed `strataCheck()` to `bagCheck()` and renamed `strata.R` to `bags.R`.
        -   `bagCheck()` split and now uses an internal function, `summarizeBadBags()`.
        -   Standardized using the phrase "bag values" over the term "strata"; HIP records contain bag values until they are written out and strata are assigned.
    -   Renamed `renameFiles()` to `fileRename()` and moved from `renameFiles.R` to `files.R` to be grouped with `fileCheck()` (previously in `fileCheck.R`).
    -   `findDuplicates()` and `fixDuplicates()` renamed to `duplicateFinder()` and `duplicateFix()` to mirror naming conventions of other functions with the subject of the verb coming first (e.g. `glyphFinder()`, `glyphCheck()`).
    -   All functions related to duplicates moved to `duplicates.R` (previously separated into `findDuplicates.R` and `fixDuplicates.R`)
    -   Renamed functions using camel case except for `read_hip()` and `write_hip()`
        -   `errorPlot_fields()`, `errorPlot_states()`, and `errorPlot_dl()` now named `errorPlotFields()`, `errorPlotStates()`, `errorPlotDL()`
        -   `errorLevel_errors_state()` and `errorLevel_errors_field()` renamed to `errorLevelErrorsByState()` and `errorLevelErrorsByField()`
    -   `redFlags()` no longer exported (used only in the download report), moved to the `errorPlots.R` script instead of being in its own file
-   Deleted functions
    -   Deleted `validate()`, `investigate()`, and `identicalBags()`.
    -   Deleted demographic-oriented `outOfStateHunters()` and `youthHunters()` functions because they were not being used.
    -   Deleted `recordLevel_errors_state()` function since it was not being used.
    -   `sumLines()` deleted and `read_hip()` param `sumlines` eliminated; no longer used and not considered useful moving forward.

## Minor changes / bug fixes

-   Vignette
    -   Since the package now contains comprehensive test data, there is no need to pre-compute the vignette using HIP data stored locally.
    -   The `vignettes/man/migbirdHIP_workflow.Rmd.orig` file was deleted.
    -   Unused files in the `vignettes/image/` subdirectory were deleted; used files were moved to `man/figures/`.
-   Template `dl_report.qmd`:
    -   Sorted errors by descending in "causes of errors by state" table.
    -   Eliminated breaking error in report rendering if there is no field exceeding the error threshold.
    -   Removed all-zero bag section from the Agenda tab.
    -   Added comma formatting to long numbers.
    -   On the Errors tab, moved the "Bad zip codes" section to its own sub-header after "Causes of errors for top 3 fields".
-   Add `zzz.R`
    -   Run package startup message which returns the installed `migbirdHIP` package version and which season of HIP data the package version is compatible with.
-   Deleted `globals.R` and switched to data masking with `{rlang}` pronoun `.data`.
-   Incorporated some linting with `lintr` and added `.lintr` to `.gitignore` and `.Rbuildignore`.
-   Almost all data params changed to be less ambiguous (e.g. `x` now `cleaned_data`, `proofed_data`, etc).
-   `{tibble}` no longer a required import
-   `read_hip()` now catches file names with incorrect MMDDYYYY or DDMMYYYY date format.
-   `issueCheck()` now returns an error for NA values in `record_key` field.
-   Added release tags to README.
-   Replace deprecated `~ .x` anonymous function notation with `\(x)`.

# migbirdHIP 1.3.0

## Major changes & new features

-   Edited `shiftCheck()` to return a summary of shift errors rather than just a table of record id values.
-   Edited `issueCheck()`, `issueAssign()`, and `issuePlot()` to accommodate new rules in evaluating if a record is current. All records are now current unless their `issue_date` falls before `issue_start` or after the last day of migratory bird hunting in the record's state.
-   Edited `proof()` and `errorPlot_fields()` to no longer flag and/or plot youth hunters (hunters with birth year \< 16 years ago).

## Minor changes / bug fixes

-   Template `dl_report.qmd`
    -   Excluded future data line from agenda tab
    -   Minor adjustments for edge cases in which summary variables are `NULL`
    -   Added clarification to table captions
    -   Eliminated irrelevant error and warning message printouts from rendered report
    -   Remove inconsequential "bad bags" from Agenda tab; do not report 1s received instead of 0s, or 0s received instead of 1s
    -   Edited "bad bags" section of agenda tab to return a message in the edge case of 0% of a file containing bad bag values
    -   Edited the Issuance tab to reflect new rules in evaluating if a record is current
-   Edited `identicalBags()` function to exclude matching coots_snipe and rails_gallinules from MI in output; this state uses the response from one question to populate both fields.
-   Edited `read_hip()` function to exclude "hold" subdirectories when reading season HIP data.
-   Updated R dependency to v4.4.0.
-   Added programmatic stops to all functions to safeguard against running with incorrect/invalid parameters.
-   Reduced variation in parameter names:
    -   `distinct` changed to `unique` for `pullErrors()`
    -   `output` changed to `return` for `outOfStateHunters()`
    -   `assigned_data` changed to `x` for `issuePlot()`
    -   `data` changed to `x` for `glyphCheck()`, `glyphFinder()`, `issueAssign()`, `issueCheck()`, and `shiftCheck()`

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
