# migbirdHIP (dev version)

## Major changes & new features

-   Added a `NEWS.md` file to track changes to the package.
-   Replaced tidy pipes `%>%` and `%<>%` with base R pipe `|>`
-   New `fileCheck()` function
-   New `shiftCheck()` function
-   New `identicalBags()` function
-   New `glyphCheck()` function; `glyphFinder()` no longer exported
-   Templates
    -   New `dl_report.qmd` replaces `dl_report.Rmd`, with additional features
    -   Eliminated `season_report.Rmd` template
-   Imports
    -   Removed `magrittr` and `rmarkdown`
    -   Added `quarto` and `sf`
-   Edited writeReport() to render quarto documents
-   Refactored read_hip() and eliminated encoding check
-   Added 3 new internal package functions (`errorLevel_errors_field()`, `errorLevel_errors_state()`, and `recordLevel_errors_state()`), which are used inside `redFlags()`, `errorPlot_fields()`, and `errorPlot_states()`. They reduce code redundancy and ensures updates happen universally.
-   `issueCheck()` no longer exports future and past data as .csv files. Past data are still filtered out from the returned tibble, and output messages indicate if future data exist. More emphasis is put upon `issue_date` to determine relevancy of a record.
-   Added 2 new internal package functions (`issueAssign()` and `issuePlot`), used inside of `issueCheck()` and by the download report
-   `strataCheck()` now returns 2 additional fields in output; number of bad strata and proportion of bad strata. It also checks for permit species coming during regular HIP and returns them as erroneous (e.g. NM band-tailed pigeon = 2).
-   `validate()` was edited to return source file field in output and filter out states and species with no season from output.
-   `investigate()` no longer exported; it works inside of `validate()` to return a more detailed output without running `investigate()` separately
-   `write_hip()` sets any state/species combinations without a season to have strata of 0; bad bag values remain NA
-   `sumLines()` returns a data table with the sum of lines per file instead of a single number; set as internal function and moved inside of `read_hip()`
-   `sysdata.rda`
    -   Added vectors of abbreviated US territories and Canada provinces/territories, both updated to include missing abbreviations from previous versions and remove redundant abbreviations
    -   Added vector of bag field names
    -   Added vector of two-season states
    -   Added vectors of seaduck and brant states, seaduck-only states, brant-only states, and two-season states
    -   Added hexmap grid for download report
    -   Added tibbles of permit file states/species and states/species of permits received inline
    -   Updated zip code reference table, bag reference table, license window reference table, and MS reference dates
-   Moved zip code checking and messaging to `clean()` from `proof()`; now checks on entire zip code, not just prefix

## Minor changes / bug fixes

-   Edited speed and efficiency in `sumLines()`
-   `read_hip()` converts blank strings to `NA`
-   Switched to `tidyr::separate_wider_delim()` or `tidyr::separate_wider_position()` to avoid superseded `tidyr::separate()`
-   Changed license back to CC0 from Public Domain, which was causing a warning in `devtools::check()`
-   Refactored `write_hip()` to eliminate redundancy; replaced repeated `left_join()` with for loop
-   Refactored `findDuplicates()`; moved error message to start, which reduces wait time for error; investigated replacing redundancy of searching for duplicate fields using a for loop or `purrr::map()` but this added 20+ seconds of processing time
-   Replaced `dplyr::summarize()` with `dplyr::reframe()` since returning more than 1 row per group was deprecated in `dplyr 1.1.0`
-   Added forward slash to end of all paths as needed
-   `clean()`
    -   Eliminated address cleaning
    -   Added zip code cleaning to remove final 0 from zip codes with length of 10 digits
-   Replaced `ggplot::stat()` with `ggplot::after_stat()`, since the former was deprecated in `ggplot2 3.4.0`

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
