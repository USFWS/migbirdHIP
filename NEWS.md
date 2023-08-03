# migbirdHIP (dev version)

## Major changes & new features

-   Added a `NEWS.md` file to track changes to the package.
-   Replaced tidy pipes `%>%` and `%<>%` with base R pipe `|>`
-   New `fileCheck()` function
-   New `shiftCheck()` function
-   New `dl_report.qmd` replaces `dl_report.Rmd`, with additional features
-   Edited `writeReport()` to render quarto documents
-   Refactored `read_hip()` and eliminated encoding check
-   Imports: removed `magrittr` and `rmarkdown`, added `quarto` and `sf`
-   Eliminated `season_report.Rmd` template
-   Added 3 new internal package functions `errorLevel_errors_field()`, `errorLevel_errors_state()`, and `recordLevel_errors_state()` which are used in both `redFlags()` and/or `errorPlot_fields()`/`errorPlot_states()`, which reduces code redundancy and ensures updates happen in both functions; the new internal functions include math updates
-   `strataCheck()` now returns 2 additional fields in output; number of bad strata and proportion of bad strata
-   `validate()` now returns source file field in output, filters out states and species with no season from output
-   Added to `sysdata.rda`: permit state, seaduck state, and seaduck/brant state definitions; vectors of abbreviated US territories and Canada provinces/territories; vector of two-season staets; hexmap grid for download report
-   Added `identicalBags()` function
-   Moved zip code checking and messaging to `clean()` from `proof()`; now checks on entire zip code, not just prefix

## Minor changes / bug fixes

-   Edited speed and efficiency in `sumLines()`
-   `read_hip()` converts blank strings to `NA`
-   Switched to `tidyr::separate_wider_delim()` or `tidyr::separate_wider_position()` to avoid superseded `tidyr::separate()`
-   Changed license back to CC0 from Public Domain, which was causing a warning in `devtools::check()`
-   Refactored `write_hip()` to eliminate redundancy; replaced repeated `left_join()` with for loop
-   Refactored `findDuplicates()`; moved error message to start, which reduces wait time for error; investigated replacing redundancy of searching for duplicate fields using a for loop or `purrr::map()` but this added 20+ seconds of processing time
-   Replaced `dplyr::summarize()` with `dplyr::reframe()` since returning more than 1 row per group was deprecated in `dplyr 1.1.0`
-   Edited list of US and Canada abbreviations, which contained missing or redundant locations
-   Added forward slash to end of all paths as needed
-   Eliminated address cleaning from `clean()`
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
