# Changelog

## migbirdHIP 2026.0.2

### Minor changes / bug fixes

- Fix bug in `REF_DATES`.

## migbirdHIP 2026.0.1

### Minor changes / bug fixes

- Exclude North Dakota from being evaluated by
  [`qIssueDateRange()`](https://usfws.github.io/migbirdHIP/reference/qIssueDateRange.md).
- Updated `REF_BAGS` for AR `cranes`, `band_tailed_pigeon`, `brant`, and
  `seaducks` to expect state bag value of `0`.
- Updated `REF_DATES` for SD issue window start date to be `5/6/2026`.
- Edited
  [`regYearEditMessage()`](https://usfws.github.io/migbirdHIP/reference/regYearEditMessage.md)
  message text to contain sum of affected registrations.

## migbirdHIP 2026.0.0

### Versioning scheme change

Starting with the 2026-2027 season, version numbers will follow a
calendar versioning scheme (`YEAR.STAGE.PATCH`) instead of semantic
versioning (`MAJOR.MINOR.PATCH`). The new convention will be more
informative to users looking to downgrade `migbirdHIP`, since release
tags will be based on the applicable migratory game bird hunting season
rather than arbitrary numbers. Previous releases will retain their
original version numbers, which are still defined in `variables.R`.

Version component definitions:

- `YEAR` - the year of the migratory game bird hunting season
  (e.g. `2026` for the `2026-2027` season).
- `STAGE` - the maturity of the season’s release, either `0` or `1`:
  - `0` = the season is ongoing; used for releases between July and the
    following April, approximately (e.g. `2026.0.12`).
  - `1` = the definitive package version for the entire season,
    indicating a stable build that should be used if downgrading (e.g.,
    `2026.1.0` for an April 2027 definitive version).
- `PATCH` - incremental fixes within the current stage:
  - For an ongoing season, a bug fix would be released as `2026.0.3` and
    subsequent new feature would be released as `2026.0.4`.
  - For critical fixes after a definitive season release, to be avoided
    unless necessary (e.g., `2026.1.1`).

### Use of artificial intelligence

AI tools approved and offered by the U.S. Department of the Interior
were used to aid in the development of `v2026.0.0`. All AI-generated and
AI-assisted code, tests, and review were checked, approved, and often
re-worded or changed by the `migbirdHIP` package author before
inclusion. The only fully AI-generated code included in the package is
new unit tests, which are clearly labeled in code comments. Anthropic’s
Claude Opus 4.8 and Opus 5 (both via Perplexity) were the primary models
used to generate and review code, with the occasional use of other
available models in Perplexity (chosen by Perplexity Computer’s default
orchestration), in July 2026.

AI was used for tasks such as:

- Identifying gaps in unit test coverage and subsequently writing new
  unit tests, which are labeled in code comments.
- Assisting in function refactoring (but not independently rewriting
  functions).
- Agentic code review to help find typos and errors.

### Major changes & new features

- GitHub Actions
  - Run `R CMD check` nightly at 11pm Eastern and for every release.
  - Run a quick check that `migbirdHIP` can be installed and all unit
    tests pass with each push.
  - Added `pkgdown` build.
- Constants and variables
  - Changed `REF_CURRENT_SEASON` to `"2026"`
  - Added `REF_STATES_AF`, `REF_STATES_MF`, `REF_STATES_CF`, and
    `REF_STATES_PF`
- DESCRIPTION
  - Added [dtplyr](https://dtplyr.tidyverse.org) to Imports
- `constants.R`
  - `LOGIC_BAD_TITLE_ASSIGNMENT` added
- Functions
  - Deduplication functions now use
    [dtplyr](https://dtplyr.tidyverse.org) for sizable speed
    improvements (see
    [\#45](https://github.com/USFWS/migbirdHIP/issues/45))
  - Failure functions
    - Since
      [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
      already checks the file names that source `dl_state` and
      `dl_date`, it seems redundant to use `dlStateNAMessage()` and
      `dlDateNAMessage()`. Instead of moving them to `quality.R` with
      the rest of the quality check helpers, they were moved to
      `fails.R` and help inside of
      [`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md),
      since the most critical use of these fields is in the data export
      step.
      - [`failDLstate()`](https://usfws.github.io/migbirdHIP/reference/failDLstate.md) -
        refactored `dlStateNAMessage()`
      - [`failDLdate()`](https://usfws.github.io/migbirdHIP/reference/failDLdate.md) -
        refactored `dlDateNAMessage()`
    - [`failWidths()`](https://usfws.github.io/migbirdHIP/reference/failWidths.md) -
      NEW (see \#[72](https://github.com/USFWS/migbirdHIP/issues/72))
  - Data proofing helper functions in `proof.R`
    - New helper functions help split
      [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)
      into smaller parts. They filter to bad values, and some are reused
      by
      [`qualityMessages()`](https://usfws.github.io/migbirdHIP/reference/qualityMessages.md)
      helper functions.
      - [`getBadTitle()`](https://usfws.github.io/migbirdHIP/reference/getBadTitle.md) -
        also checks title assignments (new) in addition to value
        expectations
      - [`getBadFirstName()`](https://usfws.github.io/migbirdHIP/reference/getBadFirstName.md)
      - [`getBadMiddle()`](https://usfws.github.io/migbirdHIP/reference/getBadMiddle.md)
      - [`getBadLastName()`](https://usfws.github.io/migbirdHIP/reference/getBadLastName.md)
      - [`getBadSuffix()`](https://usfws.github.io/migbirdHIP/reference/getBadSuffix.md)
      - [`getBadAddress()`](https://usfws.github.io/migbirdHIP/reference/getBadAddress.md)
      - [`getBadCity()`](https://usfws.github.io/migbirdHIP/reference/getBadCity.md)
      - [`getBadState()`](https://usfws.github.io/migbirdHIP/reference/getBadState.md)
      - [`getBadZIP()`](https://usfws.github.io/migbirdHIP/reference/getBadZIP.md)
      - [`getBadBirthDate()`](https://usfws.github.io/migbirdHIP/reference/getBadBirthDate.md)
      - [`getBadHuntMigBirds()`](https://usfws.github.io/migbirdHIP/reference/getBadHuntMigBirds.md)
      - [`getBadRegYear()`](https://usfws.github.io/migbirdHIP/reference/getBadRegYear.md)
  - Data quality checking functions in `quality.R`
    - [`qualityMessages()`](https://usfws.github.io/migbirdHIP/reference/qualityMessages.md)
      - Refactored from function previously named `readMessages()`. This
        function is intended to be used after
        [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
        and before
        [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md).
      - Uses new, existing, and refactored helper functions:
        - [`qTitle()`](https://usfws.github.io/migbirdHIP/reference/qTitle.md) -
          NEW
        - [`qFirstName()`](https://usfws.github.io/migbirdHIP/reference/qFirstName.md) -
          NEW
        - [`qMiddle()`](https://usfws.github.io/migbirdHIP/reference/qMiddle.md) -
          NEW
        - [`qLastName()`](https://usfws.github.io/migbirdHIP/reference/qLastName.md) -
          NEW
        - [`qSuffix()`](https://usfws.github.io/migbirdHIP/reference/qSuffix.md) -
          NEW
        - [`qAddress()`](https://usfws.github.io/migbirdHIP/reference/qAddress.md)-
          NEW
        - [`qCity()`](https://usfws.github.io/migbirdHIP/reference/qCity.md) -
          NEW
        - [`qState()`](https://usfws.github.io/migbirdHIP/reference/qState.md) -
          NEW
        - [`qZIP()`](https://usfws.github.io/migbirdHIP/reference/qZIP.md) -
          NEW
        - [`qBirthDate()`](https://usfws.github.io/migbirdHIP/reference/qBirthDate.md) -
          NEW
        - [`qBirthDateRange()`](https://usfws.github.io/migbirdHIP/reference/qBirthDateRange.md) -
          NEW
        - [`qIssueDateRange()`](https://usfws.github.io/migbirdHIP/reference/qIssueDateRange.md) -
          NEW
        - [`qHuntMigBirds()`](https://usfws.github.io/migbirdHIP/reference/qHuntMigBirds.md) -
          previously `huntMigBirdsMessage()`
        - [`qBags()`](https://usfws.github.io/migbirdHIP/reference/qBags.md) -
          umbrella for existing
          [`zeroBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroBagsMessage.md),
          [`naBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/naBagsMessage.md)
          and
          [`nonDigitBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/nonDigitBagsMessage.md)
        - [`qRegistrationYear()`](https://usfws.github.io/migbirdHIP/reference/qRegistrationYear.md) -
          previously `badRegYearMessage()`
        - [`nonResidentMessage()`](https://usfws.github.io/migbirdHIP/reference/nonResidentMessage.md) -
          NEW - returns a message for files with 10% or more of `state`
          values that do not match `dl_state`.
        - [`interStateDuplicatesMessage()`](https://usfws.github.io/migbirdHIP/reference/interStateDuplicatesMessage.md) -
          NEW - returns a message if inter-state duplicates are
          detected.
        - [`missingPIIMessage()`](https://usfws.github.io/migbirdHIP/reference/missingPIIMessage.md) -
          existing
        - [`missingEmailsMessage()`](https://usfws.github.io/migbirdHIP/reference/missingEmailsMessage.md) -
          existing, edited to evaluate only files with more than 10
          registrations to avoid small files returning false positives.
        - [`testRecordMessage()`](https://usfws.github.io/migbirdHIP/reference/testRecordMessage.md) -
          existing
        - [`inLinePermitDNHMessage()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHMessage.md) -
          existing
        - [`permitFileBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/permitFileBagsMessage.md) -
          existing
      - Note: All issue checking functions and messages remain in
        `issuance.R` under
        [`issueMessages()`](https://usfws.github.io/migbirdHIP/reference/issueMessages.md)
  - [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
    - Reduce the column naming step to one line
    - New helper functions:
      - [`dropBlankLines()`](https://usfws.github.io/migbirdHIP/reference/dropBlankLines.md) -
        delete blank lines or lines that contain only `"Result"` (see
        [\#27](https://github.com/USFWS/migbirdHIP/issues/27))
      - [`readTimeMessage()`](https://usfws.github.io/migbirdHIP/reference/readTimeMessage.md) -
        moved the
        [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
        code chunk pertaining to read time duration to its own internal
        function.
  - Bag checking functions
    - [`bagCheck()`](https://usfws.github.io/migbirdHIP/reference/bagCheck.md)
      was refactored to be more efficient, and as a result, internal
      function `summarizeBadBags()` was deleted because it is no longer
      needed.
    - Run time of
      [`bagCheck()`](https://usfws.github.io/migbirdHIP/reference/bagCheck.md)
      decreased marginally.
    - [`bagCheck()`](https://usfws.github.io/migbirdHIP/reference/bagCheck.md)
      now reports count and proportion for `NA` bag values; previously,
      `NA` bag values were reported but not counted.
  - [`questionYear()`](https://usfws.github.io/migbirdHIP/reference/questionYear.md)
    - New function that questions if the user intends to supply a year
      value different than the current season year; replaces a
      duplicated code chunk in
      [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)
      and
      [`correct()`](https://usfws.github.io/migbirdHIP/reference/correct.md),
      and added to
      [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md).
  - [`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md)
    - Updated to conform to [ggplot2](https://ggplot2.tidyverse.org)
      version updates
      - Replaced `geom_segment()` `size` to `linewidth`
      - Replaced `geom_boxplot()` `size = 3` with `linewidth = 2` AND
        `outlier.size = 2`, which is narrower than previously.
      - Increased size of issue window segment width to make it easier
        for users to see if issue dates fall in, out, or across a
        season.
      - Updated colors.
      - Changed subtitle text for clarity.
      - Created
        [`issuePlotDateLabel()`](https://usfws.github.io/migbirdHIP/reference/issuePlotDateLabel.md)
        helper function to:
        - Avoid error with labeling dates in x-axis (see
          \#[88](https://github.com/USFWS/migbirdHIP/issues/88))
        - Avoid using
          [`scales::label_date_short()`](https://scales.r-lib.org/reference/label_date.html)
          which would require adding [scales](https://scales.r-lib.org)
          to Imports
  - Deduplication functions
    - Using
      [`duplicateFix()`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)
      on season data took a long time to run; about 7 minutes for 3.17
      million records. Changes to deduplication helper functions reduced
      run time to 1.23 minutes! See
      [\#45](https://github.com/USFWS/migbirdHIP/issues/45) for more
      details. Modifications used include:
      - Require [dtplyr](https://dtplyr.tidyverse.org) and use
        [`dtplyr::lazy_dt`](https://dtplyr.tidyverse.org/reference/lazy_dt.html)
      - Use a vectorized row sum rather than
        [`purrr::pmap_chr()`](https://purrr.tidyverse.org/reference/pmap.html)
        in
        [`duplicateAllOnes()`](https://usfws.github.io/migbirdHIP/reference/duplicateAllOnes.md)
      - Parse and format dates once, rather than inside each group, in
        [`duplicateNewest()`](https://usfws.github.io/migbirdHIP/reference/duplicateNewest.md)
      - Fully vectorize
        [`duplicateRecordType()`](https://usfws.github.io/migbirdHIP/reference/duplicateRecordType.md)
        by avoiding repeated `across(matches(...), as.numeric)`
        conversions by doing one conversion and two
        [`rowSums()`](https://rdrr.io/r/base/colSums.html)
  - Helper functions
    - New
      [`assignFlyway()`](https://usfws.github.io/migbirdHIP/reference/assignFlyway.md)
      function added, mainly to be used by the HIP dashboard.
    - [`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md)
      edited to calculate based on 13 possible errors per record, not 14
      ([`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)
      dropped `"test_record"`).
- Download report template
  - Add a plot of all issue dates received per state on the issuance
    tab.
  - Add a bullet point for total number of sample eligible registrations
    to the agenda tab.
  - Minor miscellaneous updates to match function changes, `ggplot2`
    deprecations, typos, etc.

### Minor changes / bug fixes

- Dependency updates
  - Following [dplyr](https://dplyr.tidyverse.org) `1.2.0` release
    notes, update `case_when()` with `recode_values()` and
    `replace_when()` as appropriate.
- Internal package data
  - Updated internal package test data to create character `title`
    values rather than numeric.
- Functions
  - Fail gracefully if input provided contains 0 rows (or data are
    dropped to result in 0 rows) for
    [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md),
    [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md),
    and
    [`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md).
  - Changed
    [`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md)
    legend to say “Registration year provided” to be clear that the
    value has not been changed/edited yet.
  - [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)
    no longer filters out test records, since this step is completed
    upstream in
    [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md).
  - `write_hip(split = FALSE)` had a bug in the file naming technique
    that is now resolved.
  - [`issueDecide()`](https://usfws.github.io/migbirdHIP/reference/issueDecide.md)
    renders `mdy(issue_date)` only once, and then deselects the column,
    rather than calling it seven times in the `case_when()`
  - [`fileCheck()`](https://usfws.github.io/migbirdHIP/reference/fileCheck.md)
    now runs [`list.files()`](https://rdrr.io/r/base/list.files.html)
    only once per path
  - Reduced repetition in
    [`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md)
  - Reduced `geom_vline()` repetition in
    [`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md)
- Testing
  - Added test files
    - Added: `test-quality.R`, `test-write_hip.R`, `test-writeReport.R`,
      `test-errorPlots.R`, `test-errorTables.R`, and `test-files.R`
    - Added `test-edge-inputs.R` to test empty (0-row),
      all-NA-in-key-columns, and single-row inputs.
  - Expanded unit testing
    - [`correctEmail()`](https://usfws.github.io/migbirdHIP/reference/correctEmail.md)
      testing and that
      [`correct()`](https://usfws.github.io/migbirdHIP/reference/correct.md)
      changes values to `NA` as expected in `test-correct.R`
    - [`duplicateFinder()`](https://usfws.github.io/migbirdHIP/reference/duplicateFinder.md)
      and
      [`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md)
      testing in `test-duplicates.R`
    - Testing passing messages in `test-glyphs.R` and `test-shifts.R`
    - Added tests for `registration_yr` field, `birth_date` field, and
      `zip` field in `test-proof.R`
    - Added tests for `unique = FALSE` and `season = TRUE` in
      `test-read_hip.R`
    - Updated `test-proof.R` to use `getBad` family of functions
    - Updated `test-fails.R` to evaluate
      [`failWidths()`](https://usfws.github.io/migbirdHIP/reference/failWidths.md)
    - Added
      [`bagCheck()`](https://usfws.github.io/migbirdHIP/reference/bagCheck.md)
      tests to `test-bags.R`
    - Added
      [`zipCheck()`](https://usfws.github.io/migbirdHIP/reference/zipCheck.md)
      tests to `text-clean.R`
- DESCRIPTION
  - Require R version `>= 4.5.0`
  - Imports
    - Update `lubridate` to `>=1.9.5`
    - Update `purrr` to `>= 1.2.1`
    - Update `data.table` to `>= 1.18.2`
    - Update `stringr` to `>= 1.6.0`
    - Update `readr` to `>= 2.2.0`
    - Update `dplyr` to `>= 1.2.0`
    - Update `rlang` to `>= 1.1.7`
    - Update `ggplot2` to `>= 4.0.2`
  - Suggests
    - Update `rmarkdown` to `>= 2.30`
    - Update `sf` to `>= 1.1-0`
- Recommend installation in `README` and vignette changed to
  [`pak::pak()`](https://pak.r-lib.org/reference/pak.html) now that
  `devtools::install_github()` has been deprecated.
- Updated package startup message in `zzz.R`
  - Version checking is now more robust; messages are more reliable and
    clear if a package version is out of date.

## migbirdHIP 1.4.13

Final definitive version for the 2025-2026 season.

### Minor changes / bug fixes

- Fix
  [`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md)
  bar order

## migbirdHIP 1.4.12

### Minor changes / bug fixes

- Fixed
  [`fileRename()`](https://usfws.github.io/migbirdHIP/reference/fileRename.md)
  bug that returned a message if dir contained subfolder.
- Updated `REF_BAGS` for FL `geese_bag` to only include `0`
- Download report template
  - Removed double spaces.
  - Moved missing email figure legend to the bottom; gives more space
    for x-axis state abbreviations.
  - Comma formatting for long numbers
    - Bag check table for `Count` field
    - Non-current records table for `Count outside window` field
    - Future records table for `Number of records`
    - Bar labels for plot of errors by field for both before and after
      correction
    - Causes of errors for top 3 fields table for `count` field
    - Causes of errors by state for `count` field
- [`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md)
  and
  [`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md)
  - Use [`format.default()`](https://rdrr.io/r/base/format.html) for bar
    labels to add commas to big numbers, which fixes the Errors by Field
    Before Correction plot and the Errors by State before and after
    correction plots in the download report.

## migbirdHIP 1.4.11

### Minor changes / bug fixes

- Updated `REF_BAGS` for ID `dove_bag` and MA `woodcock_bag`, changing
  `5` to `4`
- Bug fix for
  [`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md)
  to show correct bar labels and y position

## migbirdHIP 1.4.10

### Minor changes / bug fixes

- Replaced ID start date with `12/1/2024`.
- Fixed time to read in files message returned by
  [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md).

## migbirdHIP 1.4.9

### Minor changes / bug fixes

- [`issueDecide()`](https://usfws.github.io/migbirdHIP/reference/issueDecide.md)
  was returning `"overlap"` as a result in the `decision` field, and the
  `"overlap"` records were being dropped. This should not happen;
  `"overlap"` records should be assigned `"future"` or `"current"` and
  only `"past"` records are to be dropped.

## migbirdHIP 1.4.8

### Major changes & new features

- Evaluate Mississippi issue dates
  - Add MS to `REF_DATES`
  - No longer assign `"MS"` as decision in
    [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
  - No longer exclude `"MS"` from
    [`nDropped()`](https://usfws.github.io/migbirdHIP/reference/nDropped.md)
    or `dl_report.qmd`
- Added 3 new internal functions:
  - [`permitFileBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/permitFileBagsMessage.md)
    - Used by `readMessages()`.
    - Checks for non-zero bag values from permit file state/species
      combinations and returns a message to the console if they are
      detected.
  - [`failBTPI()`](https://usfws.github.io/migbirdHIP/reference/failBTPI.md)
    and
    [`failCR()`](https://usfws.github.io/migbirdHIP/reference/failCR.md)
    - Used by
      [`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md).
    - Moved checks from
      [`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md)
      to
      [`failBTPI()`](https://usfws.github.io/migbirdHIP/reference/failBTPI.md)
      and
      [`failCR()`](https://usfws.github.io/migbirdHIP/reference/failCR.md)
      to reduce maintenance of repeated assertions.
    - Added warning to
      [`failBTPI()`](https://usfws.github.io/migbirdHIP/reference/failBTPI.md)
      to return message to console if any BTPI permit records contain
      `0` for `dove_bag`.

### Minor changes / bug fixes

- [`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md)
  - Minor changes to match modern tidyverse syntax
  - Add check to ensure `REF_PMT_FILES` states are not in
    `zero_translations`
- `REF_BAGS` changes:
  - Bug fixes
    - Add lines for missing, existing, and expected bag values
      - `UT` `band_tailed_pigeon` bag `2` -\> stratum `2`
      - `MN` `cranes` bag `2` -\> stratum `2`
  - Update
    - Add line for `FL` `geese_bag` bag `0` -\> stratum `0`

## migbirdHIP 1.4.7

### Minor changes / bug fixes

- Add `stateBagValue` of `3` and `5` to `REF_BAGS` for `IA`
  `woodcock_bag`

## migbirdHIP 1.4.6

### Minor changes / bug fixes

- Updated `REF_DATES$issue_start` for `FL` to be `2025-04-01`

## migbirdHIP 1.4.5

### Minor changes / bug fixes

- Edited
  [`nDroppedClean()`](https://usfws.github.io/migbirdHIP/reference/nDroppedClean.md)
  function to capture records if they contain an `NA` value in any one
  bag field, in addition to existing rule that captures records if all
  bag fields are `NA`.

## migbirdHIP 1.4.4

### Minor changes / bug fixes

- Updated `REF_BAGS` `stateBagValue` to `5` for `CO` `dove_bag`
  (previously `4`)

## migbirdHIP 1.4.3

### Minor changes / bug fixes

- Edited
  [`proofBadEmails()`](https://usfws.github.io/migbirdHIP/reference/proofBadEmails.md)
  to allow all variations of ccTLDs to be accepted for `yahoo`,
  `hotmail`, `outlook`, and `live` email domains.
  - The previously strict filter only accepted `.com`, `.co.uk`, `.fr`,
    `.es`, `.ca`, and .`de` for `yahoo` and `hotmail` email addresses.
    In addition, only `.com` was accepted for `live` and `outlook` email
    addresses.
  - Now, `live` European ccTLDs and `yahoo` emails with Indian and
    Australian ccTLDs, to name a few examples, will pass without being
    flagged as errors.

## migbirdHIP 1.4.2

### Minor changes / bug fixes

- `NA` emails are not errors!
  - Updated
    [`proofBadEmails()`](https://usfws.github.io/migbirdHIP/reference/proofBadEmails.md)
    to allow `NA` values in `email` field.
  - Updated
    [`proofBadEmails()`](https://usfws.github.io/migbirdHIP/reference/proofBadEmails.md)
    function description so that the criteria for good emails is clear
    on the help page.
  - Updated `test-proof.R` unit testing.
  - Edited `create_fake_HIP_data.R` and overwrote internal test data
    stored in `/inst/extdata/DL0901/`, `/tests/testthat/data/DL0902/`,
    and `DF_TEST` objects by introducing `NA` values to `email` field.
- Edited `zzz.R` to return a startup error message in case the package
  version in the `DESCRIPTION` was modified before appending
  `REF_RELEASES` in `variables.R`.

## migbirdHIP 1.4.1

### Major changes & new features

- Import [ggnewscale](https://eliocamp.github.io/ggnewscale/) for use of
  [`ggnewscale::new_scale_color()`](https://eliocamp.github.io/ggnewscale/reference/new_scale.html)
  in `issue_plot()`.
- New internal
  [`nDropped()`](https://usfws.github.io/migbirdHIP/reference/nDropped.md)
  function (with helpers
  [`nDroppedClean()`](https://usfws.github.io/migbirdHIP/reference/nDroppedClean.md)
  and
  [`nDroppedCurrent()`](https://usfws.github.io/migbirdHIP/reference/nDroppedCurrent.md))
  to summarize the number of dropped HIP registrations (by state or not)
  at each step of pre-processing.
- Eliminated issue template.

### Minor changes / bug fixes

- Updated `DESCRIPTION` with updated `Imports` and `Suggests` package
  versions.
- Updated `sysdata.rda`
  - `REF_DATES` updated using 2025-2026 issue window dates.
  - `REF_ZIP_CODE` and `REF_BAGS` remain the same, using 2024-2025 zip
    codes and bag/strata conversions.
- Functions
  - [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md):
    add message that reports how long it took to read in `N` number of
    files
  - [`writeReport()`](https://usfws.github.io/migbirdHIP/reference/writeReport.md):
    add more dependency checks
  - `badRegYearMessage()`: summarize by source file because it is more
    informative
  - [`invalidDateMessage()`](https://usfws.github.io/migbirdHIP/reference/invalidDateMessage.md):
    created and added to
    [`issueMessages()`](https://usfws.github.io/migbirdHIP/reference/issueMessages.md)
  - `huntMigBirdsMessage()`: created and added to `readMessages()`
  - [`duplicateFix()`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md):
    bug fix, stopped returning `duplicate_id` field in the output tibble
  - [`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md)
    - Improved plot appearance for clarity of interpretation
    - Added more colors to the registration year palette to avoid an
      error of insufficient values in case of outliers
  - Refined message text for
    [`zipCheck()`](https://usfws.github.io/migbirdHIP/reference/zipCheck.md),
    [`cranePermitBagFix()`](https://usfws.github.io/migbirdHIP/reference/cranePermitBagFix.md),
    [`btpiPermitBagFix()`](https://usfws.github.io/migbirdHIP/reference/btpiPermitBagFix.md),
    [`regYearEditMessage()`](https://usfws.github.io/migbirdHIP/reference/regYearEditMessage.md),
    [`zeroDateMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroDateMessage.md),
    [`missingPIIMessage()`](https://usfws.github.io/migbirdHIP/reference/missingPIIMessage.md),
    [`missingEmailsMessage()`](https://usfws.github.io/migbirdHIP/reference/missingEmailsMessage.md),
    [`testRecordMessage()`](https://usfws.github.io/migbirdHIP/reference/testRecordMessage.md),
    [`zeroBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroBagsMessage.md),
    [`naBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/naBagsMessage.md),
    [`nonDigitBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/nonDigitBagsMessage.md),
    `huntMigBirdsMessage()`, `dlStateNAMessage()`, `dlDateNAMessage()`,
    [`inLinePermitDNHMessage()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHMessage.md),
    and `badRegYearMessage()`
- Changes to `dl_report.qmd` template
  - Add
    [`nDropped()`](https://usfws.github.io/migbirdHIP/reference/nDropped.md)
    summary to the Agenda tab
  - Refined excluded messages in `important_statistics`
  - Split data processing into smaller chunks to waste less time
    debugging
- Unit tests
  - Suppress verbose messages in `test-clean.R`
  - Add `test-nDropped.R`
- Linted all files
- Updated `WORDLIST`

## migbirdHIP 1.4.0

### Major changes & new features

- Changed contents of `R/sysdata.rda`
  - Fewer objects are now stored in `sysdata.rda` (reduced from 14 to
    7). This enhances transparency and reduces the number of objects
    that must be generated outside of the R package itself.
  - `sysdata.rda` now contains: `REF_ZIP_CODE`, `REF_BAGS`, `REF_DATES`,
    `REF_STATES_2SEASON`, `REF_STATES_1SEASON`, `REF_EMAIL_TLDS`, and
    `SF_HEXMAP`
  - Seven objects were moved to `constants.R`:
    - `ref_bagfields`, `abbr_usa`, `abbr_canada`, `pmt_inline`,
      `pmt_files`, `states_sdbr`, and `states_seaducks` were moved and
      renamed `REF_FIELDS_BAG`, `REF_ABBR_USA`, `REF_ABBR_CANADA`,
      `REF_PMT_INLINE`, `REF_PMT_FILES`, `REF_STATES_SD_BR`, and
      `REF_STATES_SD_ONLY`, respectively.
  - Two objects were dropped entirely. `MS_firstday` and `MS_lastday`
    are no longer needed by
    [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md).
  - Five internal data objects were renamed:
    - `REF_BAGS` (previously `hip_bags_ref`), `REF_DATES` (previously
      `licenses_ref`), `REF_ZIP_CODE` (previously `zip_code_ref`),
      `REF_STATES_2SEASON` (previously `states_twoseason`) and
      `SF_HEXMAP` (previously `hexmap`).
  - Two new objects were added: `REF_STATES_1SEASON` and
    `REF_EMAIL_TLDS`
  - `REF_DATES` was changed to no longer include
    `last_day_migbird_hunting` and `category` fields.
- Added test data
  - Fake HIP test data creation script stored under `data-raw/`
  - Test data containing fake HIP registrations stored as fixed-width
    `.txt` files under `inst/extdata/DL0901/`, to be used in testing or
    simulating
    [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
  - Miniature and tiny test data stored as exported `.rda` files under
    `data/`, to make it easier to demonstrate functions and run unit
    tests
    - `DF_TEST_MINI` contains 1,606 rows from 7 states (OR records to
      represent solo permit state, ME records to represent SD-only
      state, DE records to represent SD and BR state, ND records to
      represent CR state, UT records to represent BT state, CO records
      to represent CR and BT state, and IA records to represent non-BT,
      CR, SD, or BR state) and is formatted as though the data were just
      read in.
    - Six tiny test data objects:
      - `DF_TEST_TINI_READ` is a subset of `DF_TEST_MINI`, and contains
        3 rows formatted as though the data were just read in
      - `DF_TEST_TINI_CLEANED` is the result of running
        [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md)
        on `DF_TEST_TINI_READ`
      - `DF_TEST_TINI_CURRENT` is the result of running
        [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
        on `DF_TEST_TINI_CLEANED`
      - `DF_TEST_TINI_DEDUPED` is the result of running
        [`duplicateFix()`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)
        on `DF_TEST_TINI_CURRENT`
      - `DF_TEST_TINI_PROOFED` is the result of running
        [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)
        on `DF_TEST_TINI_DEDUPED`
      - `DF_TEST_TINI_CORRECTED` is the result of running
        [`correct()`](https://usfws.github.io/migbirdHIP/reference/correct.md)
        on `DF_TEST_TINI_PROOFED`
- Created `variables.R` to define seasonally changing variables in a
  central place.
  - `REF_CURRENT_SEASON` for current HIP season.
  - `REF_RELEASES` is a named vector of all `migbirdHIP` package
    releases and the corresponding season of HIP data that the version
    was intended for.
- Created `constants.R` to define variables in a central place and thus
  evaluate data consistently.
  - Variables are used across functions (e.g.,
    [`inLinePermitDNHMessage()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHMessage.md)
    and
    [`inLinePermitDNHFix()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHFix.md)
    both use `LOGIC_INLINE_PMT_DNH`) and are shared between functions
    and `testthat` files.
  - New naming convention helps users to use and find internal reference
    data objects more quickly by using uppercase letters and categorical
    prefixes (`REF_`, `LOGIC_`, `REGEX_`, and `SF_`).
- New functions
  - [`testRecordMessage()`](https://usfws.github.io/migbirdHIP/reference/testRecordMessage.md)
    added to
    [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
    and `testRecordFilter()` added to
    [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md)
    to find and filter out any testing records mistakenly sent to us by
    the states.
  - New
    [`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md)
    function added;
    [`duplicateFinder()`](https://usfws.github.io/migbirdHIP/reference/duplicateFinder.md)
    (previously named `findDuplicates()`) function no longer outputs a
    plot.
  - The
    [`zeroBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroBagsMessage.md)
    internal function is a new feature of
    [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
    that checks for records with all-zero bag values and returns a
    message to the console if they are detected.
  - Added
    [`errorTableSummary()`](https://usfws.github.io/migbirdHIP/reference/errorTableSummary.md)
    internal function to be used by
    [`errorTable()`](https://usfws.github.io/migbirdHIP/reference/errorTable.md).
  - Added four internal failure functions to reduce maintenance of
    assertions across other exported functions:
    [`failYear()`](https://usfws.github.io/migbirdHIP/reference/failyear.md),
    [`failProofed()`](https://usfws.github.io/migbirdHIP/reference/failproofed.md),
    [`failState()`](https://usfws.github.io/migbirdHIP/reference/failState.md),
    and
    [`failTF()`](https://usfws.github.io/migbirdHIP/reference/failTF.md).
- Refactored and edited functions
  - In an effort to improve the maintainability of the package code,
    steps were made toward modularity, clarity, and unit testing in some
    of the larger functions.
  - [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
    and
    [`issueAssign()`](https://usfws.github.io/migbirdHIP/reference/issueAssign.md)
    - Edited to evaluate registrations differently:
      - `last_day_migbird_hunting` is no longer used.
      - Two-season states now use `registration_yr` to assign `decision`
        as `"overlap"` (new label), and later `"current"` or `"future"`,
        if the `issue_date` for a registration occurred during the
        overlap window.
      - `registration_yr` is not changed for two-season states, only
        `"future"` assigned one-season states.
    - [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
      split into 9 new minor internal functions:
      - [`issuePrint()`](https://usfws.github.io/migbirdHIP/reference/issuePrint.md)
      - 8 functions for messages:
        [`issueMessages()`](https://usfws.github.io/migbirdHIP/reference/issueMessages.md),
        [`regYearEditMessage()`](https://usfws.github.io/migbirdHIP/reference/regYearEditMessage.md),
        [`zeroDateMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroDateMessage.md),
        [`badDateMessage()`](https://usfws.github.io/migbirdHIP/reference/badDateMessage.md),
        [`timeTravelMessage()`](https://usfws.github.io/migbirdHIP/reference/timeTravelMessage.md),
        [`futureDateMessage()`](https://usfws.github.io/migbirdHIP/reference/futureDateMessage.md),
        [`pastDateMessage()`](https://usfws.github.io/migbirdHIP/reference/pastDateMessage.md),
        and
        [`twoSeasonMessage()`](https://usfws.github.io/migbirdHIP/reference/twoSeasonMessage.md)
    - [`issueAssign()`](https://usfws.github.io/migbirdHIP/reference/issueAssign.md)
      split to use an internal
      [`issueDecide()`](https://usfws.github.io/migbirdHIP/reference/issueDecide.md)
      helper function.
  - [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)
    - Most logic used to identify and flag errors was moved to internal
      variables in the `constants.R` file, which are used by
      [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)
      and `test-proof.R`
    - First name and last name rules were slightly relaxed
      - No limit on number of spaces, apostrophes, or hyphens as long as
        they are not consecutive
      - For first name, apostrophe or letter allowed as first character
    - Email proofing rules now more strict
  - [`duplicateFinder()`](https://usfws.github.io/migbirdHIP/reference/duplicateFinder.md)
    - New internal function
      [`duplicateFields()`](https://usfws.github.io/migbirdHIP/reference/duplicateFields.md)
      uses `purrr` to significantly reduce redundancy in
      [`duplicateFinder()`](https://usfws.github.io/migbirdHIP/reference/duplicateFinder.md);
      overall, refactoring reduced the function’s length from 151 lines
      to 50 lines and improved processing speed.
  - [`duplicateFix()`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)
    - Broken down into 7 new minor internal functions
      ([`duplicateID()`](https://usfws.github.io/migbirdHIP/reference/duplicateID.md),
      [`duplicateNewest()`](https://usfws.github.io/migbirdHIP/reference/duplicateNewest.md),
      [`duplicateAllOnes()`](https://usfws.github.io/migbirdHIP/reference/duplicateAllOnes.md),
      [`duplicateAllOnesGroupSize()`](https://usfws.github.io/migbirdHIP/reference/duplicateAllOnesGroupSize.md),
      [`duplicateDecide()`](https://usfws.github.io/migbirdHIP/reference/duplicateDecide.md),
      [`duplicateRecordType()`](https://usfws.github.io/migbirdHIP/reference/duplicateRecordType.md)
      and
      [`duplicateSample()`](https://usfws.github.io/migbirdHIP/reference/duplicateSample.md)).
    - Overall, this refactor reduced deduplication run time by an
      average of 2 seconds on a frame with 270,000 records.
    - Records are no longer evaluated for having all-zero bags or
      periods instead of bag values because these records are now
      filtered out upstream during
      [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md).
  - [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
    - Broken down into 19 new minor internal functions
      ([`listFiles()`](https://usfws.github.io/migbirdHIP/reference/listFiles.md),
      [`ignorePermits()`](https://usfws.github.io/migbirdHIP/reference/ignorePermits.md),
      [`ignoreHolds()`](https://usfws.github.io/migbirdHIP/reference/ignoreHolds.md),
      [`ignoreLifetime()`](https://usfws.github.io/migbirdHIP/reference/ignoreLifetime.md),
      [`idBlankFiles()`](https://usfws.github.io/migbirdHIP/reference/idBlankFiles.md),
      [`dropBlankFiles()`](https://usfws.github.io/migbirdHIP/reference/dropBlankFiles.md),
      [`checkFileNameDateFormat()`](https://usfws.github.io/migbirdHIP/reference/checkFileNameDateFormat.md),
      [`checkFileNameStateAbbr()`](https://usfws.github.io/migbirdHIP/reference/checkFileNameStateAbbr.md),
      `readMessages()`,
      [`missingPIIMessage()`](https://usfws.github.io/migbirdHIP/reference/missingPIIMessage.md),
      [`missingEmailsMessage()`](https://usfws.github.io/migbirdHIP/reference/missingEmailsMessage.md),
      [`testRecordMessage()`](https://usfws.github.io/migbirdHIP/reference/testRecordMessage.md),
      [`zeroBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroBagsMessage.md),
      [`naBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/naBagsMessage.md),
      [`nonDigitBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/nonDigitBagsMessage.md),
      [`inLinePermitDNHMessage()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHMessage.md),
      `badRegYearMessage()`, `dlStateNAMessage()`, and
      `dlDateNAMessage()`).
    - More strict requirements must be met for data to be successfully
      read (e.g. instead of returning a message that file names are
      incorrectly formatted, this would stop the process).
  - [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md)
    - Broken down into 8 minor internal functions (2 previously used:
      `strataFix()` split into
      [`cranePermitBagFix()`](https://usfws.github.io/migbirdHIP/reference/cranePermitBagFix.md)
      and
      [`btpiPermitBagFix()`](https://usfws.github.io/migbirdHIP/reference/btpiPermitBagFix.md);
      and 6 new functions:
      [`namesToUppercase()`](https://usfws.github.io/migbirdHIP/reference/namesToUppercase.md),
      [`missingPIIFilter()`](https://usfws.github.io/migbirdHIP/reference/missingPIIFilter.md),
      [`moveSuffixes()`](https://usfws.github.io/migbirdHIP/reference/moveSuffixes.md),
      [`formatZip()`](https://usfws.github.io/migbirdHIP/reference/formatZip.md),
      [`zipCheck()`](https://usfws.github.io/migbirdHIP/reference/zipCheck.md),
      and
      [`inLinePermitDNHFix()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHFix.md))
    - Middle initial values are no longer changed (this now happens in
      [`correct()`](https://usfws.github.io/migbirdHIP/reference/correct.md)
      via
      [`correctMiddleInitial()`](https://usfws.github.io/migbirdHIP/reference/correctMiddleInitial.md))
  - [`correct()`](https://usfws.github.io/migbirdHIP/reference/correct.md)
    - Broken down into 4 minor internal functions
      ([`correctEmail()`](https://usfws.github.io/migbirdHIP/reference/correctEmail.md),
      [`correctTitle()`](https://usfws.github.io/migbirdHIP/reference/correctTitle.md),
      [`correctSuffix()`](https://usfws.github.io/migbirdHIP/reference/correctSuffix.md),
      [`correctMiddleInitial()`](https://usfws.github.io/migbirdHIP/reference/correctMiddleInitial.md)))
    - No longer filters out all-0 bag records (this now happens in
      [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md)
      via `naAndZeroBagsFilter()`)
    - Changes middle initial values if they are flagged in
      [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)
      via
      [`correctMiddleInitial()`](https://usfws.github.io/migbirdHIP/reference/correctMiddleInitial.md)
      (this step previously happened in
      [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md))
  - [`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md)
    - Edited to include more checks before files are written out.
    - New `type` param conditionally checks `record_type` field and
      `cranes`, `band_tailed_pigeon`, and `dove_bag` fields depending on
      the user input.
    - `.xlsx` and `.xls` file extensions are converted to `.csv`
- Renamed and moved functions
  - Renamed `strataCheck()` to
    [`bagCheck()`](https://usfws.github.io/migbirdHIP/reference/bagCheck.md)
    and renamed `strata.R` to `bags.R`.
    - [`bagCheck()`](https://usfws.github.io/migbirdHIP/reference/bagCheck.md)
      split and now uses an internal function, `summarizeBadBags()`.
    - Standardized using the phrase “bag values” over the term “strata”;
      HIP records contain bag values until they are written out and
      strata are assigned.
  - Renamed `renameFiles()` to
    [`fileRename()`](https://usfws.github.io/migbirdHIP/reference/fileRename.md)
    and moved from `renameFiles.R` to `files.R` to be grouped with
    [`fileCheck()`](https://usfws.github.io/migbirdHIP/reference/fileCheck.md)
    (previously in `fileCheck.R`).
  - `findDuplicates()` and `fixDuplicates()` renamed to
    [`duplicateFinder()`](https://usfws.github.io/migbirdHIP/reference/duplicateFinder.md)
    and
    [`duplicateFix()`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)
    to mirror naming conventions of other functions with the subject of
    the verb coming first
    (e.g. [`glyphFinder()`](https://usfws.github.io/migbirdHIP/reference/glyphFinder.md),
    [`glyphCheck()`](https://usfws.github.io/migbirdHIP/reference/glyphCheck.md)).
  - All functions related to duplicates moved to `duplicates.R`
    (previously separated into `findDuplicates.R` and `fixDuplicates.R`)
  - Renamed functions using camel case except for
    [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
    and
    [`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md)
    - `errorPlot_fields()`, `errorPlot_states()`, and `errorPlot_dl()`
      now named
      [`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
      [`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
      [`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md)
    - `errorLevel_errors_state()` and `errorLevel_errors_field()`
      renamed to
      [`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md)
      and
      [`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md)
  - [`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)
    no longer exported (used only in the download report), moved to the
    `errorPlots.R` script instead of being in its own file
- Deleted functions
  - Deleted `validate()`, `investigate()`, and `identicalBags()`.
  - Deleted demographic-oriented `outOfStateHunters()` and
    `youthHunters()` functions because they were not being used.
  - Deleted `recordLevel_errors_state()` function since it was not being
    used.
  - `sumLines()` deleted and
    [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
    param `sumlines` eliminated; no longer used and not considered
    useful moving forward.

### Minor changes / bug fixes

- Vignette
  - Since the package now contains comprehensive test data, there is no
    need to pre-compute the vignette using HIP data stored locally.
  - The `vignettes/man/migbirdHIP_workflow.Rmd.orig` file was deleted.
  - Unused files in the `vignettes/image/` subdirectory were deleted;
    used files were moved to `man/figures/`.
- Template `dl_report.qmd`:
  - Sorted errors by descending in “causes of errors by state” table.
  - Eliminated breaking error in report rendering if there is no field
    exceeding the error threshold.
  - Removed all-zero bag section from the Agenda tab.
  - Added comma formatting to long numbers.
  - On the Errors tab, moved the “Bad zip codes” section to its own
    sub-header after “Causes of errors for top 3 fields”.
- Add `zzz.R`
  - Run package startup message which returns the installed `migbirdHIP`
    package version and which season of HIP data the package version is
    compatible with.
- Deleted `globals.R` and switched to data masking with
  [rlang](https://rlang.r-lib.org) pronoun `.data`.
- Incorporated some linting with `lintr` and added `.lintr` to
  `.gitignore` and `.Rbuildignore`.
- Almost all data params changed to be less ambiguous (e.g. `x` now
  `cleaned_data`, `proofed_data`, etc).
- [tibble](https://tibble.tidyverse.org/) no longer a required import
- [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
  now catches file names with incorrect MMDDYYYY or DDMMYYYY date
  format.
- [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
  now returns an error for NA values in `record_key` field.
- Added release tags to README.
- Replace deprecated `~ .x` anonymous function notation with `\(x)`.

## migbirdHIP 1.3.0

### Major changes & new features

- Edited
  [`shiftCheck()`](https://usfws.github.io/migbirdHIP/reference/shiftCheck.md)
  to return a summary of shift errors rather than just a table of record
  id values.
- Edited
  [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md),
  [`issueAssign()`](https://usfws.github.io/migbirdHIP/reference/issueAssign.md),
  and
  [`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md)
  to accommodate new rules in evaluating if a record is current. All
  records are now current unless their `issue_date` falls before
  `issue_start` or after the last day of migratory bird hunting in the
  record’s state.
- Edited
  [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md) and
  `errorPlot_fields()` to no longer flag and/or plot youth hunters
  (hunters with birth year \< 16 years ago).

### Minor changes / bug fixes

- Template `dl_report.qmd`
  - Excluded future data line from agenda tab
  - Minor adjustments for edge cases in which summary variables are
    `NULL`
  - Added clarification to table captions
  - Eliminated irrelevant error and warning message printouts from
    rendered report
  - Remove inconsequential “bad bags” from Agenda tab; do not report 1s
    received instead of 0s, or 0s received instead of 1s
  - Edited “bad bags” section of agenda tab to return a message in the
    edge case of 0% of a file containing bad bag values
  - Edited the Issuance tab to reflect new rules in evaluating if a
    record is current
- Edited `identicalBags()` function to exclude matching coots_snipe and
  rails_gallinules from MI in output; this state uses the response from
  one question to populate both fields.
- Edited
  [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
  function to exclude “hold” subdirectories when reading season HIP
  data.
- Updated R dependency to v4.4.0.
- Added programmatic stops to all functions to safeguard against running
  with incorrect/invalid parameters.
- Reduced variation in parameter names:
  - `distinct` changed to `unique` for
    [`pullErrors()`](https://usfws.github.io/migbirdHIP/reference/pullErrors.md)
  - `output` changed to `return` for `outOfStateHunters()`
  - `assigned_data` changed to `x` for
    [`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md)
  - `data` changed to `x` for
    [`glyphCheck()`](https://usfws.github.io/migbirdHIP/reference/glyphCheck.md),
    [`glyphFinder()`](https://usfws.github.io/migbirdHIP/reference/glyphFinder.md),
    [`issueAssign()`](https://usfws.github.io/migbirdHIP/reference/issueAssign.md),
    [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md),
    and
    [`shiftCheck()`](https://usfws.github.io/migbirdHIP/reference/shiftCheck.md)

## migbirdHIP 1.2.8

### Major changes & new features

- Added a `NEWS.md` file to track changes to the package.
- Added package documentation page `man/migbirdHIP-package.Rd`
- New
  [`fileCheck()`](https://usfws.github.io/migbirdHIP/reference/fileCheck.md)
  function: checks if any files in the input folder have already been
  written to processed folder.
- New
  [`shiftCheck()`](https://usfws.github.io/migbirdHIP/reference/shiftCheck.md)
  function: find and print any rows that have a line shift error with
  number of positions shifted.
- New `identicalBags()` function: returns output if any columns are
  exactly the same in a file; does not return “no season” matches.
- New
  [`glyphCheck()`](https://usfws.github.io/migbirdHIP/reference/glyphCheck.md)
  function: pull and view any non-UTF-8 characters in the raw data;
  helps guide manual fixes to read in the HIP files without line shifts.
  - [`glyphFinder()`](https://usfws.github.io/migbirdHIP/reference/glyphFinder.md)
    no longer exported, now used internally inside of
    [`glyphCheck()`](https://usfws.github.io/migbirdHIP/reference/glyphCheck.md)
- Added 3 new internal package functions (`errorLevel_errors_field()`,
  `errorLevel_errors_state()`, and `recordLevel_errors_state()`), which
  are used inside
  [`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md),
  `errorPlot_fields()`, and `errorPlot_states()`. They reduce code
  redundancy and ensure updates happen universally.
- Added 2 new internal package functions
  ([`issueAssign()`](https://usfws.github.io/migbirdHIP/reference/issueAssign.md)
  and `issuePlot`), which are used inside of
  [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
  and by the download report (`dl_report.qmd`).
- Added internal function `strataFix()` to be used inside of
  [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md) to
  resolve false permit labels. This function edits strata values for
  `band_tailed_pigeon` and `crane` from states that submit permit files
  for crane and band-tailed pigeons; values changed from `"2"` to `"0"`.
- Edited
  [`writeReport()`](https://usfws.github.io/migbirdHIP/reference/writeReport.md)
  to render quarto documents.
- Edited
  [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
  to place more emphasis on `issue_date` to determine relevancy of a
  record. The function no longer exports future and past data as `.csv`
  files. Past data are still filtered out from the returned tibble.
  Output messages indicate if future data exist.
- Edited
  [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md)
  function:
  - Filter out any rows that contain a bag value other than a single
    digit
  - Eliminated address cleaning
  - Moved zip code checking and messaging to
    [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md)
    from
    [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md);
    now checks on entire zip code, not just prefix. Remove ending `0`
    when `zip` value is 10 digits long.
  - Changed Oregon solo permit `hunt_mig_birds` field when it equals
    `"0"` to `"2"`. For context, a solo permit contains a `"2"` in at
    least one of the `band_tailed_pigeon`, `brant`, or `seaduck` fields
    and contains `"0"` in all other bag fields.
- Edited
  [`correct()`](https://usfws.github.io/migbirdHIP/reference/correct.md)
  to remove any records with value of `"0"` or `NA` value in every bag
  field; improved `email` field cleaning and repair.
- Edited `strataCheck()` to return two additional fields in output; 1)
  number of bad strata and 2) proportion of bad strata. The function now
  checks for permit species coming during regular HIP and returns them
  as erroneous (e.g. NM `band_tailed_pigeon` = `"2"`).
- Edited
  [`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md)
  to set any state/species combinations without a season to have strata
  of `"0"`; bad bag values remain NA.
- Edited `sumLines()` to improve speed and efficiency. In addition, the
  function now returns a data table with the sum of lines per file
  instead of a single number. No longer exported; set as internal
  function.
- Edited
  [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
  to eliminate encoding check and optionally use `sumLines()` function
  to ensure all lines were read in. Returns a message if any records
  contain a bag value other than a single digit. In addition, now
  converts blank strings to `NA`.
- Edited `validate()` to return `source_file` field and filter out
  states and species with no season from function output.
- Edited `investigate()` to no longer be exported; it works inside of
  `validate()` to return a more detailed output. This replaces the
  previous workflow of running `investigate()` separately.
- Removed `manualFix()` function because it is no longer relevant to the
  package.
- Removed `shiftFix()` because line shift errors cannot be fixed
  programmatically on a reliable basis.
- Templates
  - New Quarto `dl_report.qmd` replaced RMarkdown `dl_report.Rmd`.
    - The new Quarto layout allows tabset panels which divides content
      into sections that can be more easily read and focused on by the
      user. Tabset panels were also incorporated for before and after
      plots to show proportion of errors that are corrected during
      pre-processing.
    - A new summary section distills the findings of the functions
      overall for the user to discern the most important issues from the
      HIP files that were processed. This is partly accomplished with
      the use of a `catch_messages()` function created only for use in
      the `dl_report.qmd` and is not exported or contained within the
      `migbirdHIP` package internally. The `catch_messages()` function
      wraps around pre-processing functions (such as
      [`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md),
      [`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md),
      [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md),
      etc) and captures messages in a list so that they can be returned
      as readable bullet points.
    - A new map displays time lag of files received from 49 states in a
      hexagonal representation of the continental US.
    - Emojis are printed with output text to quickly indicate to readers
      whether issues ❌ need attention or ✔️ are not concerning.
    - Sections added as needed to report on new function output (see
      above for which new functions were added).
    - A new section lists any states that were excluded from the output
      when they submitted data for that download (e.g. all records were
      issued in the past and are not eligible for the current season;
      perhaps sent by mistake).
  - Eliminated `season_report.Rmd` template
- Imports
  - Removed `magrittr` and `rmarkdown`
  - Added `quarto` and `sf`
- Suggests
  - Added `spelling`
- Internal package data (`sysdata.rda`)
  - Added vectors of abbreviated US territories and Canada
    provinces/territories, both updated to include missing abbreviations
    from previous versions and remove redundant abbreviations
  - Added vector of bag field names
  - Added vector of two-season states
  - Added vectors of seaduck and brant states, seaduck-only states, and
    two-season states
  - Added hexmap grid for download report
  - Added tibbles of permit file states/species and states/species of
    permits received inline
  - Updated zip code reference table, bag reference table, license
    window reference table, and MS reference dates

### Minor changes / bug fixes

- License changed to CC0 (previously Public Domain), which was causing a
  warning in `devtools::check()`
- Refactored
  [`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md)
  to eliminate redundancy; replaced repeated `left_join()` with for loop
- Refactored `findDuplicates()` by throwing an error message for a bad
  string supplied to the `return` parameter at the start, which reduces
  wait time for failure.
  - Investigated replacing `findDuplicates()` redundancy of searching
    for duplicate fields using a `for` loop or
    [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html),
    but this change added 20+ seconds of processing time so left the
    redundancy as-is.
- Refactored all functions that take a path parameter to add a forward
  slash to the end each supplied path if not included by the user.
- Replaced superseded
  [`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html)
  with
  [`tidyr::separate_wider_delim()`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html)
  or
  [`tidyr::separate_wider_position()`](https://tidyr.tidyverse.org/reference/separate_wider_delim.html)
- Replaced
  [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
  with
  [`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html)
  since returning more than 1 row per group was deprecated in
  `dplyr 1.1.0`
- Replaced `ggplot::stat()` with `ggplot::after_stat()`, since the
  former was deprecated in `ggplot2 3.4.0`
- Replaced tidy pipes `%>%` and `%<>%` with base R pipe `|>` for
  increased speed and reduced dependency on tidyverse packages.
- Edited `DESCRIPTION` file:
  - Changed package description
  - Set language to `en-US`
  - Added a URL to the Harvest Information Program website
- Incorporated `usethis::use_spell_check()` to package checking
  workflow, which added an `inst/WORDLIST` file (whitelisted words) to
  the package.

## migbirdHIP 1.2.7

- Updated strata reference table in
  [sysdata.rda](https://github.com/USFWS/migbirdHIP/commit/b716713a96c1aeb25f4a7f32d12b98ecc2ac1b0e)
- Added kable summary tables to horizontal repetition checks in the
  [download report
  template](https://github.com/USFWS/migbirdHIP/commit/46a87cdaa025b7a4f229225a6afe65ee65853b87)
- Due to new kables, updated
  [DESCRIPTION](https://github.com/USFWS/migbirdHIP/commit/3bd4354a7769180aefa3743daa116393e7a4497e)
  to include kableExtra as a Suggests (and added other packages used in
  the dl_template.Rmd not previously included)
- Updated
  [.Rbuildignore](https://github.com/USFWS/migbirdHIP/commit/0f5df5b28c9f66fee264eee44cd4a5a8a5f4628c)
  to reduce R CMD check notes

## migbirdHIP 1.2.6

- Added
  [`glyphFinder()`](https://usfws.github.io/migbirdHIP/reference/glyphFinder.md)
  function
- Revisions to
  [`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
  and [`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)
- Revisions to `dl_report.Rmd` template

## migbirdHIP 1.2.5

- Operational for 2022-2023 season

## migbirdHIP 1.2.4

- Final version compatible with 2021-2022 season data

## migbirdHIP 1.2.3

- Updated `fixDuplicates()` and `validate()`

## migbirdHIP 1.2.2

- Updated vignette
- Updated readme
- Updated description

## migbirdHIP 1.2.1

- Updated package name to `migbirdHIP` in function documentation

## migbirdHIP 1.2.0

- Package renamed `migbirdHIP`
