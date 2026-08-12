# Plot issue date errors

The internal `issuePlot` function plots bad `issue_date` values.

## Usage

``` r
issuePlot(issue_assignments, year)
```

## Arguments

- issue_assignments:

  An intermediate tibble in
  [`issueCheck`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)

- year:

  The year of the HIP season (e.g. 2022 for the 2022-2023 season)

## See also

Other issuance functions:
[`badDateMessage()`](https://usfws.github.io/migbirdHIP/reference/badDateMessage.md),
[`futureDateMessage()`](https://usfws.github.io/migbirdHIP/reference/futureDateMessage.md),
[`invalidDateMessage()`](https://usfws.github.io/migbirdHIP/reference/invalidDateMessage.md),
[`issueAssign()`](https://usfws.github.io/migbirdHIP/reference/issueAssign.md),
[`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md),
[`issueDecide()`](https://usfws.github.io/migbirdHIP/reference/issueDecide.md),
[`issueMessages()`](https://usfws.github.io/migbirdHIP/reference/issueMessages.md),
[`issuePlotDateLabel()`](https://usfws.github.io/migbirdHIP/reference/issuePlotDateLabel.md),
[`issuePrint()`](https://usfws.github.io/migbirdHIP/reference/issuePrint.md),
[`pastDateMessage()`](https://usfws.github.io/migbirdHIP/reference/pastDateMessage.md),
[`regYearEditMessage()`](https://usfws.github.io/migbirdHIP/reference/regYearEditMessage.md),
[`timeTravelMessage()`](https://usfws.github.io/migbirdHIP/reference/timeTravelMessage.md),
[`twoSeasonMessage()`](https://usfws.github.io/migbirdHIP/reference/twoSeasonMessage.md),
[`zeroDateMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroDateMessage.md)

Other plotting functions:
[`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md),
[`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md),
[`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
[`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
[`issuePlotDateLabel()`](https://usfws.github.io/migbirdHIP/reference/issuePlotDateLabel.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

## Author

Abby Walter, <abby_walter@fws.gov>
