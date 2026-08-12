# Check issue dates and license years

After cleaning the data with
[`clean`](https://usfws.github.io/migbirdHIP/reference/clean.md), ensure
each record is assigned the appropriate `registration_yr`.

## Usage

``` r
issueCheck(clean_data, year, plot = FALSE)
```

## Arguments

- clean_data:

  The object created after cleaning data with
  [`clean`](https://usfws.github.io/migbirdHIP/reference/clean.md)

- year:

  The year of the HIP season (e.g. 2022 for the 2022-2023 season)

- plot:

  Create a plot? Default is FALSE

## See also

Other issuance functions:
[`badDateMessage()`](https://usfws.github.io/migbirdHIP/reference/badDateMessage.md),
[`futureDateMessage()`](https://usfws.github.io/migbirdHIP/reference/futureDateMessage.md),
[`invalidDateMessage()`](https://usfws.github.io/migbirdHIP/reference/invalidDateMessage.md),
[`issueAssign()`](https://usfws.github.io/migbirdHIP/reference/issueAssign.md),
[`issueDecide()`](https://usfws.github.io/migbirdHIP/reference/issueDecide.md),
[`issueMessages()`](https://usfws.github.io/migbirdHIP/reference/issueMessages.md),
[`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md),
[`issuePlotDateLabel()`](https://usfws.github.io/migbirdHIP/reference/issuePlotDateLabel.md),
[`issuePrint()`](https://usfws.github.io/migbirdHIP/reference/issuePrint.md),
[`pastDateMessage()`](https://usfws.github.io/migbirdHIP/reference/pastDateMessage.md),
[`regYearEditMessage()`](https://usfws.github.io/migbirdHIP/reference/regYearEditMessage.md),
[`timeTravelMessage()`](https://usfws.github.io/migbirdHIP/reference/timeTravelMessage.md),
[`twoSeasonMessage()`](https://usfws.github.io/migbirdHIP/reference/twoSeasonMessage.md),
[`zeroDateMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroDateMessage.md)

## Author

Abby Walter, <abby_walter@fws.gov>
