# Return messages to console for issueCheck insights

The internal `issueMessages` function is used inside of
[`issueCheck`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
to return messages for bad issue_date values, count of past
registrations, count of future registrations, and 2-season overlap
registrations.

## Usage

``` r
issueMessages(clean_data, issue_assignments)
```

## Arguments

- clean_data:

  The product of
  [`clean`](https://usfws.github.io/migbirdHIP/reference/clean.md)

- issue_assignments:

  An intermediate tibble in
  [`issueCheck`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)

## See also

Other issuance functions:
[`badDateMessage()`](https://usfws.github.io/migbirdHIP/reference/badDateMessage.md),
[`futureDateMessage()`](https://usfws.github.io/migbirdHIP/reference/futureDateMessage.md),
[`invalidDateMessage()`](https://usfws.github.io/migbirdHIP/reference/invalidDateMessage.md),
[`issueAssign()`](https://usfws.github.io/migbirdHIP/reference/issueAssign.md),
[`issueCheck()`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md),
[`issueDecide()`](https://usfws.github.io/migbirdHIP/reference/issueDecide.md),
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
