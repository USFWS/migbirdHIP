# Return message if registrations are detected from two-season overlap windows

The internal `twoSeasonMessage` function is used inside of
[`issueMessages`](https://usfws.github.io/migbirdHIP/reference/issueMessages.md)
and
[`issueCheck`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
to return a message if registrations are detected from two-season states
with `issue_date` value(s) that occur during the overlap window.

## Usage

``` r
twoSeasonMessage(issue_assignments)
```

## Arguments

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
[`issueMessages()`](https://usfws.github.io/migbirdHIP/reference/issueMessages.md),
[`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md),
[`issuePlotDateLabel()`](https://usfws.github.io/migbirdHIP/reference/issuePlotDateLabel.md),
[`issuePrint()`](https://usfws.github.io/migbirdHIP/reference/issuePrint.md),
[`pastDateMessage()`](https://usfws.github.io/migbirdHIP/reference/pastDateMessage.md),
[`regYearEditMessage()`](https://usfws.github.io/migbirdHIP/reference/regYearEditMessage.md),
[`timeTravelMessage()`](https://usfws.github.io/migbirdHIP/reference/timeTravelMessage.md),
[`zeroDateMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroDateMessage.md)

## Author

Abby Walter, <abby_walter@fws.gov>
