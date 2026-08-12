# Return message if an issue date is after the file was submitted

The internal `timeTravelMessage` function is used inside of
[`issueMessages`](https://usfws.github.io/migbirdHIP/reference/issueMessages.md)
and
[`issueCheck`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)
to return a message if `issue_date` values are after the date the file
was submitted.

## Usage

``` r
timeTravelMessage(issue_assignments)
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
[`twoSeasonMessage()`](https://usfws.github.io/migbirdHIP/reference/twoSeasonMessage.md),
[`zeroDateMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroDateMessage.md)

## Author

Abby Walter, <abby_walter@fws.gov>
