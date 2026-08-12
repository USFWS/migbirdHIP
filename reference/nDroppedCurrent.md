# Summary table of how many registrations were dropped by issueCheck

Internal function that helps
[`nDropped`](https://usfws.github.io/migbirdHIP/reference/nDropped.md)
by summarizing the number of dropped registrations by
[`issueCheck`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md).

## Usage

``` r
nDroppedCurrent(clean_data, current_data, year)
```

## Arguments

- clean_data:

  The object created after cleaning data with
  [`clean`](https://usfws.github.io/migbirdHIP/reference/clean.md)

- current_data:

  The object created after filtering to current data with
  [`issueCheck`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)

- year:

  The year of the HIP season (e.g. 2022 for the 2022-2023 season)

## See also

Other summary functions:
[`nDropped()`](https://usfws.github.io/migbirdHIP/reference/nDropped.md),
[`nDroppedClean()`](https://usfws.github.io/migbirdHIP/reference/nDroppedClean.md)

## Author

Abby Walter, <abby_walter@fws.gov>
