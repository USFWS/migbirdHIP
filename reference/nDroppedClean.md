# Summary table of how many registrations were dropped by clean

Internal function that helps
[`nDropped`](https://usfws.github.io/migbirdHIP/reference/nDropped.md)
by summarizing the number of dropped registrations by
[`clean`](https://usfws.github.io/migbirdHIP/reference/clean.md).

## Usage

``` r
nDroppedClean(raw_data, clean_data, by_state = FALSE)
```

## Arguments

- raw_data:

  The object created after reading in data with
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

- clean_data:

  The object created after cleaning data with
  [`clean`](https://usfws.github.io/migbirdHIP/reference/clean.md)

- by_state:

  TRUE summarizes the number of dropped records by state; FALSE
  (default) does not summarize

## See also

Other summary functions:
[`nDropped()`](https://usfws.github.io/migbirdHIP/reference/nDropped.md),
[`nDroppedCurrent()`](https://usfws.github.io/migbirdHIP/reference/nDroppedCurrent.md)

## Author

Abby Walter, <abby_walter@fws.gov>
