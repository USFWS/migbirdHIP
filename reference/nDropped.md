# Summary table of how many registrations were dropped

Internal function that summarizes the number of dropped registrations.

## Usage

``` r
nDropped(
  raw_data,
  clean_data,
  current_data,
  deduplicated_data,
  year,
  by_state = FALSE
)
```

## Arguments

- raw_data:

  The object created after reading in data with
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

- clean_data:

  The object created after cleaning data with
  [`clean`](https://usfws.github.io/migbirdHIP/reference/clean.md)

- current_data:

  The object created after filtering to current data with
  [`issueCheck`](https://usfws.github.io/migbirdHIP/reference/issueCheck.md)

- deduplicated_data:

  The object created after deduplicating data with
  [`duplicateFix`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)

- year:

  The year of the HIP season (e.g. 2022 for the 2022-2023 season)

- by_state:

  TRUE summarizes the number of dropped records by state; FALSE
  (default) does not summarize

## See also

Other summary functions:
[`nDroppedClean()`](https://usfws.github.io/migbirdHIP/reference/nDroppedClean.md),
[`nDroppedCurrent()`](https://usfws.github.io/migbirdHIP/reference/nDroppedCurrent.md)

## Author

Abby Walter, <abby_walter@fws.gov>
