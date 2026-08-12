# Drop blank lines

The internal `dropBlankLines` function is used inside of
[`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
to filter out any rows with `NA` in every field or a blank line
beginning with `Result`.

## Usage

``` r
dropBlankLines(raw_data)
```

## Arguments

- raw_data:

  Intermediate raw data tibble

## See also

Other reading functions:
[`checkFileNameDateFormat()`](https://usfws.github.io/migbirdHIP/reference/checkFileNameDateFormat.md),
[`checkFileNameStateAbbr()`](https://usfws.github.io/migbirdHIP/reference/checkFileNameStateAbbr.md),
[`dropBlankFiles()`](https://usfws.github.io/migbirdHIP/reference/dropBlankFiles.md),
[`idBlankFiles()`](https://usfws.github.io/migbirdHIP/reference/idBlankFiles.md),
[`ignoreHolds()`](https://usfws.github.io/migbirdHIP/reference/ignoreHolds.md),
[`ignoreLifetime()`](https://usfws.github.io/migbirdHIP/reference/ignoreLifetime.md),
[`ignorePermits()`](https://usfws.github.io/migbirdHIP/reference/ignorePermits.md),
[`listFiles()`](https://usfws.github.io/migbirdHIP/reference/listFiles.md),
[`readTimeMessage()`](https://usfws.github.io/migbirdHIP/reference/readTimeMessage.md),
[`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

## Author

Abby Walter, <abby_walter@fws.gov>
