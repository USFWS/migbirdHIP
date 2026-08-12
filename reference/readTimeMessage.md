# Read time message

The internal `readTimeMessage` function returns a message to the console
informing the user about how long it took to read in the data with
[`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md).

## Usage

``` r
readTimeMessage(raw_data, starttime, endtime)
```

## Arguments

- raw_data:

  Intermediate raw data tibble

- starttime:

  Start time

- endtime:

  End time

## See also

Other reading functions:
[`checkFileNameDateFormat()`](https://usfws.github.io/migbirdHIP/reference/checkFileNameDateFormat.md),
[`checkFileNameStateAbbr()`](https://usfws.github.io/migbirdHIP/reference/checkFileNameStateAbbr.md),
[`dropBlankFiles()`](https://usfws.github.io/migbirdHIP/reference/dropBlankFiles.md),
[`dropBlankLines()`](https://usfws.github.io/migbirdHIP/reference/dropBlankLines.md),
[`idBlankFiles()`](https://usfws.github.io/migbirdHIP/reference/idBlankFiles.md),
[`ignoreHolds()`](https://usfws.github.io/migbirdHIP/reference/ignoreHolds.md),
[`ignoreLifetime()`](https://usfws.github.io/migbirdHIP/reference/ignoreLifetime.md),
[`ignorePermits()`](https://usfws.github.io/migbirdHIP/reference/ignorePermits.md),
[`listFiles()`](https://usfws.github.io/migbirdHIP/reference/listFiles.md),
[`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

## Author

Abby Walter, <abby_walter@fws.gov>
