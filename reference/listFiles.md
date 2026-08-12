# List files

The internal `listFiles` function is used inside of
[`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
and creates a tibble of the HIP .txt files to be read in from the
provided directory.

## Usage

``` r
listFiles(path, season)
```

## Arguments

- path:

  File path to the folder containing HIP .txt files

- season:

  If set as TRUE, selects only folders starting with "DL" in a a
  season's upper-level directory

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
[`readTimeMessage()`](https://usfws.github.io/migbirdHIP/reference/readTimeMessage.md),
[`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

## Author

Abby Walter, <abby_walter@fws.gov>
