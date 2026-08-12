# Read in data

Compile data from state-exported text files by providing a path to the
download directory.

## Usage

``` r
read_hip(path, unique = TRUE, state = NA, season = FALSE)
```

## Arguments

- path:

  File path to the folder containing HIP .txt files

- unique:

  Return a distinct frame? Defaults to TRUE

- state:

  When specified, reads in download data from a specified state. Must
  match a two-letter abbreviation for a US state (excluding HI).

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
[`listFiles()`](https://usfws.github.io/migbirdHIP/reference/listFiles.md),
[`readTimeMessage()`](https://usfws.github.io/migbirdHIP/reference/readTimeMessage.md)

## Author

Abby Walter, <abby_walter@fws.gov>
