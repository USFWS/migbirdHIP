# Drop blank files

The internal `dropBlankFiles` function is used inside of
[`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
to return an error message if blank files exist in the directory, and
remove them from the file list so they are not read in.

## Usage

``` r
dropBlankFiles(filelist)
```

## Arguments

- filelist:

  The file list tibble created by
  [`listFiles`](https://usfws.github.io/migbirdHIP/reference/listFiles.md)

## See also

Other reading functions:
[`checkFileNameDateFormat()`](https://usfws.github.io/migbirdHIP/reference/checkFileNameDateFormat.md),
[`checkFileNameStateAbbr()`](https://usfws.github.io/migbirdHIP/reference/checkFileNameStateAbbr.md),
[`dropBlankLines()`](https://usfws.github.io/migbirdHIP/reference/dropBlankLines.md),
[`idBlankFiles()`](https://usfws.github.io/migbirdHIP/reference/idBlankFiles.md),
[`ignoreHolds()`](https://usfws.github.io/migbirdHIP/reference/ignoreHolds.md),
[`ignoreLifetime()`](https://usfws.github.io/migbirdHIP/reference/ignoreLifetime.md),
[`ignorePermits()`](https://usfws.github.io/migbirdHIP/reference/ignorePermits.md),
[`listFiles()`](https://usfws.github.io/migbirdHIP/reference/listFiles.md),
[`readTimeMessage()`](https://usfws.github.io/migbirdHIP/reference/readTimeMessage.md),
[`read_hip()`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

## Author

Abby Walter, <abby_walter@fws.gov>
