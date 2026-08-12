# Check HIP file name state abbreviations

The internal `checkFileNameStateAbbr` function is used inside of
[`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)
to return an error message if any file does not have an state
abbreviation from the expected 49 continental states.

## Usage

``` r
checkFileNameStateAbbr(file_list_vector)
```

## Arguments

- file_list_vector:

  A file list vector

## See also

Other reading functions:
[`checkFileNameDateFormat()`](https://usfws.github.io/migbirdHIP/reference/checkFileNameDateFormat.md),
[`dropBlankFiles()`](https://usfws.github.io/migbirdHIP/reference/dropBlankFiles.md),
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
