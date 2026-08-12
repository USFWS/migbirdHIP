# Raw HIP file name correction

This function overwrites HIP filenames. Files in the supplied directory
are renamed by converting the Julian date to YYYYMMDD format. State
abbreviations that are in lowercase format are capitalized.

## Usage

``` r
fileRename(path, year)
```

## Arguments

- path:

  Directory to download folder containing new HIP files

- year:

  The year in which the Harvest Information Program data were collected

## See also

Other file functions:
[`fileCheck()`](https://usfws.github.io/migbirdHIP/reference/fileCheck.md)

## Author

Abby Walter, <abby_walter@fws.gov>
