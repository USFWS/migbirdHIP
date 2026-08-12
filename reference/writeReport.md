# Write an R markdown report from template

Create documentation with figures and tables that summarizes HIP data at
a download cycle scale.

## Usage

``` r
writeReport(raw_path, temp_path, year, dl, dir, file)
```

## Arguments

- raw_path:

  File path to the folder containing HIP .txt files

- temp_path:

  Path to directory that contains output .csv files

- year:

  The year in which the Harvest Information Program data were collected

- dl:

  Download cycle

- dir:

  Folder in which to save the rendered report

- file:

  What the report file should be named (do not include an extension or
  suffix indicating file type, this is done automatically)

## See also

Other writing functions:
[`failWidths()`](https://usfws.github.io/migbirdHIP/reference/failWidths.md),
[`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md)

## Author

Abby Walter, <abby_walter@fws.gov>
