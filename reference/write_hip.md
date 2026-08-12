# Modify corrected data table format and write as csv

After correcting errors in the data with
[`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md),
this final step will shape up the dataframe into a format ready for the
database, and write the data to csv.

## Usage

``` r
write_hip(corrected_data, path, type, split = TRUE)
```

## Arguments

- corrected_data:

  The object created after correcting data with
  [`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md)

- path:

  Directory for data to be written

- type:

  The type of HIP file being written out, one of: "HIP", "BT", or "CR"

- split:

  Split the output into one .csv file per .txt file? Default is TRUE.

## See also

Other writing functions:
[`failWidths()`](https://usfws.github.io/migbirdHIP/reference/failWidths.md),
[`writeReport()`](https://usfws.github.io/migbirdHIP/reference/writeReport.md)

## Author

Abby Walter, <abby_walter@fws.gov>
