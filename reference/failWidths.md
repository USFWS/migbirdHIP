# Fail if field widths are exceeded by any value

Internal function that fails inside of
[`write_hip`](https://usfws.github.io/migbirdHIP/reference/write_hip.md)
if any value exceeds designated field widths.

## Usage

``` r
failWidths(corrected_data)
```

## Arguments

- corrected_data:

  The object created after correcting data with
  [`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md)

## See also

Other failure functions:
[`failBTPI()`](https://usfws.github.io/migbirdHIP/reference/failBTPI.md),
[`failCR()`](https://usfws.github.io/migbirdHIP/reference/failCR.md),
[`failDLdate()`](https://usfws.github.io/migbirdHIP/reference/failDLdate.md),
[`failDLstate()`](https://usfws.github.io/migbirdHIP/reference/failDLstate.md),
[`failProofed()`](https://usfws.github.io/migbirdHIP/reference/failproofed.md),
[`failState()`](https://usfws.github.io/migbirdHIP/reference/failState.md),
[`failTF()`](https://usfws.github.io/migbirdHIP/reference/failTF.md),
[`failYear()`](https://usfws.github.io/migbirdHIP/reference/failyear.md)

Other writing functions:
[`writeReport()`](https://usfws.github.io/migbirdHIP/reference/writeReport.md),
[`write_hip()`](https://usfws.github.io/migbirdHIP/reference/write_hip.md)

## Author

Abby Walter, <abby_walter@fws.gov>
