# Fail if there is an NA in dl_date

The internal `failDLdate` function is used inside of
[`write_hip`](https://usfws.github.io/migbirdHIP/reference/write_hip.md).

## Usage

``` r
failDLdate(corrected_data)
```

## Arguments

- corrected_data:

  The object created after correcting data with
  [`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md)

## See also

Other failure functions:
[`failBTPI()`](https://usfws.github.io/migbirdHIP/reference/failBTPI.md),
[`failCR()`](https://usfws.github.io/migbirdHIP/reference/failCR.md),
[`failDLstate()`](https://usfws.github.io/migbirdHIP/reference/failDLstate.md),
[`failProofed()`](https://usfws.github.io/migbirdHIP/reference/failproofed.md),
[`failState()`](https://usfws.github.io/migbirdHIP/reference/failState.md),
[`failTF()`](https://usfws.github.io/migbirdHIP/reference/failTF.md),
[`failWidths()`](https://usfws.github.io/migbirdHIP/reference/failWidths.md),
[`failYear()`](https://usfws.github.io/migbirdHIP/reference/failyear.md)

## Author

Abby Walter, <abby_walter@fws.gov>
