# Fix crane permit bag values

The internal `cranePermitBagFix` function is used inside of
[`clean`](https://usfws.github.io/migbirdHIP/reference/clean.md) to edit
bag values for states that submit permit files separately from HIP. If
records from these states submit a "2" for the crane field, they will be
mistakenly identified as permit records. This function changes crane "2"
values to "0" so that they are classified as HIP records until permit
files are received later in the hunting season.

## Usage

``` r
cranePermitBagFix(raw_data)
```

## Arguments

- raw_data:

  The object created after reading in data with
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

## See also

Other cleaning functions:
[`btpiPermitBagFix()`](https://usfws.github.io/migbirdHIP/reference/btpiPermitBagFix.md),
[`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md),
[`formatZip()`](https://usfws.github.io/migbirdHIP/reference/formatZip.md),
[`inLinePermitDNHFix()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHFix.md),
[`missingPIIFilter()`](https://usfws.github.io/migbirdHIP/reference/missingPIIFilter.md),
[`moveSuffixes()`](https://usfws.github.io/migbirdHIP/reference/moveSuffixes.md),
[`namesToUppercase()`](https://usfws.github.io/migbirdHIP/reference/namesToUppercase.md),
[`zipCheck()`](https://usfws.github.io/migbirdHIP/reference/zipCheck.md)

## Author

Abby Walter, <abby_walter@fws.gov>
