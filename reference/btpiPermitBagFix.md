# Fix band-tailed pigeon permit bag values

The internal `btpiPermitBagFix` function is used inside of
[`clean`](https://usfws.github.io/migbirdHIP/reference/clean.md) to edit
bag values for states that submit permit files separately from HIP. If
records from these states submit a "2" for the band_tailed_pigeon field,
they will be mistakenly identified as permit records. This function
changes band_tailed_pigeon "2" values to "0" so that they are classified
as HIP records until permit files are received later in the hunting
season.

## Usage

``` r
btpiPermitBagFix(raw_data)
```

## Arguments

- raw_data:

  The object created after reading in data with
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

## See also

Other cleaning functions:
[`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md),
[`cranePermitBagFix()`](https://usfws.github.io/migbirdHIP/reference/cranePermitBagFix.md),
[`formatZip()`](https://usfws.github.io/migbirdHIP/reference/formatZip.md),
[`inLinePermitDNHFix()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHFix.md),
[`missingPIIFilter()`](https://usfws.github.io/migbirdHIP/reference/missingPIIFilter.md),
[`moveSuffixes()`](https://usfws.github.io/migbirdHIP/reference/moveSuffixes.md),
[`namesToUppercase()`](https://usfws.github.io/migbirdHIP/reference/namesToUppercase.md),
[`zipCheck()`](https://usfws.github.io/migbirdHIP/reference/zipCheck.md)

## Author

Abby Walter, <abby_walter@fws.gov>
