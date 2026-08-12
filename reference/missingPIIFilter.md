# Missing PII filter

The internal `missingPIIFilter` function filters out HIP registrations
that are missing critical pieces of contact information.

## Usage

``` r
missingPIIFilter(raw_data)
```

## Arguments

- raw_data:

  The object created after reading in data with
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

## See also

Other cleaning functions:
[`btpiPermitBagFix()`](https://usfws.github.io/migbirdHIP/reference/btpiPermitBagFix.md),
[`clean()`](https://usfws.github.io/migbirdHIP/reference/clean.md),
[`cranePermitBagFix()`](https://usfws.github.io/migbirdHIP/reference/cranePermitBagFix.md),
[`formatZip()`](https://usfws.github.io/migbirdHIP/reference/formatZip.md),
[`inLinePermitDNHFix()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHFix.md),
[`moveSuffixes()`](https://usfws.github.io/migbirdHIP/reference/moveSuffixes.md),
[`namesToUppercase()`](https://usfws.github.io/migbirdHIP/reference/namesToUppercase.md),
[`zipCheck()`](https://usfws.github.io/migbirdHIP/reference/zipCheck.md)

## Author

Abby Walter, <abby_walter@fws.gov>
