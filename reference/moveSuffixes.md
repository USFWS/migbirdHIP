# Move suffixes

The internal `moveSuffixes` function moves suffixes from first name or
last name columns into the suffix column and performs other cleaning
steps. This function catches values from 1 to 20 in Roman numerals and
numeric, excluding XVIII since the database limit is 4 characters.

## Usage

``` r
moveSuffixes(raw_data)
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
[`missingPIIFilter()`](https://usfws.github.io/migbirdHIP/reference/missingPIIFilter.md),
[`namesToUppercase()`](https://usfws.github.io/migbirdHIP/reference/namesToUppercase.md),
[`zipCheck()`](https://usfws.github.io/migbirdHIP/reference/zipCheck.md)

## Author

Abby Walter, <abby_walter@fws.gov>
