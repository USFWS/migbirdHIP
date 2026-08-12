# In-line permit did-not-hunt fix

The internal `inLinePermitDNHFix` function changes any presumed solo
permit from OR or WA indicating "did not hunt" in the hunt_mig_birds
field if one or more of the band_tailed_pigeon, brant, or seaducks
fields indicate hunting.

## Usage

``` r
inLinePermitDNHFix(raw_data)
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
[`missingPIIFilter()`](https://usfws.github.io/migbirdHIP/reference/missingPIIFilter.md),
[`moveSuffixes()`](https://usfws.github.io/migbirdHIP/reference/moveSuffixes.md),
[`namesToUppercase()`](https://usfws.github.io/migbirdHIP/reference/namesToUppercase.md),
[`zipCheck()`](https://usfws.github.io/migbirdHIP/reference/zipCheck.md)

## Author

Abby Walter, <abby_walter@fws.gov>
