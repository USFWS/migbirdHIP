# Find causes of duplication

The internal `duplicateFields` function is used inside of
[`duplicateFinder`](https://usfws.github.io/migbirdHIP/reference/duplicateFinder.md)
to find which fields have different values among a group of duplicate
registrations.

## Usage

``` r
duplicateFields(duplicates, fields)
```

## Arguments

- duplicates:

  The tibble created by
  [`duplicateID`](https://usfws.github.io/migbirdHIP/reference/duplicateID.md)

- fields:

  Name of the columns to compare values for. One or more of the fields
  from the following list may be supplied:

  - title, firstname, middle, lastname, suffix, address, city, state,
    zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag,
    dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes,
    band_tailed_pigeon, brant, seaducks, registration_yr, email

## See also

Other deduplication functions:
[`duplicateAllOnes()`](https://usfws.github.io/migbirdHIP/reference/duplicateAllOnes.md),
[`duplicateAllOnesGroupSize()`](https://usfws.github.io/migbirdHIP/reference/duplicateAllOnesGroupSize.md),
[`duplicateDecide()`](https://usfws.github.io/migbirdHIP/reference/duplicateDecide.md),
[`duplicateFinder()`](https://usfws.github.io/migbirdHIP/reference/duplicateFinder.md),
[`duplicateFix()`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md),
[`duplicateID()`](https://usfws.github.io/migbirdHIP/reference/duplicateID.md),
[`duplicateNewest()`](https://usfws.github.io/migbirdHIP/reference/duplicateNewest.md),
[`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md),
[`duplicateRecordType()`](https://usfws.github.io/migbirdHIP/reference/duplicateRecordType.md),
[`duplicateSample()`](https://usfws.github.io/migbirdHIP/reference/duplicateSample.md)

## Author

Abby Walter, <abby_walter@fws.gov>
