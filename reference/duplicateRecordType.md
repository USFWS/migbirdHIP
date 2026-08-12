# Set record type

The internal `duplicateRecordType` function is used inside of
[`duplicateFix`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)
to set record type of registrations based on each record's bag values.

## Usage

``` r
duplicateRecordType(duplicates)
```

## Arguments

- duplicates:

  The tibble created by
  [`duplicateID`](https://usfws.github.io/migbirdHIP/reference/duplicateID.md)

## See also

Other deduplication functions:
[`duplicateAllOnes()`](https://usfws.github.io/migbirdHIP/reference/duplicateAllOnes.md),
[`duplicateAllOnesGroupSize()`](https://usfws.github.io/migbirdHIP/reference/duplicateAllOnesGroupSize.md),
[`duplicateDecide()`](https://usfws.github.io/migbirdHIP/reference/duplicateDecide.md),
[`duplicateFields()`](https://usfws.github.io/migbirdHIP/reference/duplicateFields.md),
[`duplicateFinder()`](https://usfws.github.io/migbirdHIP/reference/duplicateFinder.md),
[`duplicateFix()`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md),
[`duplicateID()`](https://usfws.github.io/migbirdHIP/reference/duplicateID.md),
[`duplicateNewest()`](https://usfws.github.io/migbirdHIP/reference/duplicateNewest.md),
[`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md),
[`duplicateSample()`](https://usfws.github.io/migbirdHIP/reference/duplicateSample.md)

## Author

Abby Walter, <abby_walter@fws.gov>
