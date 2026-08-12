# De-duplicate by randomly sampling intermediate tibbles

The internal `duplicateSample` function is used inside of
[`duplicateFix`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)
to deduplicate intermediate tibbles that have been evaluated using other
criteria already.

## Usage

``` r
duplicateSample(dupes)
```

## Arguments

- dupes:

  Intermediate tibble created in
  [`duplicateFix`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)

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
[`duplicateRecordType()`](https://usfws.github.io/migbirdHIP/reference/duplicateRecordType.md)

## Author

Abby Walter, <abby_walter@fws.gov>
