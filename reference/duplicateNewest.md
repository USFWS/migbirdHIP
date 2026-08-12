# Find the most recent records out of a group of duplicates

The internal `duplicateNewest` function is used inside of
[`duplicateFix`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)
to filter groups of duplicates to the most recent records out of each
group.

## Usage

``` r
duplicateNewest(duplicates)
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
[`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md),
[`duplicateRecordType()`](https://usfws.github.io/migbirdHIP/reference/duplicateRecordType.md),
[`duplicateSample()`](https://usfws.github.io/migbirdHIP/reference/duplicateSample.md)

## Author

Abby Walter, <abby_walter@fws.gov>
