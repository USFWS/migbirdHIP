# Flag all-one records in a group of duplicates

The internal `duplicateAllOnes` function is used inside of
[`duplicateFix`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md)
to evaluate groups of duplicates for records containing "1" for every
bag value.

## Usage

``` r
duplicateAllOnes(duplicates)
```

## Arguments

- duplicates:

  The tibble created by
  [`duplicateID`](https://usfws.github.io/migbirdHIP/reference/duplicateID.md)

## See also

Other deduplication functions:
[`duplicateAllOnesGroupSize()`](https://usfws.github.io/migbirdHIP/reference/duplicateAllOnesGroupSize.md),
[`duplicateDecide()`](https://usfws.github.io/migbirdHIP/reference/duplicateDecide.md),
[`duplicateFields()`](https://usfws.github.io/migbirdHIP/reference/duplicateFields.md),
[`duplicateFinder()`](https://usfws.github.io/migbirdHIP/reference/duplicateFinder.md),
[`duplicateFix()`](https://usfws.github.io/migbirdHIP/reference/duplicateFix.md),
[`duplicateID()`](https://usfws.github.io/migbirdHIP/reference/duplicateID.md),
[`duplicateNewest()`](https://usfws.github.io/migbirdHIP/reference/duplicateNewest.md),
[`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md),
[`duplicateRecordType()`](https://usfws.github.io/migbirdHIP/reference/duplicateRecordType.md),
[`duplicateSample()`](https://usfws.github.io/migbirdHIP/reference/duplicateSample.md)

## Author

Abby Walter, <abby_walter@fws.gov>
