# Correct data

After flagging errors in the data with
[`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md),
attempt corrections in all fields. Errors that cannot be
programmatically corrected will be reported for manual correction.

## Usage

``` r
correct(proofed_data, year)
```

## Arguments

- proofed_data:

  The object created after error flagging data with
  [`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md)

- year:

  The year in which the Harvest Information Program data were collected

## See also

Other correcting functions:
[`correctEmail()`](https://usfws.github.io/migbirdHIP/reference/correctEmail.md),
[`correctMiddleInitial()`](https://usfws.github.io/migbirdHIP/reference/correctMiddleInitial.md),
[`correctSuffix()`](https://usfws.github.io/migbirdHIP/reference/correctSuffix.md),
[`correctTitle()`](https://usfws.github.io/migbirdHIP/reference/correctTitle.md)

## Author

Abby Walter, <abby_walter@fws.gov>
