# Correct middle initials

The internal `correctMiddleInitial` function changes non-alphabetic
characters in the middle initial column to NA.

## Usage

``` r
correctMiddleInitial(proofed_data)
```

## Arguments

- proofed_data:

  The object created after error flagging data with
  [`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md)

## See also

Other correcting functions:
[`correct()`](https://usfws.github.io/migbirdHIP/reference/correct.md),
[`correctEmail()`](https://usfws.github.io/migbirdHIP/reference/correctEmail.md),
[`correctSuffix()`](https://usfws.github.io/migbirdHIP/reference/correctSuffix.md),
[`correctTitle()`](https://usfws.github.io/migbirdHIP/reference/correctTitle.md)

## Author

Abby Walter, <abby_walter@fws.gov>
