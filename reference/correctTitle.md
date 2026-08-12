# Correct title

The internal `correctTitle` function is used inside of
[`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md) to
change the value(s) in the title field to NA if an error is detected.

## Usage

``` r
correctTitle(proofed_data)
```

## Arguments

- proofed_data:

  The object created after error flagging data with
  [`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md)

## See also

Other correcting functions:
[`correct()`](https://usfws.github.io/migbirdHIP/reference/correct.md),
[`correctEmail()`](https://usfws.github.io/migbirdHIP/reference/correctEmail.md),
[`correctMiddleInitial()`](https://usfws.github.io/migbirdHIP/reference/correctMiddleInitial.md),
[`correctSuffix()`](https://usfws.github.io/migbirdHIP/reference/correctSuffix.md)

## Author

Abby Walter, <abby_walter@fws.gov>
