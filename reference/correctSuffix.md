# Correct suffix

The internal `correctSuffix` function is used inside of
[`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md) to
change the value(s) in the suffix field to NA if an error is detected.

## Usage

``` r
correctSuffix(proofed_data)
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
[`correctTitle()`](https://usfws.github.io/migbirdHIP/reference/correctTitle.md)

## Author

Abby Walter, <abby_walter@fws.gov>
