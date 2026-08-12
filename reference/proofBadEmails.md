# Proof bad emails

The internal `proofBadEmails` function is used inside of
[`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md) to find
poorly formatted or intentionally obfuscative email addresses. Email
addresses that match `REGEX_EMAIL` or are `NA` are considered good
emails.

## Usage

``` r
proofBadEmails(keyed_data)
```

## Arguments

- keyed_data:

  An tibble used internally in
  [`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md)

## See also

Other proofing functions:
[`getBadAddress()`](https://usfws.github.io/migbirdHIP/reference/getBadAddress.md),
[`getBadBirthDate()`](https://usfws.github.io/migbirdHIP/reference/getBadBirthDate.md),
[`getBadCity()`](https://usfws.github.io/migbirdHIP/reference/getBadCity.md),
[`getBadFirstName()`](https://usfws.github.io/migbirdHIP/reference/getBadFirstName.md),
[`getBadHuntMigBirds()`](https://usfws.github.io/migbirdHIP/reference/getBadHuntMigBirds.md),
[`getBadLastName()`](https://usfws.github.io/migbirdHIP/reference/getBadLastName.md),
[`getBadMiddle()`](https://usfws.github.io/migbirdHIP/reference/getBadMiddle.md),
[`getBadRegYear()`](https://usfws.github.io/migbirdHIP/reference/getBadRegYear.md),
[`getBadState()`](https://usfws.github.io/migbirdHIP/reference/getBadState.md),
[`getBadSuffix()`](https://usfws.github.io/migbirdHIP/reference/getBadSuffix.md),
[`getBadTitle()`](https://usfws.github.io/migbirdHIP/reference/getBadTitle.md),
[`getBadZIP()`](https://usfws.github.io/migbirdHIP/reference/getBadZIP.md),
[`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md)

## Author

Abby Walter, <abby_walter@fws.gov>
