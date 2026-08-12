# Get bad state values

The internal `getBadState` function is used inside of
[`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md) and
[`qualityMessages`](https://usfws.github.io/migbirdHIP/reference/qualityMessages.md)
to filter to `state` values that are not expected. Used by
[`qState`](https://usfws.github.io/migbirdHIP/reference/qState.md).

## Usage

``` r
getBadState(data)
```

## Arguments

- data:

  Harvest Information Program registration data

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
[`getBadSuffix()`](https://usfws.github.io/migbirdHIP/reference/getBadSuffix.md),
[`getBadTitle()`](https://usfws.github.io/migbirdHIP/reference/getBadTitle.md),
[`getBadZIP()`](https://usfws.github.io/migbirdHIP/reference/getBadZIP.md),
[`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md),
[`proofBadEmails()`](https://usfws.github.io/migbirdHIP/reference/proofBadEmails.md)

## Author

Abby Walter, <abby_walter@fws.gov>
