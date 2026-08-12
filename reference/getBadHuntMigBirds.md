# Get bad hunt mig birds values

The internal `getBadHuntMigBirds` function is used inside of
[`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md) and
[`qualityMessages`](https://usfws.github.io/migbirdHIP/reference/qualityMessages.md)
to filter to `hunt_mig_birds` values that are not expected.

## Usage

``` r
getBadHuntMigBirds(data)
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
[`getBadLastName()`](https://usfws.github.io/migbirdHIP/reference/getBadLastName.md),
[`getBadMiddle()`](https://usfws.github.io/migbirdHIP/reference/getBadMiddle.md),
[`getBadRegYear()`](https://usfws.github.io/migbirdHIP/reference/getBadRegYear.md),
[`getBadState()`](https://usfws.github.io/migbirdHIP/reference/getBadState.md),
[`getBadSuffix()`](https://usfws.github.io/migbirdHIP/reference/getBadSuffix.md),
[`getBadTitle()`](https://usfws.github.io/migbirdHIP/reference/getBadTitle.md),
[`getBadZIP()`](https://usfws.github.io/migbirdHIP/reference/getBadZIP.md),
[`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md),
[`proofBadEmails()`](https://usfws.github.io/migbirdHIP/reference/proofBadEmails.md)

## Author

Abby Walter, <abby_walter@fws.gov>
