# Get bad registration year values

The internal `getBadRegYear` function is used inside of
[`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md) and
[`qualityMessages`](https://usfws.github.io/migbirdHIP/reference/qualityMessages.md)
to filter to `registration_yr` values that are not expected. We expect
the registration_yr to equal the current hunting season.

## Usage

``` r
getBadRegYear(data, year)
```

## Arguments

- data:

  Harvest Information Program registration data

- year:

  The year in which the Harvest Information Program data were collected

## See also

Other proofing functions:
[`getBadAddress()`](https://usfws.github.io/migbirdHIP/reference/getBadAddress.md),
[`getBadBirthDate()`](https://usfws.github.io/migbirdHIP/reference/getBadBirthDate.md),
[`getBadCity()`](https://usfws.github.io/migbirdHIP/reference/getBadCity.md),
[`getBadFirstName()`](https://usfws.github.io/migbirdHIP/reference/getBadFirstName.md),
[`getBadHuntMigBirds()`](https://usfws.github.io/migbirdHIP/reference/getBadHuntMigBirds.md),
[`getBadLastName()`](https://usfws.github.io/migbirdHIP/reference/getBadLastName.md),
[`getBadMiddle()`](https://usfws.github.io/migbirdHIP/reference/getBadMiddle.md),
[`getBadState()`](https://usfws.github.io/migbirdHIP/reference/getBadState.md),
[`getBadSuffix()`](https://usfws.github.io/migbirdHIP/reference/getBadSuffix.md),
[`getBadTitle()`](https://usfws.github.io/migbirdHIP/reference/getBadTitle.md),
[`getBadZIP()`](https://usfws.github.io/migbirdHIP/reference/getBadZIP.md),
[`proof()`](https://usfws.github.io/migbirdHIP/reference/proof.md),
[`proofBadEmails()`](https://usfws.github.io/migbirdHIP/reference/proofBadEmails.md)

## Author

Abby Walter, <abby_walter@fws.gov>
