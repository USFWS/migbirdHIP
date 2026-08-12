# Issue date range message

The internal `qIssueDateRange` function returns a message for
`issue_date` values in a file that do not span a period of time more
than 1 day.

## Usage

``` r
qIssueDateRange(raw_data)
```

## Arguments

- raw_data:

  The product of
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

## See also

Other quality functions:
[`inLinePermitDNHMessage()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHMessage.md),
[`interStateDuplicatesMessage()`](https://usfws.github.io/migbirdHIP/reference/interStateDuplicatesMessage.md),
[`missingEmailsMessage()`](https://usfws.github.io/migbirdHIP/reference/missingEmailsMessage.md),
[`missingPIIMessage()`](https://usfws.github.io/migbirdHIP/reference/missingPIIMessage.md),
[`naBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/naBagsMessage.md),
[`nonDigitBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/nonDigitBagsMessage.md),
[`nonResidentMessage()`](https://usfws.github.io/migbirdHIP/reference/nonResidentMessage.md),
[`permitFileBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/permitFileBagsMessage.md),
[`qAddress()`](https://usfws.github.io/migbirdHIP/reference/qAddress.md),
[`qBags()`](https://usfws.github.io/migbirdHIP/reference/qBags.md),
[`qBirthDate()`](https://usfws.github.io/migbirdHIP/reference/qBirthDate.md),
[`qBirthDateRange()`](https://usfws.github.io/migbirdHIP/reference/qBirthDateRange.md),
[`qCity()`](https://usfws.github.io/migbirdHIP/reference/qCity.md),
[`qFirstName()`](https://usfws.github.io/migbirdHIP/reference/qFirstName.md),
[`qHuntMigBirds()`](https://usfws.github.io/migbirdHIP/reference/qHuntMigBirds.md),
[`qLastName()`](https://usfws.github.io/migbirdHIP/reference/qLastName.md),
[`qMiddle()`](https://usfws.github.io/migbirdHIP/reference/qMiddle.md),
[`qRegistrationYear()`](https://usfws.github.io/migbirdHIP/reference/qRegistrationYear.md),
[`qState()`](https://usfws.github.io/migbirdHIP/reference/qState.md),
[`qSuffix()`](https://usfws.github.io/migbirdHIP/reference/qSuffix.md),
[`qSummary()`](https://usfws.github.io/migbirdHIP/reference/qSummary.md),
[`qTitle()`](https://usfws.github.io/migbirdHIP/reference/qTitle.md),
[`qZIP()`](https://usfws.github.io/migbirdHIP/reference/qZIP.md),
[`qualityMessages()`](https://usfws.github.io/migbirdHIP/reference/qualityMessages.md),
[`testRecordMessage()`](https://usfws.github.io/migbirdHIP/reference/testRecordMessage.md),
[`zeroBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroBagsMessage.md)

## Author

Abby Walter, <abby_walter@fws.gov>
