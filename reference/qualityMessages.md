# Return messages to console for common or catastrophic data quality issues

The `qualityMessages` function is used to return messages for missing
PII, missing email addresses, all-zero bag records, non-numeric bag
values, bad title assignments, high proportions of non-resident hunters
in a file, inter-state duplicates, low range of issue dates in a file,
and low range of birth dates in a file.

## Usage

``` r
qualityMessages(raw_data, year)
```

## Arguments

- raw_data:

  The product of
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

- year:

  The year in which the Harvest Information Program data were collected

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
[`qIssueDateRange()`](https://usfws.github.io/migbirdHIP/reference/qIssueDateRange.md),
[`qLastName()`](https://usfws.github.io/migbirdHIP/reference/qLastName.md),
[`qMiddle()`](https://usfws.github.io/migbirdHIP/reference/qMiddle.md),
[`qRegistrationYear()`](https://usfws.github.io/migbirdHIP/reference/qRegistrationYear.md),
[`qState()`](https://usfws.github.io/migbirdHIP/reference/qState.md),
[`qSuffix()`](https://usfws.github.io/migbirdHIP/reference/qSuffix.md),
[`qSummary()`](https://usfws.github.io/migbirdHIP/reference/qSummary.md),
[`qTitle()`](https://usfws.github.io/migbirdHIP/reference/qTitle.md),
[`qZIP()`](https://usfws.github.io/migbirdHIP/reference/qZIP.md),
[`testRecordMessage()`](https://usfws.github.io/migbirdHIP/reference/testRecordMessage.md),
[`zeroBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroBagsMessage.md)

## Author

Abby Walter, <abby_walter@fws.gov>
