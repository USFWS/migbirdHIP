# Inter-state duplicates message

The internal `interStateDuplicatesMessage` function returns a message
for duplicates found among files submitted from different states.

## Usage

``` r
interStateDuplicatesMessage(raw_data, n_threshold = 100, p_threshold = 0.05)
```

## Arguments

- raw_data:

  The product of
  [`read_hip`](https://usfws.github.io/migbirdHIP/reference/read_hip.md)

- n_threshold:

  A whole number; the threshold below which inter-state duplicates will
  not be reported, unless `p_threshold` is exceeded. Defaults to n = 100
  inter-state duplicates.

- p_threshold:

  A number ranging from 0 to 1; the threshold below which inter-state
  duplicates will not be reported, unless `n_threshold` is exceeded.
  Defaults to 0.05, or inter-state duplicates exceeding more than 5% of
  the number of submitted registrations by the first download state OR
  the second download state.

## See also

Other quality functions:
[`inLinePermitDNHMessage()`](https://usfws.github.io/migbirdHIP/reference/inLinePermitDNHMessage.md),
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
[`qualityMessages()`](https://usfws.github.io/migbirdHIP/reference/qualityMessages.md),
[`testRecordMessage()`](https://usfws.github.io/migbirdHIP/reference/testRecordMessage.md),
[`zeroBagsMessage()`](https://usfws.github.io/migbirdHIP/reference/zeroBagsMessage.md)

## Author

Abby Walter, <abby_walter@fws.gov>
