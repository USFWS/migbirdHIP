# Pull flagged errors

Pull and view errors that have been flagged for a particular field. This
function allows you to easily see what the
[`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md)
function has determined to be unacceptable data.

## Usage

``` r
pullErrors(proofed_data, field, unique = TRUE)
```

## Arguments

- proofed_data:

  The object created after error flagging data with
  [`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md)

- field:

  Field that should be pulled. Acceptable values include:

  - title, firstname, middle, lastname, suffix, address, city, state,
    zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag,
    dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes,
    band_tailed_pigeon, brant, seaducks, registration_yr, email

- unique:

  If FALSE, returns all error values; if TRUE (default), only returns
  unique values.

## See also

Other error-finding functions:
[`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md),
[`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
[`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
[`errorTable()`](https://usfws.github.io/migbirdHIP/reference/errorTable.md),
[`errorTableSummary()`](https://usfws.github.io/migbirdHIP/reference/errorTableSummary.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

## Author

Abby Walter, <abby_walter@fws.gov>
