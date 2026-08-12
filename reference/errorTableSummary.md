# Summary table of errors

Internal function used inside of
[`errorTable`](https://usfws.github.io/migbirdHIP/reference/errorTable.md).

## Usage

``` r
errorTableSummary(proofed_data, initial_tbl, loc, field)
```

## Arguments

- proofed_data:

  The object created after error flagging data with
  [`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md) or
  [`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md)

- initial_tbl:

  [`errorTable`](https://usfws.github.io/migbirdHIP/reference/errorTable.md)
  intermediate object

- loc:

  Which location the error data should be tabulated by. Acceptable
  values include:

  - a two-letter abbreviation for a US state; one of:

    - AL, AK, AZ, AR, CA, CO, CT, DE, FL, GA, ID, IL, IN, IA, KS, KY,
      LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC,
      ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY

  - "all" - all states

  - "none" - table will not include location in its output

- field:

  Field the error data should be tabulated by. Acceptable values
  include:

  - If loc = "none", field must be "all". Otherwise, choose one of:

    - title, firstname, middle, lastname, suffix, address, city, state,
      zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag,
      dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes,
      band_tailed_pigeon, brant, seaducks, registration_yr, email

  - "all" - all fields

  - "none" - table will not include field in its output

## See also

Other error-finding functions:
[`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md),
[`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
[`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
[`errorTable()`](https://usfws.github.io/migbirdHIP/reference/errorTable.md),
[`pullErrors()`](https://usfws.github.io/migbirdHIP/reference/pullErrors.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

## Author

Abby Walter, <abby_walter@fws.gov>
