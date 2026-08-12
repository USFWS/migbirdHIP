# Plot HIP errors by field

Create a bar plot of proportion of error per field. The plot defaults to
all 49 states, but location can be specified.

## Usage

``` r
errorPlotFields(proofed_data, loc = "all", year)
```

## Arguments

- proofed_data:

  The object created after error flagging data with
  [`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md) or
  [`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md)

- loc:

  The location that errors should be plotted for. Acceptable values
  include:

  - "all" - all states

  - a two-letter abbreviation for a US state; one of:

    - AL, AK, AZ, AR, CA, CO, CT, DE, FL, GA, ID, IL, IN, IA, KS, KY,
      LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC,
      ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY

- year:

  The year in which the HIP data were collected.

## See also

Other error-finding functions:
[`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md),
[`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
[`errorTable()`](https://usfws.github.io/migbirdHIP/reference/errorTable.md),
[`errorTableSummary()`](https://usfws.github.io/migbirdHIP/reference/errorTableSummary.md),
[`pullErrors()`](https://usfws.github.io/migbirdHIP/reference/pullErrors.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

Other plotting functions:
[`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md),
[`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md),
[`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
[`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md),
[`issuePlotDateLabel()`](https://usfws.github.io/migbirdHIP/reference/issuePlotDateLabel.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

## Author

Abby Walter, <abby_walter@fws.gov>
