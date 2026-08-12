# Plot errors across download cycles

Create a plot of errors per download cycle, either by all states in the
data set or a specific state, province, or territory.

## Usage

``` r
errorPlotDL(proofed_data, loc = "all")
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

## See also

Other error-finding functions:
[`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
[`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
[`errorTable()`](https://usfws.github.io/migbirdHIP/reference/errorTable.md),
[`errorTableSummary()`](https://usfws.github.io/migbirdHIP/reference/errorTableSummary.md),
[`pullErrors()`](https://usfws.github.io/migbirdHIP/reference/pullErrors.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

Other plotting functions:
[`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md),
[`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
[`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
[`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md),
[`issuePlotDateLabel()`](https://usfws.github.io/migbirdHIP/reference/issuePlotDateLabel.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

## Author

Abby Walter, <abby_walter@fws.gov>
