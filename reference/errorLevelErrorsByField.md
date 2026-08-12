# Calculate error-level errors by field

The internal `errorLevelErrorsByField` function calculates a summary
table of the count of errors and proportion of erroneous values by
field.

## Usage

``` r
errorLevelErrorsByField(proofed_data)
```

## Arguments

- proofed_data:

  The object created after error flagging data with
  [`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md) or
  [`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md)

## See also

Other error-finding functions:
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md),
[`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
[`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
[`errorTable()`](https://usfws.github.io/migbirdHIP/reference/errorTable.md),
[`errorTableSummary()`](https://usfws.github.io/migbirdHIP/reference/errorTableSummary.md),
[`pullErrors()`](https://usfws.github.io/migbirdHIP/reference/pullErrors.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

Other plotting functions:
[`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md),
[`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
[`errorPlotStates()`](https://usfws.github.io/migbirdHIP/reference/errorPlotStates.md),
[`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md),
[`issuePlotDateLabel()`](https://usfws.github.io/migbirdHIP/reference/issuePlotDateLabel.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

## Author

Abby Walter, <abby_walter@fws.gov>
