# Plot errors by state

Create a bar plot of errors by state, either by count or proportion.

## Usage

``` r
errorPlotStates(proofed_data, threshold = NA)
```

## Arguments

- proofed_data:

  The object created after error flagging data with
  [`proof`](https://usfws.github.io/migbirdHIP/reference/proof.md) or
  [`correct`](https://usfws.github.io/migbirdHIP/reference/correct.md)

- threshold:

  Optional. A decimal value above which error proportions should be
  plotted.

## See also

Other error-finding functions:
[`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md),
[`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
[`errorTable()`](https://usfws.github.io/migbirdHIP/reference/errorTable.md),
[`errorTableSummary()`](https://usfws.github.io/migbirdHIP/reference/errorTableSummary.md),
[`pullErrors()`](https://usfws.github.io/migbirdHIP/reference/pullErrors.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

Other plotting functions:
[`duplicatePlot()`](https://usfws.github.io/migbirdHIP/reference/duplicatePlot.md),
[`errorLevelErrorsByField()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByField.md),
[`errorLevelErrorsByState()`](https://usfws.github.io/migbirdHIP/reference/errorLevelErrorsByState.md),
[`errorPlotDL()`](https://usfws.github.io/migbirdHIP/reference/errorPlotDL.md),
[`errorPlotFields()`](https://usfws.github.io/migbirdHIP/reference/errorPlotFields.md),
[`issuePlot()`](https://usfws.github.io/migbirdHIP/reference/issuePlot.md),
[`issuePlotDateLabel()`](https://usfws.github.io/migbirdHIP/reference/issuePlotDateLabel.md),
[`redFlags()`](https://usfws.github.io/migbirdHIP/reference/redFlags.md)

## Author

Abby Walter, <abby_walter@fws.gov>
