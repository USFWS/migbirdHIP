
<!-- README.md is generated from README.Rmd. Please edit that file -->

# migbirdHarvestData

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Overview

The migbirdHarvestData package provides an easy-to-use set of R
functions for the U.S. Fish and Wildlife Service Migratory Bird Program
to wrangle, tidy, and visualize [Harvest Information
Program](https://www.fws.gov/harvestsurvey) data.

Tasks that can be accomplished with this package include:

  - `read_hip` imports data
  - `tidy` does basic reorganization tasks
  - `strataCheck` identifies any new strata
  - `validate` checks for table-wide repetition
  - `investigate` reports details on repeated values
  - `proof` checks values for errors
  - `correct` systematically fixes erroneous values
  - `manualFix` lets the user fix any remaining unsatisfactory values
  - Error visualization with `errorPlot_dl`, `errorPlot_fields`, and
    `errorPlot_states`
  - Exploration of specific errors with `findDuplicates`,
    `youthHunters`, and `outOfStateHunters`
  - Detailed error reporting with `errorTable`, `pullErrors`, and
    `redFlags`
  - Automated report writing with `writeReport`
  - Random sampling with `hunterSample`

## Installation

``` r
library(devtools)
install_github("USFWS/migbirdHarvestData", build_vignettes = TRUE)
```

#### Vignette

Once the migbirdHarvestData package is installed, you can find a
detailed how-to guide in *The migbirdHarvestData Workflow* vignette by
running: `vignette(topic = "migbirdHarvestData_workflow", package =
"migbirdHarvestData")`

## USFWS Disclaimer

The United States Fish and Wildlife Service (FWS) GitHub project code is
provided on an “as is” basis and the user assumes responsibility for its
use. FWS has relinquished control of the information and no longer has
responsibility to protect the integrity, confidentiality, or
availability of the information. Any reference to specific commercial
products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by FWS. The FWS seal and logo
shall not be used in any manner to imply endorsement of any commercial
product or activity by FWS or the United States Government.

## License

This project is licensed under the terms of the Creative Commons Zero
v1.0 Universal license.
