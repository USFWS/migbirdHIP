## ----install, eval = FALSE----------------------------------------------------
# devtools::install_github(
#   "USFWS/migbirdHIP",
#   quiet = T,
#   upgrade = F,
#   build_vignettes = T
# )

## ----releases, eval = FALSE---------------------------------------------------
# devtools::install_github("USFWS/migbirdHIP@v1.2.8", build_vignettes = T)

## ----setup--------------------------------------------------------------------
library(migbirdHIP)

## ----flowchart_svg, echo = FALSE, fig.cap = "Overview of migbirdHIP functions in a flowchart format.", out.width = "100%", dev = "svg", fig.align = "center"----
knitr::include_graphics(
  paste0(here::here(), "/man/figures/migbirdHIP_flowchart.svg"))

## ----rename, eval = FALSE-----------------------------------------------------
# fileRename(path = "C:/HIP/raw_data/DL0901", year = 2025)

## ----filecheck, eval = FALSE--------------------------------------------------
# fileCheck(
#   raw_path = "C:/HIP/raw_data/DL0901/",
#   processed_path = "C:/HIP/corrected_data/"
# )

## ----readhip------------------------------------------------------------------
raw_data <- read_hip(paste0(here::here(), "/inst/extdata/DL0901/"))

## ----glyphCheck---------------------------------------------------------------
glyphCheck(raw_data)

## ----shiftCheck---------------------------------------------------------------
shiftCheck(raw_data)

## ----clean--------------------------------------------------------------------
clean_data <- clean(raw_data)

## ----issuecheck, eval = F-----------------------------------------------------
# current_data <- issueCheck(clean_data, year = 2025, plot = F)

## ----createcurrent, include = F-----------------------------------------------
current_data <- clean_data

## ----findDuplicates_tbl-------------------------------------------------------
duplicateFinder(current_data)

## ----duplicateFix-------------------------------------------------------------
deduplicated_data <- duplicateFix(current_data)

## ----bagCheck-----------------------------------------------------------------
bagCheck(deduplicated_data)

## ----proof--------------------------------------------------------------------
proofed_data <- proof(deduplicated_data, year = 2025)

## ----correct------------------------------------------------------------------
corrected_data <- correct(proofed_data, year = 2025)

## ----duplicatePlot, fig.cap = "Figure 1. Plot of types of duplicates.", fig.width = 6, fig.height = 3.5, dpi = 300, out.width = 500, fig.align = "center"----
duplicatePlot(current_data)

## ----errorfieldsplotall, fig.cap = "Figure 2. Plot of all location's errors by field name.", fig.width = 6, fig.height = 3.6, dpi = 300, out.width = 500, fig.align = "center"----
errorPlotFields(proofed_data, loc = "all", year = 2025)

## ----errorfieldsplotsc, fig.cap = "Figure 3. Plot of Louisiana's errors by field name.", fig.width = 6, fig.height = 3.6, dpi = 300, out.width = 500, fig.align = "center"----
errorPlotFields(proofed_data, loc = "LA", year = 2025)

## ----errorfieldsfacet, eval = FALSE-------------------------------------------
# errorPlotFields(
#   hipdata2025 |>
#     filter(str_detect(dl_cycle, "0800|0901|0902|1001")),
#     year = 2025) +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1),
#     legend.position = "bottom") +
#   facet_wrap(~dl_cycle, ncol = 2)

## ----errorplotstates, fig.cap = "Figure 5. Plot of proportion of error by state.", fig.width = 6, fig.height = 4, dpi = 300, out.width = 500, fig.align = "center"----
errorPlotStates(proofed_data, threshold = 0.05)

## ----errorPlotDL, eval = FALSE------------------------------------------------
# errorPlotDL(hipdata2025, loc = "MI")

## ----errorplotdl_example, echo = FALSE, fig.cap = "errorPlot_dl example output", out.width = "70%", fig.align = "center"----
knitr::include_graphics(paste0(here::here(), "/man/figures/errorPlot_dl.png"))

## ----pullerrors---------------------------------------------------------------
pullErrors(proofed_data, field = "suffix")

## ----pullerrors2--------------------------------------------------------------
pullErrors(proofed_data, field = "dove_bag")

## ----errortable1--------------------------------------------------------------
errorTable(proofed_data)

## ----errortable2--------------------------------------------------------------
errorTable(proofed_data, field = "none")

## ----errortable3--------------------------------------------------------------
errorTable(proofed_data, loc = "none")

## ----errortable4--------------------------------------------------------------
errorTable(proofed_data, loc = "CA")

## ----errortable5--------------------------------------------------------------
errorTable(proofed_data, field = "suffix")

## ----errortable6--------------------------------------------------------------
errorTable(proofed_data, loc = "CA", field = "none")

## ----errortable7--------------------------------------------------------------
errorTable(proofed_data, loc = "CA", field = "dove_bag")

## ----writecsv, eval = FALSE---------------------------------------------------
# 
# write_hip(corrected_data, path = "C:/HIP/processed_data/")
# 

## ----writerpt, eval = FALSE---------------------------------------------------
# 
# writeReport(
#   raw_path = "C:/HIP/DL0901/",
#   temp_path = "C:/HIP/corrected_data",
#   year = 2025,
#   dl = "0901",
#   dir = "C:/HIP/dl_reports",
#   file = "DL0901_report")

