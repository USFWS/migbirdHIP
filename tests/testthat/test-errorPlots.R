# This test file was generated with AI assistance using Claude Opus 4.8 in
# Perplexity on July 10, 2026.

# errorPlots --------------------------------------------------------------

# Covered exported functions: errorPlotDL, errorPlotFields, errorPlotStates
#
# Fixtures follow existing convention (test-proof.R / test-quality.R): use the
# bundled DF_TEST_TINI_DEDUPED dataset and the real proof() pipeline. A forced
# zip error is injected so every record carries a known, single "zip" error;
# records may or may not contain one or more additional errors.
# DF_TEST_TINI_DEDUPED is example data from "IA" for dl_cycle "0901".

yr <- as.numeric(REF_CURRENT_SEASON)

# Proofed data with injected zip code errors
proofed_err <-
  DF_TEST_TINI_DEDUPED |>
  dplyr::mutate(zip = "000000") |>
  proof(year = yr)

# errorPlotDL -------------------------------------------------------------

test_that("errorPlotDL returns a ggplot for loc = 'all'", {
  p <- errorPlotDL(proofed_err, loc = "all")
  expect_s3_class(p, "ggplot")

  # The data piped into ggplot() is summarized by dl_cycle, so dl_cycle must
  # survive into the plot data.
  expect_true("dl_cycle" %in% names(p$data))
})

test_that("errorPlotDL returns plot for specific state w/o dropping dl_cycle", {
  p <- errorPlotDL(proofed_err, loc = "IA")
  expect_s3_class(p, "ggplot")
  expect_true("dl_cycle" %in% names(p$data))
  expect_true(all(!is.na(p$data$dl_cycle)))
})

test_that("errorPlotDL title reflects the requested state", {
  p <- errorPlotDL(proofed_err, loc = "IA")
  expect_identical(p$labels$title, "Errors per download cycle in IA")
})

test_that("errorPlotDL rejects an invalid loc", {
  expect_error(errorPlotDL(proofed_err, loc = "ZZ"))
  expect_error(errorPlotDL(proofed_err, loc = "notastate"))
})

test_that("errorPlotDL fails on non-proofed data", {
  expect_error(errorPlotDL(DF_TEST_MINI, loc = "all"))
})

# errorPlotFields ---------------------------------------------------------

test_that("errorPlotFields returns a ggplot for loc = 'all'", {
  p <- errorPlotFields(proofed_err, loc = "all", year = yr)
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$title, "Error proportion per field")
})

test_that("errorPlotFields returns a ggplot for a specific state", {
  p <- errorPlotFields(proofed_err, loc = "IA", year = yr)
  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$title, "Error proportion per field in IA")
})

test_that("errorPlotFields plot data carries the flagged field(s)", {
  p <- errorPlotFields(proofed_err, loc = "all", year = yr)
  # Only zip errors were injected, so the summarized field table must contain it.
  expect_true("errors" %in% names(p$data))
  expect_true("zip" %in% p$data$errors)
})

test_that("errorPlotFields rejects an invalid loc", {
  expect_error(errorPlotFields(proofed_err, loc = "ZZ", year = yr))
})

test_that("errorPlotFields fails on a bad year", {
  expect_error(errorPlotFields(proofed_err, loc = "all", year = 1999))
})

test_that("errorPlotFields fails on non-proofed data", {
  expect_error(errorPlotFields(DF_TEST_MINI, loc = "all", year = yr))
})

# errorPlotStates ---------------------------------------------------------

test_that("errorPlotStates returns a ggplot with the default threshold", {
  p <- errorPlotStates(proofed_err)
  expect_s3_class(p, "ggplot")
  expect_true("dl_state" %in% names(p$data))
})

test_that("errorPlotStates returns a ggplot when threshold is met", {
  # Each of 3 records has 1 error -> proportion = 3 / (3 * 14) ~= 0.071.
  p <- errorPlotStates(proofed_err, threshold = 0.05)
  expect_s3_class(p, "ggplot")
})

test_that("errorPlotStates returns a ggplot for threshold = 0", {
  p <- errorPlotStates(proofed_err, threshold = 0)
  expect_s3_class(p, "ggplot")
})

test_that("errorPlotStates messages and returns NULL when threshold too high", {
  # proportion ~0.071 < 0.5, so no state survives the filter.
  expect_message(
    result <- errorPlotStates(proofed_err, threshold = 0.5),
    "Threshold too great"
  )
  expect_null(suppressMessages(errorPlotStates(proofed_err, threshold = 0.5)))
})

test_that("errorPlotStates rejects out-of-range and non-numeric thresholds", {
  expect_error(errorPlotStates(proofed_err, threshold = 2))
  expect_error(errorPlotStates(proofed_err, threshold = -1))
  expect_error(errorPlotStates(proofed_err, threshold = "high"))
})

test_that("errorPlotStates fails on non-proofed data", {
  expect_error(errorPlotStates(DF_TEST_MINI))
})
