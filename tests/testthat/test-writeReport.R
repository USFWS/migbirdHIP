# This test file was generated with AI assistance using Claude Opus 4.8 in
# Perplexity on July 10, 2026.

# writeReport -------------------------------------------------------------

# Covered exported function: writeReport
#
# writeReport() is almost entirely an I/O + quarto_render() wrapper. The render
# step copies a .qmd template and calls quarto::quarto_render, which needs a
# real quarto install plus the Suggested packages (kableExtra, DT, sf, stringi)
# and would produce an HTML file on disk. We therefore do NOT exercise the
# render path here; instead we test the input-validation logic, all of which
# runs (and stops) BEFORE the template copy / render. See the skipped test at
# the bottom documenting the untested render path.

yr <- as.numeric(REF_CURRENT_SEASON)

make_tmpdir <- function() {
  d <- tempfile("wrpt_")
  dir.create(d)
  d
}

test_that("writeReport fails on a bad year", {
  d <- make_tmpdir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(
    writeReport(raw_path = d, temp_path = d, year = 1999, dl = "0901",
                dir = d, file = "report")
  )
})

test_that("writeReport requires dl to be a character string", {
  d <- make_tmpdir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(
    writeReport(raw_path = d, temp_path = d, year = yr, dl = 901,
                dir = d, file = "report"),
    "must be string"
  )
})

test_that("writeReport requires dl to be a 4-digit cycle", {
  d <- make_tmpdir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(
    writeReport(raw_path = d, temp_path = d, year = yr, dl = "9",
                dir = d, file = "report"),
    "4-char dl cycle"
  )
})

test_that("writeReport rejects a file name containing a period/suffix", {
  d <- make_tmpdir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(
    writeReport(raw_path = d, temp_path = d, year = yr, dl = "0901",
                dir = d, file = "report.html"),
    "must not contain period"
  )
})

test_that("writeReport render path is not exercised (documented limitation)", {
  # The successful render path depends on a working quarto CLI and several
  # Suggested packages, and writes an HTML file. It is intentionally not tested
  # here to keep the suite deterministic and free of heavy external
  # dependencies.
  skip("writeReport render path requires quarto + Suggested deps; not tested")
})
