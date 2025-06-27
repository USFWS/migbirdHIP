# Skip these read_hip tests during R CMD check: The "hold" subdirectory causes
# test failure during R CMD check, but testing files under a subdir named "hold"
# is required for the migbirdHIP package to work as intended.

# listFiles function ------------------------------------------------------

test_that("listFiles works", {
  testthat::skip_on_os("windows")

  yr <- as.numeric(REF_CURRENT_SEASON)

  files_listed <-
    listFiles(
      testthat::test_path("data", "DL0902"),
      yr)

  expect_equal(nrow(files_listed), 6)
})

# ignoreHolds -------------------------------------------------------------

test_that("ignoreHolds works", {
  testthat::skip_on_os("windows")

  hold_file <- list.files(testthat::test_path("data", "DL0902", "hold"))

  suppressMessages(
    invisible(
      capture.output(
        read_data_files <-
          read_hip(testthat::test_path("data", "DL0902")) |>
          distinct(source_file) |>
          pull()
      )))

  expect_false(hold_file %in% read_data_files)
})
