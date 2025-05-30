# read_hip function -------------------------------------------------------

test_that("read_hip works for single state", {

  suppressMessages(
    invisible(
      capture.output(
        read_data <-
          read_hip(testthat::test_path("data", "DL0902"), state = "PA")
        )))

  expect_true(nrow(read_data) > 0)
})

test_that("read_hip works for a download", {

  suppressMessages(
    invisible(
      capture.output(
        read_data <- read_hip(testthat::test_path("data", "DL0902"))
        )))

  expect_true(length(unique(read_data$dl_state)) == 2)
})

# listFiles function ------------------------------------------------------

test_that("listFiles works", {

  files_listed <-
    listFiles(
      paste0(testthat::test_path("data", "DL0902"), "/"),
      as.numeric(REF_CURRENT_SEASON))

  expect_true(nrow(files_listed) == 6)
})

# ignorePermits -----------------------------------------------------------

test_that("ignorePermits works", {

  bad_file <- list.files(testthat::test_path("data", "DL0902", "permit"))

  suppressMessages(
    invisible(
      capture.output(
        read_data_files <-
          read_hip(testthat::test_path("data", "DL0902")) |>
          distinct(source_file) |>
          pull()
        )))


  expect_true(!bad_file %in% read_data_files)
})

# ignoreHolds -------------------------------------------------------------

test_that("ignoreHolds works", {

  bad_file <- list.files(testthat::test_path("data", "DL0902", "hold"))

  suppressMessages(
    invisible(
      capture.output(
        read_data_files <-
          read_hip(testthat::test_path("data", "DL0902")) |>
          distinct(source_file) |>
          pull()
      )))

  expect_true(!bad_file %in% read_data_files)
})

# ignoreLifetime ----------------------------------------------------------

test_that("ignoreLifetime works", {

  bad_file <- list.files(testthat::test_path("data", "DL0902", "lifetime"))

  suppressMessages(
    invisible(
      capture.output(
        read_data_files <-
          read_hip(testthat::test_path("data", "DL0902")) |>
          distinct(source_file) |>
          pull()
      )))

  expect_true(!bad_file %in% read_data_files)
})

# idBlankFiles ------------------------------------------------------------

test_that("idBlankFiles works", {

  files_listed <-
    listFiles(
      testthat::test_path("data", "DL0902"),
      as.numeric(REF_CURRENT_SEASON))

  blanks <- idBlankFiles(files_listed) |> filter(check == "blank")

  expect_true(nrow(blanks) == 1)
})

# dropBlankFiles ----------------------------------------------------------

test_that("idBlankFiles works", {

  files_listed <-
    listFiles(
      testthat::test_path("data", "DL0902"),
      as.numeric(REF_CURRENT_SEASON))

  suppressMessages(
    invisible(
      capture.output(
        blanks <-
          dropBlankFiles(idBlankFiles(files_listed)) |>
          filter(check == "blank")
      )))

  expect_true(nrow(blanks) == 0)
})

# checkFileNameDateFormat -------------------------------------------------

test_that("checkFileNameDateFormat works for MMDDYYYY", {

  suppressMessages(
    invisible(
      capture.output(
        badformat <- checkFileNameDateFormat("MD02022024.txt")
      )))

  expect_true(badformat == "error")
})

# checkFileNameStateAbbr --------------------------------------------------

test_that("checkFileNameStateAbbr works", {

  suppressMessages(
    invisible(
      capture.output(
        badformat <- checkFileNameStateAbbr("md20241010.txt")
      )))

  expect_true(badformat == "error")
})
