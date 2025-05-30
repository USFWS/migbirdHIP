# read_hip function -------------------------------------------------------

test_that("read_hip works for single state", {

  suppressMessages(
    invisible(
      capture.output(
        read_data <-
          read_hip(
            paste0(here::here(), "/inst/extdata/DL0901/"),
            state = "RI")
        )))

  expect_true(nrow(read_data) > 0)
})

test_that("read_hip works for a download", {

  suppressMessages(
    invisible(
      capture.output(
        read_data <-
          read_hip(
            paste0(here::here(), "/inst/extdata/DL0901/"))
        )))

  expect_true(length(unique(read_data$dl_state)) == 49)
})

# listFiles function ------------------------------------------------------

test_that("listFiles works", {

  files_listed <-
    listFiles(
      paste0(here::here(), "/inst/extdata/DL0901/"),
      as.numeric(REF_CURRENT_SEASON))

  expect_true(nrow(files_listed) == 49)
})

# ignorePermits -----------------------------------------------------------

test_that("ignorePermits works", {

  bad_file <- list.files(paste0(here::here(), "/inst/extdata/DL0902/permit/"))

  suppressMessages(
    invisible(
      capture.output(
        read_data_files <-
          read_hip(paste0(here::here(), "/inst/extdata/DL0902/")) |>
          distinct(source_file) |>
          pull()
        )))


  expect_true(!bad_file %in% read_data_files)
})

# ignoreHolds -------------------------------------------------------------

test_that("ignoreHolds works", {

  bad_file <- list.files(paste0(here::here(), "/inst/extdata/DL0902/hold/"))

  suppressMessages(
    invisible(
      capture.output(
        read_data_files <-
          read_hip(paste0(here::here(), "/inst/extdata/DL0902/")) |>
          distinct(source_file) |>
          pull()
      )))

  expect_true(!bad_file %in% read_data_files)
})

# ignoreLifetime ----------------------------------------------------------

test_that("ignoreLifetime works", {

  bad_file <- list.files(paste0(here::here(), "/inst/extdata/DL0902/lifetime/"))

  suppressMessages(
    invisible(
      capture.output(
        read_data_files <-
          read_hip(paste0(here::here(), "/inst/extdata/DL0902/")) |>
          distinct(source_file) |>
          pull()
      )))

  expect_true(!bad_file %in% read_data_files)
})

# idBlankFiles ------------------------------------------------------------

test_that("idBlankFiles works", {

  files_listed <-
    listFiles(
      paste0(here::here(), "/inst/extdata/DL0902/"),
      as.numeric(REF_CURRENT_SEASON))

  blanks <- idBlankFiles(files_listed) |> filter(check == "blank")

  expect_true(nrow(blanks) == 1)
})

# dropBlankFiles ----------------------------------------------------------

test_that("idBlankFiles works", {

  files_listed <-
    listFiles(
      paste0(here::here(), "/inst/extdata/DL0902/"),
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


# checkFileNameStateAbbr --------------------------------------------------


