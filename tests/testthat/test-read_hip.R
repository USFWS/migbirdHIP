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

# ignorePermits -----------------------------------------------------------

test_that("ignorePermits works", {

  pmt_file <- list.files(testthat::test_path("data", "DL0902", "permit"))

  suppressMessages(
    invisible(
      capture.output(
        read_data_files <-
          read_hip(testthat::test_path("data", "DL0902")) |>
          distinct(source_file) |>
          pull()
        )))

  expect_false(pmt_file %in% read_data_files)
})

# ignoreLifetime ----------------------------------------------------------

test_that("ignoreLifetime works", {

  lifetime_file <- list.files(testthat::test_path("data", "DL0902", "lifetime"))

  suppressMessages(
    invisible(
      capture.output(
        read_data_files <-
          read_hip(testthat::test_path("data", "DL0902")) |>
          distinct(source_file) |>
          pull()
      )))

  expect_false(lifetime_file %in% read_data_files)
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

test_that("dropBlankFiles works", {

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

# dropBlankLines ----------------------------------------------------------

test_that("dropBlankLines works", {

  test_raw_data <-
    bind_rows(
      tibble(title = NA, firstname = NA),
      tibble(title = "R", firstname = "esult"),
      DF_TEST_MINI |> slice_sample(n = 10)
    )

  expect_true(nrow(dropBlankLines(test_raw_data)) == (nrow(test_raw_data) - 2))
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

# extended argument testing -----------------------------------------------

# The following tests for read_hip() were generated with AI assistance using
# Claude Opus 4.8 in Perplexity on July 22, 2026.

# unique = FALSE suppresses distinct() AND the record_key column; default keeps
# them
test_that("read_hip unique = FALSE retains rows and drops record_key", {
  suppressMessages(invisible(capture.output(
    def <- read_hip(testthat::test_path("data", "DL0902")))))
  suppressMessages(invisible(capture.output(
    nou <- read_hip(testthat::test_path("data", "DL0902"), unique = FALSE))))

  expect_true("record_key" %in% names(def))
  expect_false("record_key" %in% names(nou))
  # Duplicate suppression can only add rows, never remove them.
  expect_gte(nrow(nou), nrow(def))
})

# season = TRUE lists folders recursively. The bundled files live in
# data/DL0902/, so a non-recursive read of the parent finds nothing, whereas
# season = TRUE finds the two states.
test_that("read_hip season = TRUE recurses where the default cannot", {
  # Default (season = FALSE): no .txt directly under data/ -> hard error.
  expect_error(
    suppressMessages(read_hip(testthat::test_path("data"))),
    "No file")

  suppressMessages(invisible(capture.output(
    seasoned <- read_hip(testthat::test_path("data"), season = TRUE))))
  expect_equal(length(unique(seasoned$dl_state)), 2)
})
