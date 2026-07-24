test_that("shiftCheck finds line shifts", {

  shifted_data <-
    DF_TEST_TINI_READ |>
    dplyr::slice_head(n = 1) |>
    mutate(
      issue_date =
        paste0(
          stringr::str_sub(birth_date, 10, 10),
          stringr::str_sub(issue_date, 1, 9)),
      birth_date = paste0(" ", stringr::str_sub(birth_date, 1, 9)))

  suppressMessages(
    invisible(
      capture.output(
        shift_checked <- shiftCheck(shifted_data)
      )))

  expect_true(nrow(shift_checked) == 1)
})

# The following tests for shiftCheck() were generated with AI assistance using
# Claude Opus 4.8 in Perplexity on July 22, 2026.

test_that("shiftCheck messages 'No line shifts detected.' on clean input", {
  # Force well-formed birth dates so shiftFinder returns 0 rows.
  clean_input <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 5) |>
    dplyr::mutate(birth_date = "01/01/1990")

  expect_message(shiftCheck(clean_input), "No line shifts detected\\.")
})
