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
