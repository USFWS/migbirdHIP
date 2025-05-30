# issueCheck function -----------------------------------------------------

test_that("issueCheck drops past registrations", {

  bad_date <-
    DF_TEST_TINI_CLEANED |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(
      dl_state = "DE",
      issue_date = "01/01/2022")

  suppressMessages(
    invisible(
      capture.output(
        current <- issueCheck(bad_date, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(current) == 0)
})

test_that("issueCheck keeps future registrations", {

  bad_date <-
    DF_TEST_TINI_CLEANED |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(
      dl_state = "DE",
      issue_date = paste0("01/01/", as.numeric(REF_CURRENT_SEASON)+5))

  suppressMessages(
    invisible(
      capture.output(
        current <- issueCheck(bad_date, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(current) == 1)
})

# issueAssign function ----------------------------------------------------


