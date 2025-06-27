# issueCheck function -----------------------------------------------------

test_that("issueCheck drops issue_date of 00/00/0000", {

  bad_date <-
    DF_TEST_TINI_CLEANED |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(
      dl_state = "DE",
      issue_date = "00/00/0000")

  suppressMessages(
    invisible(
      capture.output(
        current <- issueCheck(bad_date, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(current) == 0)
})

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
      issue_date = paste0("01/01/", as.numeric(REF_CURRENT_SEASON) + 5))

  suppressMessages(
    invisible(
      capture.output(
        current <- issueCheck(bad_date, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(current) == 1)
})

# issueAssign function ----------------------------------------------------

test_that("issueAssign evaluates issue_date values correctly", {

  bad_date <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 4) |>
    dplyr::mutate(
      dl_state =
        ifelse(record_key == "record_1", "MS", "CA"),
      issue_date =
        case_when(
          record_key %in% c("record_1", "record_4") ~
            paste0("09/09/", REF_CURRENT_SEASON),
          record_key == "record_2" ~
            "01/01/2022",
          record_key == "record_3" ~
            paste0("01/01/", as.numeric(REF_CURRENT_SEASON) + 5),
          TRUE ~ NA_character_)
    )

  answers <- c("MS", "past", "future", "current")

  suppressMessages(
    invisible(
      capture.output(
        current <- issueAssign(bad_date, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_identical(answers, current$decision)
})
