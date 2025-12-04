test_that("issueCheck keeps MS registrations", {

  MS_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "MS",
      issue_date = paste0("10/01/", REF_CURRENT_SEASON))

  suppressMessages(
    invisible(
      capture.output(
        current <- issueCheck(MS_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(MS_df) == nrow(current))

})

test_that("issueAssign sets decision as current for current 1-season regs", {

  current_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "CA",
      issue_date = paste0("10/01/", REF_CURRENT_SEASON))

  suppressMessages(
    invisible(
      capture.output(
        current <- issueAssign(current_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_equal(current$decision, rep("current", 3))

})

test_that("issueCheck keeps current registrations, 1-season", {

  current_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "CA",
      issue_date = paste0("10/01/", REF_CURRENT_SEASON))

  suppressMessages(
    invisible(
      capture.output(
        current <- issueCheck(current_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(current_df) == nrow(current))
})

test_that("issueAssign sets decision as future for future 1-season regs", {

  future_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "CA",
      issue_date = paste0("04/30/", as.numeric(REF_CURRENT_SEASON) + 1))

  suppressMessages(
    invisible(
      capture.output(
        future <- issueAssign(future_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_equal(future$decision, rep("future", 3))
})

test_that("issueCheck keeps future registrations, 1-season", {

  future_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "CA",
      issue_date = paste0("04/30/", as.numeric(REF_CURRENT_SEASON) + 1))

  suppressMessages(
    invisible(
      capture.output(
        future <- issueCheck(future_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(future) == 3)
})

test_that("issueAssign sets decision as invalid for invalid 1-season regs", {

  invalid_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "CA",
      issue_date = paste0("03/30/", as.numeric(REF_CURRENT_SEASON) + 1))

  suppressMessages(
    invisible(
      capture.output(
        invalid <- issueAssign(invalid_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_equal(invalid$decision, rep("invalid", 3))
})

test_that("issueCheck drops invalid registrations, 1-season", {

  invalid_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "CA",
      issue_date = paste0("03/30/", as.numeric(REF_CURRENT_SEASON) + 1))

  suppressMessages(
    invisible(
      capture.output(
        invalid <- issueCheck(invalid_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(invalid) == 0)
})

test_that("issueAssign sets decision as past for past 1-season regs", {

  past_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "DE",
      issue_date = "01/01/2022")

  suppressMessages(
    invisible(
      capture.output(
        past <- issueAssign(past_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_equal(past$decision, rep("past", 3))
})

test_that("issueCheck drops past registrations, 1-season", {

  past_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "DE",
      issue_date = "01/01/2022")

  suppressMessages(
    invisible(
      capture.output(
        past <- issueCheck(past_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(past) == 0)
})

test_that("issueAssign sets decision as bad, for too far in future, 1-season", {

  # Too far in future
  bid_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "DE",
      issue_date = paste0("12/01/", as.numeric(REF_CURRENT_SEASON) + 2))

  suppressMessages(
    invisible(
      capture.output(
        bid <- issueAssign(bid_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_equal(bid$decision, rep("bad issue dates", 3))
})

test_that("issueCheck drops bad issue date registrations, 1-season", {

  # Too far in future
  bid_df <-
    DF_TEST_TINI_CLEANED |>
    mutate(
      dl_state = "DE",
      issue_date = paste0("12/01/", as.numeric(REF_CURRENT_SEASON) + 2))

  suppressMessages(
    invisible(
      capture.output(
        bid <- issueCheck(bid_df, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(bid) == 0)
})

test_that("issueAssign drops issue_date of 00/00/0000", {

  bad_date <-
    DF_TEST_TINI_CLEANED |>
    dplyr::slice_head(n = 1) |>
    mutate(
      dl_state = "DE",
      issue_date = "00/00/0000")

  suppressMessages(
    invisible(
      capture.output(
        current <- issueAssign(bad_date, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_true(nrow(current) == 0)
})

test_that("issueAssign evaluates correctly when registration_yr is current", {

  bad_date <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 5) |>
    select(
      c("record_key", "issue_date", "registration_yr", "dl_state")) |>
    mutate(
      dl_state =
        ifelse(record_key == "record_1", "MA", "CA"),
      issue_date =
        case_when(
          record_key %in% c("record_1", "record_4") ~
            paste0("09/09/", REF_CURRENT_SEASON),
          record_key == "record_2" ~
            "01/01/2022",
          record_key == "record_3" ~
            paste0("01/01/", as.numeric(REF_CURRENT_SEASON) + 5),
          record_key == "record_5" ~
            paste0("05/01/", as.numeric(REF_CURRENT_SEASON) + 1),
          TRUE ~ NA_character_)
    )

  answers <- c("current", "past", "bad issue dates", "current", "future")

  suppressMessages(
    invisible(
      capture.output(
        current <- issueAssign(bad_date, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_identical(answers, current$decision)
})

test_that("issueAssign changes future registration_yr correctly", {

  bad_date <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 5) |>
    select(
      c("record_key", "issue_date", "registration_yr", "dl_state")) |>
    mutate(
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
          record_key == "record_5" ~
            paste0("05/01/", as.numeric(REF_CURRENT_SEASON) + 1),
          TRUE ~ NA_character_)
    )

  answers <- c(rep("2025", 4), "2026")

  suppressMessages(
    invisible(
      capture.output(
        current <- issueAssign(bad_date, as.numeric(REF_CURRENT_SEASON))
      )))

  expect_identical(answers, current$registration_yr)
})

test_that("issueAssign evaluates 2-season states correctly", {

  test_start <- paste0("01/01/", as.numeric(REF_CURRENT_SEASON) - 1)
  test_end <- paste0("12/31/", as.numeric(REF_CURRENT_SEASON) + 2)
  st <- "CT"

  issue_window_begin <- REF_DATES$issue_start[REF_DATES$state == st]
  issue_window_end <- REF_DATES$issue_end[REF_DATES$state == st]

  days_btwn_starts <- issue_window_begin - mdy(test_start)
  days_overlap <- issue_window_end - (issue_window_begin + 365) + 1
  days_in_window <- (issue_window_begin + 365) - issue_window_begin
  days_bad <- mdy(test_end) - (issue_window_end + 365)
  days_btwn_ends <-
    (issue_window_end + 365) - (issue_window_begin + 365) - days_overlap + 1

  twoseason_data <-
    tibble(
      yearmonthday =
        as.character(
          as.Date(
            mdy(test_start):mdy(test_end))),
      issue_date =
        paste(
          stringr::str_sub(yearmonthday, 6, 7),
          stringr::str_sub(yearmonthday, 9, 10),
          stringr::str_sub(yearmonthday, 1, 4),
          sep = "/"),
      registration_yr = REF_CURRENT_SEASON,
      dl_state = st) |>
    select(-"yearmonthday")

  assigned_count <-
    issueAssign(twoseason_data, as.numeric(REF_CURRENT_SEASON)) |>
    count(decision)

  expect_equal(
    assigned_count$n[assigned_count$decision == "past"],
    as.integer(days_btwn_starts)
  )

  expect_equal(
    assigned_count$n[assigned_count$decision == "current"],
    396
  )

  expect_equal(
    nrow(filter(assigned_count, decision == "overlap")),
    0
  )

  expect_equal(
    assigned_count$n[assigned_count$decision == "future"],
    365
  )

  expect_equal(
    assigned_count$n[assigned_count$decision == "bad issue dates"],
    as.integer(days_bad)
  )
})

test_that("issueAssign evaluates 1-season states correctly", {

  test_start <- paste0("01/01/", as.numeric(REF_CURRENT_SEASON) - 1)
  test_end <- paste0("12/31/", as.numeric(REF_CURRENT_SEASON) + 2)
  st <- "KY"

  issue_window_begin <- REF_DATES$issue_start[REF_DATES$state == st]
  issue_window_end <- REF_DATES$issue_end[REF_DATES$state == st]

  days_btwn_starts <- issue_window_begin - mdy(test_start)
  days_btwn_ends <- (issue_window_end + 365) - (issue_window_begin + 365) + 1
  days_in_window <- (issue_window_begin + 365) - issue_window_begin
  days_bad <- mdy(test_end) - (issue_window_end + 365)

  oneseason_data <-
    tibble(
      yearmonthday =
        as.character(
          as.Date(
            mdy(test_start):mdy(test_end))),
      issue_date =
        paste(
          stringr::str_sub(yearmonthday, 6, 7),
          stringr::str_sub(yearmonthday, 9, 10),
          stringr::str_sub(yearmonthday, 1, 4),
          sep = "/"),
      registration_yr = REF_CURRENT_SEASON,
      dl_state = st) |>
    select(-"yearmonthday")

  assigned_count <-
    issueAssign(oneseason_data, as.numeric(REF_CURRENT_SEASON)) |>
    count(decision)

  expect_equal(
    assigned_count$n[assigned_count$decision == "past"],
    as.integer(days_btwn_starts)
  )

  expect_equal(
    assigned_count$n[assigned_count$decision == "future"],
    as.integer(days_btwn_ends)
  )

  expect_equal(
    assigned_count$n[assigned_count$decision == "current"],
    as.integer(days_in_window)
  )

  expect_equal(
    assigned_count$n[assigned_count$decision == "bad issue dates"],
    as.integer(days_bad)
  )
})
