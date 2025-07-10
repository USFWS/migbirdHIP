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
    dplyr::select(
      c("record_key", "issue_date", "registration_yr", "dl_state")) |>
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

test_that("issueAssign evaluates 2-season states correctly", {

  twoseason_data <-
    # Past
    dplyr::tibble(
      issue_date = c("02/01/2023"),
      registration_yr = as.numeric(REF_CURRENT_SEASON),
      dl_state = REF_DATES$state[REF_DATES$category == "2 season"],
      answer = "past") |>
    # Current
    dplyr::bind_rows(
      dplyr::tibble(
        issue_date = c("12/01/2024"),
        registration_yr = as.numeric(REF_CURRENT_SEASON),
        dl_state = REF_DATES$state[REF_DATES$category == "2 season"],
        answer = "current")
    ) |>
    # Future
    dplyr::bind_rows(
      dplyr::tibble(
        issue_date = c("03/11/2025"),
        registration_yr = as.numeric(REF_CURRENT_SEASON) + 1,
        dl_state = REF_DATES$state[REF_DATES$category == "2 season"],
        answer = "future")
    )

  assigned <- issueAssign(twoseason_data, as.numeric(REF_CURRENT_SEASON))

  expect_equal(assigned$answer, assigned$decision)
})

test_that("issueAssign evaluates 2-season states correctly for all dates", {

  start <- "01/01/2023"
  end <- "12/31/2026"
  st <- "CT"

  window_begin <- REF_DATES$issue_start[REF_DATES$state == st]
  window_end <- REF_DATES$last_day_migbird_hunting[REF_DATES$state == st]

  days_btwn_starts <- window_begin - lubridate::mdy(start)
  days_btwn_ends <- lubridate::mdy(end) - window_end
  days_in_window <- window_end - window_begin + 1

  twoseason_data <-
    dplyr::tibble(
      yearmonthday =
        as.character(
          as.Date(
            lubridate::mdy(start):lubridate::mdy(end))),
      issue_date =
        paste(
          stringr::str_sub(yearmonthday, 6, 7),
          stringr::str_sub(yearmonthday, 9, 10),
          stringr::str_sub(yearmonthday, 1, 4),
          sep = "/"),
      registration_yr = as.numeric(REF_CURRENT_SEASON),
      dl_state = st) |>
    dplyr::select(-"yearmonthday")

  assigned_count <-
    issueAssign(twoseason_data, as.numeric(REF_CURRENT_SEASON)) |>
    dplyr::count(decision)

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
})

test_that("issueAssign evaluates 1-season states correctly", {

  oneseason_data <-
    # Past
    dplyr::tibble(
      issue_date = c("02/01/2023"),
      registration_yr = as.numeric(REF_CURRENT_SEASON),
      dl_state = REF_DATES$state[REF_DATES$category == "1 season"],
      answer = "past") |>
    # Current
    dplyr::bind_rows(
      dplyr::tibble(
        issue_date = c("12/01/2024"),
        registration_yr = as.numeric(REF_CURRENT_SEASON),
        dl_state = REF_DATES$state[REF_DATES$category == "1 season"],
        answer = "current")
    ) |>
    # Future
    dplyr::bind_rows(
      dplyr::tibble(
        issue_date = c("03/11/2025"),
        registration_yr = as.numeric(REF_CURRENT_SEASON) + 1,
        dl_state = REF_DATES$state[REF_DATES$category == "1 season"],
        answer = "future")
    )

  assigned <- issueAssign(twoseason_data, as.numeric(REF_CURRENT_SEASON))

  expect_equal(assigned$answer, assigned$decision)
})

test_that("issueAssign evaluates 1-season states correctly for all dates", {

  start <- "01/01/2023"
  end <- "12/31/2026"
  st <- "KY"

  window_begin <- REF_DATES$issue_start[REF_DATES$state == st]
  window_end <- REF_DATES$last_day_migbird_hunting[REF_DATES$state == st]

  days_btwn_starts <- window_begin - lubridate::mdy(start)
  days_btwn_ends <- lubridate::mdy(end) - window_end
  days_in_window <- window_end - window_begin + 1

  twoseason_data <-
    dplyr::tibble(
      yearmonthday =
        as.character(
          as.Date(
            lubridate::mdy(start):lubridate::mdy(end))),
      issue_date =
        paste(
          stringr::str_sub(yearmonthday, 6, 7),
          stringr::str_sub(yearmonthday, 9, 10),
          stringr::str_sub(yearmonthday, 1, 4),
          sep = "/"),
      registration_yr = as.numeric(REF_CURRENT_SEASON),
      dl_state = st) |>
    dplyr::select(-"yearmonthday")

  assigned_count <-
    issueAssign(twoseason_data, as.numeric(REF_CURRENT_SEASON)) |>
    dplyr::count(decision)

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
})
