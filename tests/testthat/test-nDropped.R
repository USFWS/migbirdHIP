test_that("nDropped works", {
  raw_data <- DF_TEST_MINI

  suppressMessages(invisible(capture.output(
    cleaned_data <- clean(DF_TEST_MINI)
  )))
  suppressMessages(invisible(capture.output(
    current_data <- issueCheck(cleaned_data, as.numeric(REF_CURRENT_SEASON))
  )))
  deduped_data <- duplicateFix(current_data)

  n_dropped <-
    nDropped(
      raw_data,
      cleaned_data,
      current_data,
      deduped_data,
      as.numeric(REF_CURRENT_SEASON)
    )

  expect_true(nrow(n_dropped) > 0)
})

test_that("nDropped works by state", {
  raw_data <- DF_TEST_MINI

  suppressMessages(invisible(capture.output(
    cleaned_data <- clean(DF_TEST_MINI)
  )))
  suppressMessages(invisible(capture.output(
    current_data <- issueCheck(cleaned_data, as.numeric(REF_CURRENT_SEASON))
  )))
  deduped_data <- duplicateFix(current_data)

  n_dropped <-
    nDropped(
      raw_data,
      cleaned_data,
      current_data,
      deduped_data,
      as.numeric(REF_CURRENT_SEASON),
      by_state = TRUE
    )

  expect_true(nrow(n_dropped) > 0)
  expect_true(!is.null(n_dropped$dl_state))
})

test_that("nDroppedClean works", {
  raw_data <-
    DF_TEST_MINI |>
    # Set partial NA bag
    dplyr::mutate(
      ducks_bag = ifelse(record_key == "record_1", NA, ducks_bag))

  suppressMessages(invisible(capture.output(
    cleaned_data <- clean(raw_data)
  )))

  n_dropped <-
    nDroppedClean(
      raw_data,
      cleaned_data
    )

  expect_true(nrow(n_dropped) > 0)
})

test_that("nDroppedClean works by state", {
  raw_data <- DF_TEST_MINI
  suppressMessages(invisible(capture.output(
    cleaned_data <- clean(DF_TEST_MINI)
  )))

  n_dropped <-
    nDroppedClean(
      raw_data,
      cleaned_data,
      by_state = TRUE
    )

  expect_true(nrow(n_dropped) > 0)
  expect_true(!is.null(n_dropped$dl_state))
})

test_that("nDroppedCurrent works", {
  raw_data <- DF_TEST_MINI
  suppressMessages(invisible(capture.output(
    cleaned_data <- clean(DF_TEST_MINI)
  )))
  suppressMessages(invisible(capture.output(
    current_data <- issueCheck(cleaned_data, as.numeric(REF_CURRENT_SEASON))
  )))

  n_dropped <-
    nDroppedCurrent(
      cleaned_data,
      current_data,
      as.numeric(REF_CURRENT_SEASON)
    )

  expect_true(nrow(n_dropped) > 0)
})

test_that("Only drop records via clean, issueCheck, and duplicateFix", {
  raw_data <- DF_TEST_MINI
  suppressMessages(invisible(capture.output(
    cleaned_data <- clean(DF_TEST_MINI)
  )))
  suppressMessages(invisible(capture.output(
    current_data <- issueCheck(cleaned_data, as.numeric(REF_CURRENT_SEASON))
  )))
  deduped_data <- duplicateFix(current_data)
  proofed_data <- proof(deduped_data, as.numeric(REF_CURRENT_SEASON))
  corrected_data <- correct(proofed_data, as.numeric(REF_CURRENT_SEASON))

  expect_true(nrow(cleaned_data) < nrow(raw_data))
  expect_true(nrow(current_data) < nrow(cleaned_data))
  expect_true(nrow(deduped_data) < nrow(current_data))
  expect_equal(nrow(deduped_data), nrow(proofed_data))
  expect_equal(nrow(proofed_data), nrow(corrected_data))
})
