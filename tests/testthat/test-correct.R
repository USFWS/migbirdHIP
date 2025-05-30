# correct function --------------------------------------------------------

test_that("correct input and output have the same number of records", {

  test_correct <- correct(DF_TEST_TINI_PROOFED, as.numeric(REF_CURRENT_SEASON))
  expect_equal(nrow(DF_TEST_TINI_PROOFED), nrow(test_correct))
})

# title -------------------------------------------------------------------

test_that("correct title works", {

  bad_title <-
    DF_TEST_TINI_PROOFED |>
    mutate(
      title = ifelse(record_key == "record_1", 3, title),
      errors = ifelse(record_key == "record_1", "title", NA))

  test_correct <- correct(bad_title, as.numeric(REF_CURRENT_SEASON))
  expect_false(identical(test_correct$title, bad_title$title))
})

# suffix ------------------------------------------------------------------

test_that("correct suffix works", {

  bad_suffix <-
    DF_TEST_TINI_PROOFED |>
    mutate(
      suffix = ifelse(record_key == "record_1", "ESQ", suffix),
      errors = ifelse(record_key == "record_1", "suffix", NA))

  test_correct <- correct(bad_suffix, as.numeric(REF_CURRENT_SEASON))
  expect_false(identical(test_correct$suffix, bad_suffix$suffix))
})

# middle initial ----------------------------------------------------------

test_that("correct middle initial works", {

  bad_middle <-
    DF_TEST_TINI_PROOFED |>
    mutate(
      middle = ifelse(record_key == "record_1", "3", middle),
      errors = ifelse(record_key == "record_1", "middle", NA))

  test_correct <- correct(bad_middle, as.numeric(REF_CURRENT_SEASON))
  expect_false(identical(test_correct$middle, bad_middle$middle))
})

# email -------------------------------------------------------------------

test_that("correct email initial works", {

  bad_email <-
    DF_TEST_TINI_PROOFED |>
    mutate(
      email = ifelse(record_key == "record_1", "bugsbunny@gmailcom", email),
      errors = ifelse(record_key == "record_1", "email", NA))

  test_correct <- correct(bad_email, as.numeric(REF_CURRENT_SEASON))
  expect_false(identical(test_correct$email, bad_email$email))
})
