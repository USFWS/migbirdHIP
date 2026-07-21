# correct function --------------------------------------------------------

test_that("correct input and output have the same number of records", {

  test_correct <- correct(DF_TEST_TINI_PROOFED, as.numeric(REF_CURRENT_SEASON))
  expect_equal(nrow(DF_TEST_TINI_PROOFED), nrow(test_correct))
})

test_that("correct title works", {

  bad_title <-
    DF_TEST_TINI_PROOFED |>
    mutate(
      title = ifelse(record_key == "record_1", 3, title),
      errors = ifelse(record_key == "record_1", "title", NA))

  test_correct <- correct(bad_title, as.numeric(REF_CURRENT_SEASON))
  expect_false(identical(test_correct$title, bad_title$title))
})

test_that("correct suffix works", {

  bad_suffix <-
    DF_TEST_TINI_PROOFED |>
    mutate(
      suffix = ifelse(record_key == "record_1", "ESQ", suffix),
      errors = ifelse(record_key == "record_1", "suffix", NA))

  test_correct <- correct(bad_suffix, as.numeric(REF_CURRENT_SEASON))
  expect_false(identical(test_correct$suffix, bad_suffix$suffix))
})

test_that("correct middle initial works", {

  bad_middle <-
    DF_TEST_TINI_PROOFED |>
    mutate(
      middle = ifelse(record_key == "record_1", "3", middle),
      errors = ifelse(record_key == "record_1", "middle", NA))

  test_correct <- correct(bad_middle, as.numeric(REF_CURRENT_SEASON))
  expect_false(identical(test_correct$middle, bad_middle$middle))
})

test_that("correct email: adding period before TLD works", {

  bad_email <-
    DF_TEST_TINI_PROOFED |>
    mutate(
      email = ifelse(record_key == "record_1", "bugsbunny@gmailcom", email),
      errors = ifelse(record_key == "record_1", "email", NA))

  test_correct <- correct(bad_email, as.numeric(REF_CURRENT_SEASON))
  expect_false(identical(test_correct$email, bad_email$email))
})

# correctEmail function ---------------------------------------------------

# The following tests for correctEmail() were generated with AI assistance using
# Claude Opus 4.8 in Perplexity on July 13, 2026.

# Tests for the internal helper correctEmail()

# Internal-coupling tradeoff: correctEmail() is not exported, so these call it
# via migbirdHIP:::correctEmail(). That couples the test to an internal name;
# the payoff is direct, per-arm coverage of the ~35 replace_when TLD/domain
# fixes that are otherwise only exercised opaquely through correct().

# NOTE: "str_length > 100" does not exist in correctEmail(). The length check
# lives in proofBadEmails(), which is a proofing check, not a correction. Not
# tested here.

# correctEmail requires an `errors` column (failProofed) and operates only on
# `email`. A minimal 2-column fixture is sufficient and keeps each case legible.

ce <- function(x) {
  migbirdHIP:::correctEmail(
    dplyr::tibble(email = x, errors = NA_character_)
  )$email
}

test_that("correctEmail appends the missing top-level domain per provider", {
  cases <- c(
    "a@gmail"      = "a@gmail.com",
    "a@yahoo"      = "a@yahoo.com",
    "a@hotmail"    = "a@hotmail.com",
    "a@aol"        = "a@aol.com",
    "a@icloud"     = "a@icloud.com",
    "a@comcast"    = "a@comcast.net",
    "a@outlook"    = "a@outlook.com",
    "a@sbcglobal"  = "a@sbcglobal.net",
    "a@att"        = "a@att.net",
    "a@msn"        = "a@msn.com",
    "a@live"       = "a@live.com",
    "a@bellsouth"  = "a@bellsouth.net",
    "a@charter"    = "a@charter.net",
    "a@ymail"      = "a@ymail.com",
    "a@me"         = "a@me.com",
    "a@verizon"    = "a@verizon.net",
    "a@cox"        = "a@cox.net",
    "a@earthlink"  = "a@earthlink.net",
    "a@protonmail" = "a@protonmail.com",
    "a@pm"         = "a@pm.me",
    "a@mail"       = "a@mail.com",
    "a@duck"       = "a@duck.com",
    "a@ducks"      = "a@ducks.org"
  )
  for (i in seq_along(cases)) {
    expect_identical(ce(names(cases)[i]), unname(cases[i]),
                     info = names(cases)[i])
  }
})

test_that("correctEmail inserts the missing period before a generic TLD", {
  expect_identical(ce("a@domaincom"), "a@domain.com")
  expect_identical(ce("a@domainnet"), "a@domain.net")
  expect_identical(ce("a@schooledu"), "a@school.edu")
  expect_identical(ce("a@agencygov"), "a@agency.gov")
  expect_identical(ce("a@grouporg"),  "a@group.org")
})

test_that("correctEmail fixes concatenated military domains", {
  expect_identical(ce("a@navymil"),      "a@navy.mil")
  expect_identical(ce("a@usnavymil"),    "a@us.navy.mil")
  expect_identical(ce("a@usafmil"),      "a@us.af.mil")
  expect_identical(ce("a@mailmil"),      "a@mail.mil")
  expect_identical(ce("a@armymil"),      "a@army.mil")
  expect_identical(ce("a@usarmymil"),    "a@us.army.mil")
  expect_identical(ce("a@usacearmymil"), "a@us.ace.army.mil")
})

test_that("correctEmail's com$ arm repairs a provider missing its dot", {
  expect_identical(ce("a@gmailcom"), "a@gmail.com")
})

test_that("correctEmail leaves already-valid emails and NA unchanged", {
  expect_identical(ce("a@gmail.com"), "a@gmail.com")
  expect_identical(ce("a@navy.mil"),  "a@navy.mil")
  expect_identical(ce(NA_character_), NA_character_)
})
