# test record -------------------------------------------------------------

# see test-proof.R


# suffixes ----------------------------------------------------------------

test_that("suffixes found in firstname", {
  firstname_suffixes <-
    tibble(
      first = rep(c("J'AMES", "JAY", "JOHN JAY", "JOHN-JAY", "JOHN"), 9),
      suffix = c(REF_SUFFIXES, "JR.", "SR.", " JR", " SR.", "1ST.", "  II."),
      firstname = paste(first, suffix)) |>
    select(firstname)

  firstname_suffixes_filtered <-
    firstname_suffixes |>
    filter(str_detect(firstname, REGEX_SUFFIX_SEARCH))

  expect_equal(nrow(firstname_suffixes), nrow(firstname_suffixes_filtered))
})

test_that("suffixes found in lastname", {
  lastname_suffixes <-
    tibble(
      last = rep(c("MAL'LARD", "DOE", "BLUE WING", "PIN-TAIL", "ST. DUCK"), 9),
      suffix = c(REF_SUFFIXES, "JR.", "SR.", " JR", " SR.", "1ST.", "  II."),
      lastname = paste(last, suffix)) |>
    select(lastname)

  lastname_suffixes_filtered <-
    lastname_suffixes |>
    filter(str_detect(lastname, REGEX_SUFFIX_SEARCH))

  expect_equal(nrow(lastname_suffixes), nrow(lastname_suffixes_filtered))
})
