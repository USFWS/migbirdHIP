# test record -------------------------------------------------------------

# see test-proof.R

# bag filters -------------------------------------------------------------

test_that("filter out any record if any bag value is not a 1-digit number", {
  suppressMessages(
    test_clean <-
      DF_TEST_TINI_READ |>
      dplyr::mutate(
        ducks_bag =
          case_when(
            record_key == "record_1" ~ "*",
            record_key == "record_2" ~ "A",
            record_key == "record_3" ~ "1",
            TRUE ~ ducks_bag)) |>
      clean())

  expect_equal(nrow(DF_TEST_TINI_READ)-2, nrow(test_clean))
})

test_that("filter out any record with all-NA or all-0 bag values", {
  test_data <-
    bind_rows(
      DF_TEST_TINI_READ |>
        filter(record_key == "record_1") |>
        mutate(across(contains(REF_BAG_FIELDS), \(x) "0")),
      DF_TEST_TINI_READ |>
        filter(record_key == "record_2") |>
        mutate(across(contains(REF_BAG_FIELDS), \(x) "1")),
      DF_TEST_TINI_READ |>
        filter(record_key == "record_3") |>
        mutate(across(contains(REF_BAG_FIELDS), \(x) NA)))

  suppressMessages(test_clean <- clean(test_data))

  expect_equal(nrow(DF_TEST_TINI_READ)-2, nrow(test_clean))
})

# missing PII filter ------------------------------------------------------

test_that("filter out if firstname, lastname, state, or DOB missing", {

  test_missing <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 10) |>
    dplyr::mutate(
      firstname = ifelse(record_key == "record_1", NA, firstname),
      lastname = ifelse(record_key == "record_2", NA, lastname),
      state = ifelse(record_key == "record_3", NA, state),
      birth_date = ifelse(record_key == "record_4", NA, birth_date))

  suppressMessages(invisible(capture.output(test_clean <- clean(test_missing))))

  expect_equal(nrow(test_missing)-4, nrow(test_clean))
})

test_that("filter out if email AND address are missing", {
  test_missing <-
    DF_TEST_TINI_READ |>
    dplyr::mutate(
      email = ifelse(record_key == "record_1", NA, email),
      address = ifelse(record_key == "record_1", NA, address))

  test_clean <- suppressMessages(clean(test_missing))

  expect_equal(nrow(test_missing)-1, nrow(test_clean))
})

test_that("filter out if email AND city AND zip are missing", {
  test_missing <-
    DF_TEST_TINI_READ |>
    dplyr::mutate(
      email = ifelse(record_key == "record_1", NA, email),
      city = ifelse(record_key == "record_1", NA, city),
      zip = ifelse(record_key == "record_1", NA, zip))

  test_clean <- suppressMessages(clean(test_missing))

  expect_equal(nrow(test_missing)-1, nrow(test_clean))
})

# names to uppercase ------------------------------------------------------

test_that("firstname converted to uppercase", {
  expect_true(
    unique(stringr::str_detect(DF_TEST_TINI_CLEANED$firstname, "^[^a-z]+$"))
  )
})

test_that("lastname converted to uppercase", {
  expect_true(
    unique(stringr::str_detect(DF_TEST_TINI_CLEANED$lastname, "^[^a-z]+$"))
  )
})

test_that("suffix converted to uppercase", {
  suppressMessages(
    test_clean <-
      DF_TEST_TINI_READ |>
      dplyr::mutate(suffix = "ii") |>
      clean())

  expect_true(
    unique(stringr::str_detect(test_clean$suffix, "^[^a-z]+$"))
  )
})

# test record filter ------------------------------------------------------

test_that("filter out test records", {

  test_data <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 10) |>
    dplyr::mutate(
      firstname =
        dplyr::case_when(
          record_key == "record_1" ~ "TEST",
          record_key == "record_2" ~ "TEST",
          record_key == "record_3" ~ "INAUDIBLE",
          record_key == "record_4" ~ "BLANK",
          record_key == "record_5" ~ "USER",
          record_key == "record_6" ~ "RESIDENT",
          TRUE ~ firstname),
      lastname =
        dplyr::case_when(
          record_key == "record_1" ~ "TEST",
          record_key == "record_7" ~ "INAUDIBLE",
          TRUE ~ lastname))

  suppressMessages(invisible(capture.output(test_clean <- clean(test_data))))

  expect_equal(nrow(test_data)-7, nrow(test_clean))
})

# zip formatting ----------------------------------------------------------

test_that("zips formatted correctly", {
  test_data <-
    tibble(
      zip =
        c("41898",
          "29022-0676",
          "37852-",
          "1509222790",
          "674425228",
          "85568 2880",
          "36517-0000",
          "53768-____"))

  correct_data <-
    tibble(
      zip =
        c("41898",
          "29022-0676",
          "37852",
          "15092-2279",
          "67442-5228",
          "85568-2880",
          "36517",
          "53768"))

  reformatted_test_data <- formatZip(test_data)

  expect_equal(correct_data, reformatted_test_data)
})

# solo permit DNH ---------------------------------------------------------

test_that("change in-line permit record hunt_mig_birds value to 2", {

  inline_pmt_test <-
    DF_TEST_MINI |>
    dplyr::slice_sample(n = 3) |>
    dplyr::mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(dplyr::row_number() == 1, "2", "0"),
      brant = ifelse(dplyr::row_number() == 2, "2", "0"),
      seaducks = ifelse(dplyr::row_number() == 3, "2", "0"))

  test_data <-
    bind_rows(
      inline_pmt_test |>
        mutate(
          dl_state = "WA",
          hunt_mig_birds = "0"),
      inline_pmt_test |>
        mutate(
          dl_state = "WA",
          hunt_mig_birds = "1"),
      inline_pmt_test |>
        mutate(
          dl_state = "OR",
          hunt_mig_birds = "0"),
      inline_pmt_test |>
        mutate(
          dl_state = "OR",
          hunt_mig_birds = "1")
      )

  cleaned_data <- suppressMessages(clean(test_data))

  expect_true(unique(cleaned_data$hunt_mig_birds) == "2")
})

# permit file bag values changed to 0 -------------------------------------

test_that("change crane permit file state crane bag values to 0", {
  test_data <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 12) |>
    dplyr::mutate(
      dl_state =
        dplyr::case_when(
          record_key == "record_1" ~ "CO",
          record_key == "record_2" ~ "KS",
          record_key == "record_3" ~ "MN",
          record_key == "record_4" ~ "MT",
          record_key == "record_5" ~ "ND",
          record_key == "record_6" ~ "NM",
          record_key == "record_7" ~ "OK",
          record_key == "record_8" ~ "TX",
          record_key == "record_9" ~ "WY",
          TRUE ~ "SD"),
      cranes = "2")

  suppressMessages(invisible(capture.output(cleaned_data <- clean(test_data))))

  expect_equal(
    nrow(test_data)-9,
    nrow(filter(cleaned_data, cranes == "2"))
  )

})

test_that("change BTPI permit file state BTPI bag values to 0", {
  test_data <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 10) |>
    missingPIIFilter() |>
    dplyr::mutate(
      dl_state =
        dplyr::case_when(
          record_key == "record_1" ~ "CO",
          record_key == "record_2" ~ "NM",
          record_key == "record_3" ~ "UT",
          record_key == "record_4" ~ "CA",
          TRUE ~ "AZ"),
      band_tailed_pigeon = "2")

  suppressMessages(invisible(capture.output(cleaned_data <- clean(test_data))))

  expect_equal(
    nrow(test_data)-3,
    nrow(filter(cleaned_data, band_tailed_pigeon == "2"))
  )

})

# suffixes ----------------------------------------------------------------

test_that("suffixes found in firstname", {
  firstname_suffixes <-
    tibble(
      first = rep(c("J'AMES", "JAY", "JOHN JAY", "JOHN-JAY", "JOHN"), 9),
      suffix = c(REF_SUFFIXES, " JR.", " SR.", "1ST.", "  II."),
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
      suffix = c(REF_SUFFIXES, " JR.", " SR.", "1ST.", "  II."),
      lastname = paste(last, suffix)) |>
    select(lastname)

  lastname_suffixes_filtered <-
    lastname_suffixes |>
    filter(str_detect(lastname, REGEX_SUFFIX_SEARCH))

  expect_equal(nrow(lastname_suffixes), nrow(lastname_suffixes_filtered))
})

test_that("suffixes moved from firstname to suffix", {

  test_data <-
    tibble(
      firstname = paste("JOHN", c(REF_SUFFIXES, "JR.", "SR."), sep = " "),
      suffix = NA,
      lastname = "SMITH")

  suffixes_moved <- moveSuffixes(test_data)

  bad_move <-
    suffixes_moved |>
    filter(
      str_detect(firstname, "\\.") |
        str_detect(lastname, "\\.") |
        str_detect(
          firstname,
          paste0(
            "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|",
            "XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$")))

  expect_equal(nrow(bad_move), 0)
})

test_that("suffixes moved from lastname to suffix", {

  test_data <-
    tibble(
      firstname = "JOHN",
      suffix = NA,
      lastname = paste("SMITH", c(REF_SUFFIXES, "JR.", "SR."), sep = " "))

  suffixes_moved <- moveSuffixes(test_data)

  bad_move <-
    suffixes_moved |>
    filter(
      str_detect(lastname, "\\.") |
        str_detect(lastname, "\\.") |
        str_detect(
          lastname,
          paste0(
            "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|",
            "XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$")))

  expect_equal(nrow(bad_move), 0)
})
