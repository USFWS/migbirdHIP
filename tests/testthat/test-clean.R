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
  suppressMessages(
    test_clean <-
      DF_TEST_TINI_READ |>
      dplyr::mutate(
        ducks_bag =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ ducks_bag),
        geese_bag =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ geese_bag),
        dove_bag =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ dove_bag),
        woodcock_bag =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ woodcock_bag),
        coots_snipe =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ coots_snipe),
        rails_gallinules =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ rails_gallinules),
        cranes =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ cranes),
        band_tailed_pigeon =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ band_tailed_pigeon),
        brant =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ brant),
        seaducks =
          dplyr::case_when(
            record_key == "record_1" ~ "0",
            record_key == "record_2" ~ NA,
            record_key == "record_3" ~ "1",
            TRUE ~ seaducks)) |>
      clean())

  expect_equal(nrow(DF_TEST_TINI_READ)-2, nrow(test_clean))
})

# missing PII filter ------------------------------------------------------

test_that("filter out if firstname, lastname, state, or DOB missing", {

  test_missing <-
    bind_rows(
      DF_TEST_TINI_READ |>
        dplyr::mutate(
          firstname = ifelse(record_key == "record_1", NA, firstname),
          lastname = ifelse(record_key == "record_2", NA, lastname),
          state = ifelse(record_key == "record_3", NA, state),
        ),
      DF_TEST_TINI_READ |>
        dplyr::mutate(
          record_key =
            dplyr::case_when(
              record_key == "record_1" ~ "record_a",
              record_key == "record_2" ~ "record_b",
              record_key == "record_3" ~ "record_c",
              TRUE ~ record_key),
          birth_date = ifelse(record_key == "record_a", NA, birth_date)
        )
      )

  test_clean <- suppressMessages(clean(test_missing))

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

# # Convert firstname, lastname, and suffix to upper case
# namesToUppercase()

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

  test_missing <-
    bind_rows(
      DF_TEST_TINI_READ |>
        dplyr::mutate(
          firstname =
            dplyr::case_when(
              record_key == "record_1" ~ "TEST",
              record_key == "record_2" ~ "TEST",
              record_key == "record_3" ~ "INAUDIBLE",
              TRUE ~ record_key),
          lastname = ifelse(record_key == "record_1", "TEST", lastname)),
      DF_TEST_TINI_READ |>
        dplyr::mutate(
          firstname =
            dplyr::case_when(
              record_key == "record_1" ~ "BLANK",
              record_key == "record_2" ~ "USER",
              record_key == "record_3" ~ "RESIDENT",
              TRUE ~ firstname)),
      DF_TEST_TINI_READ |>
        dplyr::mutate(
          lastname = ifelse(record_key == "record_1", "INAUDIBLE", lastname))
    )

  test_clean <- suppressMessages(clean(test_missing))

  expect_equal(nrow(test_missing)-7, nrow(test_clean))
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
  test_data <-
    dplyr::bind_rows(
      DF_TEST_TINI_READ,
      DF_TEST_TINI_READ |>
        mutate(
          record_key =
            dplyr::case_when(
              record_key == "record_1" ~ "record_a",
              record_key == "record_2" ~ "record_b",
              record_key == "record_3" ~ "record_c",
              TRUE ~ record_key))
      ) |>
    dplyr::mutate(
      hunt_mig_birds =
        dplyr::case_when(
          record_key == "record_1" ~ "0",
          record_key == "record_a" ~ "1",
          record_key == "record_2" ~ "1",
          record_key == "record_b" ~ "0",
          record_key %in% c("record_3", "record_c") ~ "2",
          TRUE ~ ducks_bag),
      dl_state =
        dplyr::case_when(
          record_key %in% c("record_1", "record_a") ~ "WA",
          record_key %in% c("record_2", "record_b") ~ "OR",
          record_key %in% c("record_3", "record_c") ~ "IL",
          TRUE ~ ducks_bag),
      ducks_bag =
        dplyr::case_when(
          record_key %in% c("record_1", "record_a") ~ "0",
          record_key %in% c("record_2", "record_b") ~ "0",
          record_key %in% c("record_3", "record_c") ~ ducks_bag,
          TRUE ~ ducks_bag),
      geese_bag =
        dplyr::case_when(
          record_key %in% c("record_1", "record_a") ~ "0",
          record_key %in% c("record_2", "record_b") ~ "0",
          record_key %in% c("record_3", "record_c") ~ geese_bag,
          TRUE ~ geese_bag),
      dove_bag =
        dplyr::case_when(
          record_key %in% c("record_1", "record_a") ~ "0",
          record_key %in% c("record_2", "record_b") ~ "0",
          record_key %in% c("record_3", "record_c") ~ dove_bag,
          TRUE ~ dove_bag),
      woodcock_bag =
        dplyr::case_when(
          record_key %in% c("record_1", "record_a") ~ "0",
          record_key %in% c("record_2", "record_b") ~ "0",
          record_key %in% c("record_3", "record_c") ~ woodcock_bag,
          TRUE ~ woodcock_bag),
      coots_snipe =
        dplyr::case_when(
          record_key %in% c("record_1", "record_a") ~ "0",
          record_key %in% c("record_2", "record_b") ~ "0",
          record_key %in% c("record_3", "record_c") ~ coots_snipe,
          TRUE ~ coots_snipe),
      rails_gallinules =
        dplyr::case_when(
          record_key %in% c("record_1", "record_a") ~ "0",
          record_key %in% c("record_2", "record_b") ~ "0",
          record_key %in% c("record_3", "record_c") ~ rails_gallinules,
          TRUE ~ rails_gallinules),
      cranes =
        dplyr::case_when(
          record_key %in% c("record_1", "record_a") ~ "0",
          record_key %in% c("record_2", "record_b") ~ "0",
          record_key %in% c("record_3", "record_c") ~ cranes,
          TRUE ~ cranes),
      band_tailed_pigeon =
        dplyr::case_when(
          record_key == "record_1" ~ "2",
          record_key == "record_a" ~ "2",
          record_key == "record_2" ~ "0",
          record_key == "record_b" ~ "0",
          record_key %in% c("record_3", "record_c") ~ band_tailed_pigeon,
          TRUE ~ band_tailed_pigeon),
      brant =
        dplyr::case_when(
          record_key == "record_1" ~ "2",
          record_key == "record_a" ~ "0",
          record_key == "record_2" ~ "2",
          record_key == "record_b" ~ "0",
          record_key %in% c("record_3", "record_c") ~ brant,
          TRUE ~ brant),
      seaducks =
        dplyr::case_when(
          record_key == "record_1" ~ "2",
          record_key == "record_a" ~ "0",
          record_key == "record_2" ~ "0",
          record_key == "record_b" ~ "2",
          record_key %in% c("record_3", "record_c") ~ seaducks,
          TRUE ~ seaducks))

  cleaned_data <- suppressMessages(clean(test_data))

  expect_true(unique(cleaned_data$hunt_mig_birds) == "2")
})

# permit file bag values changed to 0 -------------------------------------

test_that("change crane permit file state crane bag values to 0", {
  test_data <-
    dplyr::bind_rows(
      DF_TEST_TINI_READ,
      DF_TEST_TINI_READ |>
        mutate(
          record_key =
            dplyr::case_when(
              record_key == "record_1" ~ "record_a",
              record_key == "record_2" ~ "record_b",
              record_key == "record_3" ~ "record_c",
              TRUE ~ record_key)),
      DF_TEST_TINI_READ |>
        mutate(
          record_key =
            dplyr::case_when(
              record_key == "record_1" ~ "record_x",
              record_key == "record_2" ~ "record_y",
              record_key == "record_3" ~ "record_z",
              TRUE ~ record_key)),
      DF_TEST_TINI_READ |>
        mutate(
          record_key =
            dplyr::case_when(
              record_key == "record_1" ~ "record_q",
              record_key == "record_2" ~ "record_r",
              record_key == "record_3" ~ "record_s",
              TRUE ~ record_key))
    ) |>
    mutate(
      dl_state =
        dplyr::case_when(
          record_key == "record_1" ~ "CO",
          record_key == "record_2" ~ "KS",
          record_key == "record_3" ~ "MN",
          record_key == "record_a" ~ "MT",
          record_key == "record_b" ~ "ND",
          record_key == "record_c" ~ "NM",
          record_key == "record_x" ~ "OK",
          record_key == "record_y" ~ "TX",
          record_key == "record_z" ~ "WY",
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
    dplyr::bind_rows(
      DF_TEST_TINI_READ,
      DF_TEST_TINI_READ |>
        mutate(
          record_key =
            dplyr::case_when(
              record_key == "record_1" ~ "record_a",
              record_key == "record_2" ~ "record_b",
              record_key == "record_3" ~ "record_c",
              TRUE ~ record_key))
    ) |>
    mutate(
      dl_state =
        dplyr::case_when(
          record_key == "record_1" ~ "CO",
          record_key == "record_2" ~ "NM",
          record_key == "record_3" ~ "UT",
          record_key == "record_a" ~ "CA",
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
