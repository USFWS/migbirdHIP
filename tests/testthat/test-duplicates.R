# duplicateFix function ---------------------------------------------------

test_that("exact duplicate removed", {

  # Create an exact duplicate
  duplicated_data <-
    bind_rows(
      DF_TEST_TINI_CURRENT |>
        dplyr::filter(record_key == "record_1") |>
        dplyr::mutate(
          record_key = paste0("record_", nrow(DF_TEST_TINI_CURRENT) + 1)),
      DF_TEST_TINI_CURRENT
    )

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(nrow(DF_TEST_TINI_CURRENT), nrow(deduped_data))
})

test_that("the most recent record in a set of duplicates is kept", {

  dupl_record <-
    DF_TEST_TINI_CURRENT |>
    dplyr::slice_head(n = 1)

  duplicated_data <-
    bind_rows(
      dupl_record,
      dupl_record |>
        mutate(
          issue_date =
            paste(
              stringr::str_sub(lubridate::mdy(issue_date), 6, 7),
              stringr::str_sub(lubridate::mdy(issue_date)-1, 9, 10),
              stringr::str_sub(lubridate::mdy(issue_date), 1, 4),
              sep = "/")))

  newest <- duplicateFix(duplicated_data)

  expect_equal(nrow(newest), 1)
})

# solo permit states, OR and WA -------------------------------------------

test_that("HIP duplicates resolved in solo PMT state, WA", {

  hip_test <-
    DF_TEST_MINI |>
    dplyr::slice_sample(n = 3) |>
    dplyr::mutate(
      dl_state = "WA",
      across(matches("band|brant|seaduck"), \(x) "0"))

  inline_pmt_test <-
    hip_test |>
    dplyr::mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(dplyr::row_number() == 1, "2", "0"),
      brant = ifelse(dplyr::row_number() == 2, "2", "0"),
      seaducks = ifelse(dplyr::row_number() == 3, "2", "0"))

  duplicated_data <- bind_rows(hip_test, hip_test, inline_pmt_test)

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(deduped_data),
    (nrow(hip_test)+nrow(inline_pmt_test))
  )
})

test_that("HIP duplicates resolved in solo PMT state, OR", {

  hip_test <-
    DF_TEST_MINI |>
    dplyr::slice_sample(n = 3) |>
    dplyr::mutate(
      dl_state = "OR",
      across(matches("band|brant|seaduck"), \(x) "0"))

  inline_pmt_test <-
    hip_test |>
    dplyr::mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(dplyr::row_number() == 1, "2", "0"),
      brant = ifelse(dplyr::row_number() == 2, "2", "0"),
      seaducks = ifelse(dplyr::row_number() == 3, "2", "0"))

  duplicated_data <- bind_rows(hip_test, hip_test, inline_pmt_test)

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(deduped_data),
    (nrow(hip_test)+nrow(inline_pmt_test))
  )
})

test_that("multiple solo PMTs retained, WA", {

  hip_test <-
    DF_TEST_MINI |>
    dplyr::slice_sample(n = 3) |>
    dplyr::mutate(
      dl_state = "WA",
      across(matches("band|brant|seaduck"), \(x) "0"))

  inline_pmt_test1 <-
    hip_test |>
    dplyr::mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(dplyr::row_number() == 1, "2", "0"),
      brant = ifelse(dplyr::row_number() == 2, "2", "0"),
      seaducks = ifelse(dplyr::row_number() == 3, "2", "0"))

  inline_pmt_test2 <-
    hip_test |>
    dplyr::mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(dplyr::row_number() == 2, "2", "0"),
      brant = ifelse(dplyr::row_number() == 3, "2", "0"),
      seaducks = ifelse(dplyr::row_number() == 1, "2", "0"))

  duplicated_data <- bind_rows(hip_test, inline_pmt_test1, inline_pmt_test2)

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(deduped_data),
    (nrow(hip_test)+nrow(inline_pmt_test1)+nrow(inline_pmt_test2))
  )
})

test_that("multiple solo PMTs retained, OR", {

  hip_test <-
    DF_TEST_MINI |>
    dplyr::slice_sample(n = 3) |>
    dplyr::mutate(
      dl_state = "OR",
      across(matches("band|brant|seaduck"), \(x) "0"))

  inline_pmt_test1 <-
    hip_test |>
    dplyr::mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(dplyr::row_number() == 1, "2", "0"),
      brant = ifelse(dplyr::row_number() == 2, "2", "0"),
      seaducks = ifelse(dplyr::row_number() == 3, "2", "0"))

  inline_pmt_test2 <-
    hip_test |>
    dplyr::mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(dplyr::row_number() == 2, "2", "0"),
      brant = ifelse(dplyr::row_number() == 3, "2", "0"),
      seaducks = ifelse(dplyr::row_number() == 1, "2", "0"))

  duplicated_data <- bind_rows(hip_test, inline_pmt_test1, inline_pmt_test2)

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(deduped_data),
    (nrow(hip_test)+nrow(inline_pmt_test1)+nrow(inline_pmt_test2))
  )
})

# SD and BR state ---------------------------------------------------------


# SD only state -----------------------------------------------------------

