# duplicateFix function ---------------------------------------------------

test_that("exact duplicate removed", {

  # Create an exact duplicate
  duplicated_data <-
    bind_rows(
      DF_TEST_TINI_CURRENT |>
        filter(record_key == "record_1") |>
        mutate(
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
              stringr::str_sub(mdy(issue_date), 6, 7),
              stringr::str_sub(mdy(issue_date) - 1, 9, 10),
              stringr::str_sub(mdy(issue_date), 1, 4),
              sep = "/")))

  newest <- duplicateFix(duplicated_data)

  expect_equal(nrow(newest), 1)
})

# duplicateFix: solo permit states, OR and WA -----------------------------

test_that("HIP duplicates resolved in solo PMT state, WA", {

  hip_test <-
    DF_TEST_MINI |>
    slice_sample(n = 3) |>
    mutate(
      dl_state = "WA",
      across(matches("band|brant|seaduck"), \(x) "0"))

  inline_pmt_test <-
    hip_test |>
    mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(row_number() == 1, "2", "0"),
      brant = ifelse(row_number() == 2, "2", "0"),
      seaducks = ifelse(row_number() == 3, "2", "0"))

  duplicated_data <- bind_rows(hip_test, hip_test, inline_pmt_test)

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(deduped_data),
    (nrow(hip_test) + nrow(inline_pmt_test))
  )
})

test_that("HIP duplicates resolved in solo PMT state, OR", {

  hip_test <-
    DF_TEST_MINI |>
    slice_sample(n = 3) |>
    mutate(
      dl_state = "OR",
      across(matches("band|brant|seaduck"), \(x) "0"))

  inline_pmt_test <-
    hip_test |>
    mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(row_number() == 1, "2", "0"),
      brant = ifelse(row_number() == 2, "2", "0"),
      seaducks = ifelse(row_number() == 3, "2", "0"))

  duplicated_data <- bind_rows(hip_test, hip_test, inline_pmt_test)

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(deduped_data),
    (nrow(hip_test) + nrow(inline_pmt_test))
  )
})

test_that("multiple solo PMTs retained, WA", {

  hip_test <-
    DF_TEST_MINI |>
    slice_sample(n = 3) |>
    mutate(
      dl_state = "WA",
      across(matches("band|brant|seaduck"), \(x) "0"))

  inline_pmt_test1 <-
    hip_test |>
    mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(row_number() == 1, "2", "0"),
      brant = ifelse(row_number() == 2, "2", "0"),
      seaducks = ifelse(row_number() == 3, "2", "0"))

  inline_pmt_test2 <-
    hip_test |>
    mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(row_number() == 2, "2", "0"),
      brant = ifelse(row_number() == 3, "2", "0"),
      seaducks = ifelse(row_number() == 1, "2", "0"))

  duplicated_data <- bind_rows(hip_test, inline_pmt_test1, inline_pmt_test2)

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(deduped_data),
    (nrow(hip_test) + nrow(inline_pmt_test1) + nrow(inline_pmt_test2))
  )
})

test_that("multiple solo PMTs retained, OR", {

  hip_test <-
    DF_TEST_MINI |>
    slice_sample(n = 3) |>
    mutate(
      dl_state = "OR",
      across(matches("band|brant|seaduck"), \(x) "0"))

  inline_pmt_test1 <-
    hip_test |>
    mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(row_number() == 1, "2", "0"),
      brant = ifelse(row_number() == 2, "2", "0"),
      seaducks = ifelse(row_number() == 3, "2", "0"))

  inline_pmt_test2 <-
    hip_test |>
    mutate(
      across(matches("bag|crane|coot|rail"), \(x) "0"),
      band_tailed_pigeon = ifelse(row_number() == 2, "2", "0"),
      brant = ifelse(row_number() == 3, "2", "0"),
      seaducks = ifelse(row_number() == 1, "2", "0"))

  duplicated_data <- bind_rows(hip_test, inline_pmt_test1, inline_pmt_test2)

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(deduped_data),
    (nrow(hip_test) + nrow(inline_pmt_test1) + nrow(inline_pmt_test2))
  )
})

# duplicateFix: SD and BR state -------------------------------------------

test_that("HIP duplicates resolved in SD and BR state, DE", {

  hip_test <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 3) |>
    mutate(
      dl_state = "DE",
      brant =
        replace_when(
          .data$brant,
          record_key == "record_1" ~ "2",
          record_key == "record_2" ~ "0",
          record_key == "record_3" ~ "2"),
      seaducks =
        replace_when(
          .data$seaducks,
          record_key == "record_1" ~ "0",
          record_key == "record_2" ~ "2",
          record_key == "record_3" ~ "2"))

  duplicated_data <-
    bind_rows(
      hip_test,
      hip_test |> mutate(brant = "0", seaducks = "0"))

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(hip_test),
    nrow(deduped_data)
  )

  br <- c("record_1", "record_3")
  sd <- c("record_2", "record_3")

  expect_true(
    "2" == unique(deduped_data$brant[deduped_data$record_key %in% br])
  )

  expect_true(
    "2" == unique(deduped_data$seaducks[deduped_data$record_key %in% sd])
  )
})


# duplicateFix: SD only state ---------------------------------------------

test_that("HIP duplicates resolved in SD and BR state, ME", {

  hip_test <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 1) |>
    mutate(
      dl_state = "ME",
      brant = "0",
      seaducks = "2")

  duplicated_data <-
    bind_rows(
      hip_test,
      hip_test |> mutate(seaducks = "0"))

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(
    nrow(hip_test),
    nrow(deduped_data)
  )

  expect_true("2" == deduped_data$seaducks)
})

# duplicateFinder function ------------------------------------------------

# The following tests for duplicateFinder() were generated with AI assistance
# using Claude Opus 4.8 in Perplexity on July 13, 2026.

# Tests for duplicateFinder(). Indirectly also covers the internal
# duplicateFields() and duplicateRecordType().

# Build a fixture with a single, deliberately injected duplicate hunter: two
# records identical across every REF_FIELDS_HUNTER_ID field but differing only
# in `email`. duplicateFinder() should report exactly one duplicated hunter
# whose cause-of-duplication field is "email".
inject_email_dupe <- function() {
  base <- DF_TEST_MINI |> dplyr::slice_head(n = 5)
  dupe <- base |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(
      email = "second-address@example.com",
      record_key = "rk_injected")
  dplyr::bind_rows(base, dupe)
}

test_that("duplicateFinder returns a tibble of duplicate causes with counts", {
  out <- suppressMessages(duplicateFinder(inject_email_dupe()))

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("duplicate_field", "n"))
  # Exactly one duplicated hunter, and the differing field is the email.
  expect_identical(out$duplicate_field, "email")
  expect_identical(out$n, 1L)
})

test_that("duplicateFinder emits a message with the duplicate count", {
  expect_message(
    duplicateFinder(inject_email_dupe()),
    "1 registrations with duplicates")
})

test_that("duplicateFinder attributes an unequal bag value to 'bag'", {
  # Two otherwise-identical records that differ ONLY in a bag field should be
  # labelled "bag" (duplicateFields returns "" for bag-only differences, which
  # duplicateFinder rewrites to "bag").
  base <- DF_TEST_MINI |> dplyr::slice_head(n = 3)
  dupe <- base |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(ducks_bag = "2", record_key = "rk_bagdupe")
  out <- suppressMessages(duplicateFinder(dplyr::bind_rows(base, dupe)))
  expect_true("bag" %in% out$duplicate_field)
})

test_that("duplicateFinder finds nothing when there are no duplicates", {
  # All-unique input: message reports 0, and (dupl_tibble has 0 rows) returns
  # NULL. Documented behavior.
  uniq <- DF_TEST_MINI |> dplyr::slice_head(n = 5)
  out <- suppressMessages(duplicateFinder(uniq))
  expect_null(out)
})

# duplicatePlot function --------------------------------------------------

# The following tests for duplicatePlot() were generated with AI assistance
# using Claude Opus 4.8 in Perplexity on July 13, 2026.

# Tests for duplicatePlot(). Thin by nature (a plot builder): assert the object
# type and that the plotted data/aesthetic is what we expect.

test_that("duplicatePlot returns a ggplot on valid input", {
  p <- suppressMessages(duplicatePlot(inject_email_dupe()))
  expect_s3_class(p, "ggplot")
})

test_that("duplicatePlot maps total_count to y and duplicate_field to x", {
  p <- suppressMessages(duplicatePlot(inject_email_dupe()))

  # The plotted data is the reframe()'d cause table with a total_count column.
  expect_true("total_count" %in% names(p$data))
  expect_true("duplicate_field" %in% names(p$data))
  # Our single injected email duplicate must appear with a count of 1.
  expect_identical(
    p$data$total_count[p$data$duplicate_field == "email"], 1L)

  # The y aesthetic references total_count (label may be ".data$total_count").
  expect_match(rlang::as_label(p$mapping$y), "total_count")
})

