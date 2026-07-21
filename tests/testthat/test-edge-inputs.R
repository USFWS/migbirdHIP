# This test file was generated with AI assistance using Claude Opus 4.8 in
# Perplexity on July 13, 2026.

# Empty (0-row), all-NA-in-key-columns, and single-row input tests for clean(),
# proof(), duplicateFix(), and write_hip().
#
# Where a function handles the edge gracefully, we assert the graceful result.

yr <- as.numeric(migbirdHIP:::REF_CURRENT_SEASON)
test_data <- DF_TEST_MINI[0, ]
test_data2 <- DF_TEST_TINI_DEDUPED[0, ]
test_data3 <- DF_TEST_TINI_CORRECTED[0, ]

# --- 0-row inputs ------------------------------------------------------------

test_that("clean handles a 0-row input gracefully (returns 0 rows)", {
  out <- suppressMessages(clean(test_data))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
})

test_that("duplicateFix handles a 0-row input gracefully (returns 0 rows)", {
  out <- suppressMessages(duplicateFix(test_data))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
})

test_that("proof handles a 0-row input gracefully (returns 0 rows)", {
  out <- suppressMessages(proof(test_data2, yr))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
})

test_that("duplicateFinder handles a 0-row input gracefully (NULL, no error)", {
  expect_null(suppressMessages(duplicateFinder(test_data)))
})

test_that("write_hip() handles a 0-row input gracefully", {
  d <- withr::local_tempdir()
  expect_no_error(
    suppressMessages(
      write_hip(test_data3, path = d, type = "HIP")))
  expect_message(write_hip(test_data3, path = d, type = "HIP"))
})

# --- all-NA in key columns ---------------------------------------------------

test_that("clean() with all-NA PII collapses to 0 rows then fails gracefully", {
  # After missingPIIFilter drops every record, clean() fails gracefully and
  # returns 0 rows.
  na_pii <- DF_TEST_MINI |>
    dplyr::mutate(
      firstname = NA_character_, lastname = NA_character_,
      state = NA_character_, birth_date = NA_character_)
  expect_no_error(suppressMessages(clean(na_pii)))

  out <- suppressMessages(clean(na_pii))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
})

test_that("write_hip() gives a CLEAR error when dl_state is all NA", {
  d <- withr::local_tempdir()
  bad <- DF_TEST_TINI_CORRECTED |> dplyr::mutate(dl_state = NA_character_)
  expect_error(
    write_hip(bad, path = d, type = "HIP"),
    "NA values detected in dl_state")
})

# --- single-row inputs (cheap sanity) ----------------------------------------

test_that("clean() handles a single-row input", {
  suppressMessages(invisible(
    capture.output(out <- clean(DF_TEST_MINI |> dplyr::slice_head(n = 1)))))
  expect_s3_class(out, "data.frame")
  expect_lte(nrow(out), 1)
})

test_that("duplicateFix() handles a single-row input (1 row out)", {
  out <- suppressMessages(
    duplicateFix(DF_TEST_MINI |> dplyr::slice_head(n = 1) |>
                   dplyr::mutate(record_type = "HIP")))
  expect_equal(nrow(out), 1)
})

test_that("proof() handles a single-row input", {
  out <- suppressMessages(
    proof(DF_TEST_TINI_DEDUPED |> dplyr::slice_head(n = 1), yr))
  expect_equal(nrow(out), 1)
})
