# This test file was generated with AI assistance using Claude Opus 4.8 in
# Perplexity on July 10, 2026.

# errorTables -------------------------------------------------------------

# Covered exported functions: errorTable (and its internal helper
# errorTableSummary, exercised through errorTable) plus pullErrors.

# Proofed data with injected zip code errors
proofed_err <-
  DF_TEST_TINI_DEDUPED |>
  dplyr::mutate(zip = "000000") |>
  proof(year = as.numeric(REF_CURRENT_SEASON))

# errorTable: loc x field grid --------------------------------------------

test_that("errorTable default counts by state and field", {
  tbl <- errorTable(proofed_err)
  expect_named(tbl, c("dl_state", "error", "error_count"))
  expect_true("zip" %in% tbl$error)
  expect_true(all(tbl$dl_state == "IA"))
  expect_equal(tbl$error_count[tbl$error == "zip"], 3L)
})

test_that("errorTable loc = 'all', field = 'none' counts by state only", {
  tbl <- errorTable(proofed_err, loc = "all", field = "none")
  expect_named(tbl, c("dl_state", "error_count"))
  expect_gte(tbl$error_count[tbl$dl_state == "IA"], 3L)
  expect_lte(tbl$error_count[tbl$dl_state == "IA"], 5L)
})

test_that("errorTable loc = 'none', field = 'all' counts by field only", {
  tbl <- errorTable(proofed_err, loc = "none", field = "all")
  expect_named(tbl, c("error", "error_count"))
  expect_equal(tbl$error_count[tbl$error == "zip"], 3L)
})

test_that("errorTable loc = 'all', field = <field> filters to that field", {
  tbl <- errorTable(proofed_err, loc = "all", field = "zip")
  expect_named(tbl, c("error", "error_count"))
  expect_equal(nrow(tbl), 1L)
  expect_equal(tbl$error, "zip")
  expect_equal(tbl$error_count, 3L)
})

test_that("errorTable for a specific state, field = 'all'", {
  tbl <- errorTable(proofed_err, loc = "IA", field = "all")
  expect_named(tbl, c("dl_state", "error", "error_count"))
  expect_true(all(tbl$dl_state == "IA"))
  expect_equal(tbl$error_count[tbl$error == "zip"], 3L)
})

test_that("errorTable for a specific state, field = 'none' totals errors", {
  tbl <- errorTable(proofed_err, loc = "IA", field = "none")
  expect_named(tbl, c("dl_state", "total_errors"))
  expect_gte(tbl$total_errors, 3L)
  expect_lte(tbl$total_errors, 5L)
})

test_that("errorTable for a specific state and field with rows", {
  tbl <- errorTable(proofed_err, loc = "IA", field = "zip")
  expect_named(tbl, c("dl_state", "error", "error_count"))
  expect_equal(tbl$dl_state, "IA")
  expect_equal(tbl$error, "zip")
  expect_equal(tbl$error_count, 3L)
})

test_that("errorTable for a specific state and field with NO rows messages", {
  # No "middle" errors were injected, so the state+field path finds nothing.
  expect_message(
    errorTable(proofed_err, loc = "IA", field = "middle"),
    "No errors in middle for IA"
  )
})

test_that("errorTable loc = 'none' with field != 'all' returns the message", {
  expect_message(
    errorTable(proofed_err, loc = "none", field = "zip"),
    "If .loc = 'none'. then .field. must be 'all'"
  )
})

# errorTable: validation --------------------------------------------------

test_that("errorTable rejects an invalid loc", {
  expect_error(errorTable(proofed_err, loc = "ZZ"))
})

test_that("errorTable rejects an invalid field", {
  expect_error(errorTable(proofed_err, field = "not_a_field"))
})

test_that("errorTable fails on non-proofed data", {
  expect_error(errorTable(DF_TEST_MINI))
})

# pullErrors --------------------------------------------------------------

test_that("pullErrors returns the flagged values for a field", {
  vals <- pullErrors(proofed_err, field = "zip")
  expect_true(is.character(vals) || is.numeric(vals))
  expect_true(all(vals == "000000"))
})

test_that("pullErrors unique = FALSE returns one value per erroneous record", {
  vals <- pullErrors(proofed_err, field = "zip", unique = FALSE)
  expect_equal(length(vals), 3L)
})

test_that("pullErrors messages 'Success' when a field has no errors", {
  # No middle-initial errors were injected.
  expect_message(pullErrors(proofed_err, field = "middle"), "Success")
})

test_that("pullErrors validates its arguments", {
  expect_error(pullErrors(proofed_err, field = "not_a_field"))
  expect_error(pullErrors(proofed_err, field = "zip", unique = "yes"))
  expect_error(pullErrors(DF_TEST_MINI, field = "zip"))
})
