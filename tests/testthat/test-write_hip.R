# This test file was generated with AI assistance using Claude Opus 4.8 in
# Perplexity on July 10, 2026.

# write_hip ---------------------------------------------------------------

# Covered exported function: write_hip

make_tmpdir <- function() {
  d <- tempfile("whip_")
  dir.create(d)
  d
}

out_data <- DF_TEST_MINI |> mutate(errors = NA, record_type = "HIP")

# split = TRUE ------------------------------------------------------------

test_that("write_hip split = TRUE writes one csv per source_file", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  write_hip(out_data, path = out_dir, type = "HIP", split = TRUE)

  csvs <- list.files(out_dir, pattern = "\\.csv$", full.names = TRUE)
  n_files <- length(unique(DF_TEST_MINI$source_file))
  expect_equal(length(csvs), n_files)

  dt <- data.table::fread(csvs[1])
  expect_gt(nrow(dt), 0)
})

test_that("write_hip renames columns to the database output format", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  write_hip(DF_TEST_TINI_CORRECTED, path = out_dir, type = "HIP", split = TRUE)
  dt <- data.table::fread(list.files(out_dir, pattern = "\\.csv$",
                                     full.names = TRUE)[1])

  expect_true(
    all(c("dl", "postal_code", "Q_ducks", "Q_geese", "Q_doves", "Q_woodcock",
          "Q_coot_snipe", "Q_rail_gallinule", "Q_cranes", "Q_bt_pigeons",
          "Q_brant", "Q_seaducks", "source_file") %in% names(dt)))

  # Dropped columns must not appear in the output.
  expect_false(any(c("dl_date", "dl_key", "record_key", "errors") %in% names(dt)))
})

test_that("write_hip writes NA values as empty strings", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  write_hip(DF_TEST_TINI_CORRECTED, path = out_dir, type = "HIP", split = TRUE)
  lines <- readLines(list.files(out_dir, pattern = "\\.csv$",
                                full.names = TRUE)[1])
  # fwrite(na = "") means no field should be the literal token "NA".
  expect_false(any(grepl("(^|,)NA(,|$)", lines)))
})

test_that("write_hip source_file field is stripped of folder names", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  test_df <-
    DF_TEST_TINI_CORRECTED |>
    mutate(source_file = "permit/AZ20260906.txt")

  write_hip(DF_TEST_TINI_CORRECTED, path = out_dir, type = "HIP", split = TRUE)
  dt <- data.table::fread(list.files(out_dir, pattern = "\\.csv$",
                                     full.names = TRUE)[1])
  expect_false(any(grepl("/", dt$source_file)))
})

test_that("write_hip adds a trailing slash to a directory path", {
  # Path given without a trailing slash should still resolve to files inside it.
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  expect_no_error(
    write_hip(DF_TEST_TINI_CORRECTED, path = out_dir, type = "HIP", split = TRUE)
  )
  expect_gt(length(list.files(out_dir, pattern = "\\.csv$")), 0)
})

# split = FALSE -----------------------------------------------------------

test_that("write_hip split = FALSE writes a single csv to the path", {
  # Intended behavior with split = FALSE: the final table is saved as a single
  # .csv to `path`.

  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  dl <- unique(out_data$dl_cycle)
  out_file <- paste0(out_dir, "/", dl, ".csv")

  write_hip(out_data, path = out_dir, type = "HIP", split = FALSE)

  expect_true(file.exists(out_file))
  dt <- data.table::fread(out_file)
  expect_equal(nrow(dt), nrow(out_data))
  expect_true(all(c("dl", "postal_code", "Q_ducks") %in% names(dt)))
})

# validation / guards -----------------------------------------------------

test_that("write_hip rejects an invalid type", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  expect_error(
    write_hip(DF_TEST_TINI_CORRECTED, path = out_dir, type = "XYZ"),
    "HIP.*BT.*CR"
  )
})

test_that("write_hip rejects a non-logical split", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  expect_error(
    write_hip(DF_TEST_TINI_CORRECTED, path = out_dir, type = "HIP",
              split = "maybe")
  )
})

test_that("write_hip fails on non-proofed data", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  expect_error(write_hip(DF_TEST_MINI, path = out_dir, type = "HIP"))
})

test_that("write_hip fails when dl_state contains NA", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  bad <- dplyr::mutate(DF_TEST_TINI_CORRECTED, dl_state = NA_character_)
  expect_error(
    write_hip(bad, path = out_dir, type = "HIP"),
    "dl_state"
  )
})

test_that("write_hip fails when dl_date contains NA", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
  bad <- dplyr::mutate(DF_TEST_TINI_CORRECTED, dl_date = NA_character_)
  expect_error(
    write_hip(bad, path = out_dir, type = "HIP"),
    "dl_date"
  )
})

test_that("write_hip fails when a value exceeds the fixed-width limit", {
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  # `title` has a fixed width of 1; a 2-character value must trip failWidths.
  bad <- dplyr::mutate(DF_TEST_TINI_CORRECTED, title = "99")
  expect_error(
    write_hip(bad, path = out_dir, type = "HIP"),
    "fixed-width"
  )
})

