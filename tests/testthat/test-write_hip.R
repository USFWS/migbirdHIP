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

  csvs2 <- basename(list.files(out_dir, pattern = "\\.csv$"))
  ns <- str_replace(sort(unique(out_data$source_file)), "txt", "csv")
  expect_setequal(csvs2, ns)

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

  write_hip(test_df, path = out_dir, type = "HIP", split = TRUE)
  dt <- data.table::fread(list.files(out_dir, pattern = "\\.csv$",
                                     full.names = TRUE)[1])

  # Positive assertion: the folder prefix is removed and the bare file name
  # remains -- not merely "no slash present".
  expect_identical(unique(dt$source_file), "AZ20260906.txt")
  # And the output file itself is named from the stripped source_file.
  expect_true(file.exists(file.path(out_dir, "AZ20260906.csv")))
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

test_that("write_hip HIP>PMT guard passes when HIP records outnumber PMT", {
  # ACTUAL PMT records so hip_v_pmt has both rows and the guard is exercised
  # HIP(n=4) > PMT(n=2)
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  mixed <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 6) |>
    mutate(errors = NA,
           record_type = c(rep("HIP", 4), rep("PMT", 2)))

  expect_no_error(
    suppressMessages(write_hip(mixed, path = out_dir, type = "HIP")))
})

test_that("write_hip HIP>PMT guard ERRORS when PMT records outnumber HIP", {
  # The failing side of the same guard: HIP(n=2) < PMT(n=4) must stop().
  out_dir <- make_tmpdir()
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  mixed <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 6) |>
    mutate(errors = NA,
           record_type = c(rep("HIP", 2), rep("PMT", 4)))

  expect_error(
    suppressMessages(write_hip(mixed, path = out_dir, type = "HIP")),
    "More HIP records than PMT records expected")
})

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

test_that("write_hip path must be a directory", {
  # write_hip always treats `path` as a DIRECTORY. It appends a trailing "/",
  # then split = FALSE writes to paste0(path, "/", dl, ".csv"). Passing a file
  # name like "out.csv" yields "out.csv//<dl>.csv" and fwrite errors because
  # that directory does not exist.
  d <- mk_dir2()
  fp <- file.path(d, "out.csv")
  input <- DF_TEST_TINI_CORRECTED
  expect_error(
    write_hip(input, path = fp, type = "HIP", split = FALSE),
    "No such file or directory|does not exist|cannot open")
})

# types -------------------------------------------------------------------

# Tests for write_hip() type = "BT" and type = "CR", the REF_BAGS value
# translation, and the split = TRUE / split = FALSE writers (R/write_hip.R:40).
#
# Covered exported function: write_hip
# Internal guards exercised: failBTPI, failCR (via migbirdHIP:::) for the
# negative cases -- internal-coupling tradeoff noted; the payoff is asserting
# the exact stop() messages the guards raise.

mk_dir2 <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  d
}

# REF_BAGS translation VALUE assertion (the crux; not just column names)

test_that("write_hip translates bag values to FWS strata (AR brant 1 -> 0)", {
  # REF_BAGS maps state=AR, spp=brant, stateBagValue=1 -> FWSstratum=0, a
  # NON-identity translation. Assert the written stratum column value, proving
  # the left_join produced the correct translated value, not merely a column.
  d <- mk_dir2()
  input <- DF_TEST_TINI_CORRECTED |> dplyr::mutate(dl_state = "AR", brant = "1")

  write_hip(input, path = d, type = "HIP", split = TRUE)
  dt <- data.table::fread(
    list.files(d, pattern = "\\.csv$", full.names = TRUE)[1],
    colClasses = "character")

  expect_true("S_brant" %in% names(dt))
  expect_identical(unique(dt$S_brant), "0")   # translated stratum
  expect_identical(unique(dt$Q_brant), "1")   # original bag value, renamed
})

# type = "BT"

# A valid band-tailed-pigeon permit file: record_type HIP, band_tailed_pigeon
# all "2", dove_bag non-zero, every other bag field "0", from a BTPI permit
# state (UT is a BTPI permit-file state, not a crane state).
bt_valid <- function() {
  DF_TEST_TINI_CORRECTED |>
    dplyr::mutate(
      dl_state = "UT", record_type = "HIP",
      band_tailed_pigeon = "2", dove_bag = "1",
      ducks_bag = "0", geese_bag = "0", woodcock_bag = "0", coots_snipe = "0",
      rails_gallinules = "0", cranes = "0", brant = "0", seaducks = "0")
}

test_that("write_hip type = 'BT' writes a valid BTPI permit file", {
  d <- mk_dir2()
  expect_no_error(
    suppressWarnings(suppressMessages(
      write_hip(bt_valid(), path = d, type = "BT", split = TRUE))))
  csvs <- list.files(d, pattern = "\\.csv$", full.names = TRUE)
  expect_length(csvs, 1)
  dt <- data.table::fread(csvs[1], colClasses = "character")
  expect_identical(unique(dt$Q_bt_pigeons), "2")
})

test_that("write_hip type = 'BT' rejects a file whose band_tailed_pigeon != 2", {
  d <- mk_dir2()
  bad <- bt_valid() |> dplyr::mutate(band_tailed_pigeon = "1")
  expect_error(
    suppressWarnings(suppressMessages(
      write_hip(bad, path = d, type = "BT", split = TRUE))),
    "band_tailed_pigeon = 2")
})

test_that("write_hip type = 'BT' rejects a non-HIP record_type", {
  d <- mk_dir2()
  bad <- bt_valid() |> dplyr::mutate(record_type = "PMT")
  expect_error(
    suppressWarnings(suppressMessages(
      write_hip(bad, path = d, type = "BT", split = TRUE))),
    "record_type = HIP")
})

# type = "CR"

# A valid crane permit file: record_type PMT, cranes all "2", every other bag
# field "0", from a crane permit-file state (MT).
cr_valid <- function() {
  DF_TEST_TINI_CORRECTED |>
    dplyr::mutate(
      dl_state = "MT", record_type = "PMT", cranes = "2",
      ducks_bag = "0", geese_bag = "0", dove_bag = "0", woodcock_bag = "0",
      coots_snipe = "0", rails_gallinules = "0", band_tailed_pigeon = "0",
      brant = "0", seaducks = "0")
}

test_that("write_hip type = 'CR' writes a valid crane permit file and strata", {
  d <- mk_dir2()
  expect_no_error(
    suppressWarnings(suppressMessages(
      write_hip(cr_valid(), path = d, type = "CR", split = TRUE))))
  dt <- data.table::fread(
    list.files(d, pattern = "\\.csv$", full.names = TRUE)[1],
    colClasses = "character")
  expect_identical(unique(dt$Q_cranes), "2")
  # MT crane stateBagValue 2 -> FWSstratum 2 (translated value present).
  expect_identical(unique(dt$S_cranes), "2")
})

test_that("write_hip type = 'CR' rejects a file whose cranes != 2", {
  d <- mk_dir2()
  bad <- cr_valid() |> dplyr::mutate(cranes = "1")
  expect_error(
    suppressWarnings(suppressMessages(
      write_hip(bad, path = d, type = "CR", split = TRUE))),
    "cranes = 2")
})

test_that("write_hip type = 'CR' rejects a non-PMT record_type", {
  d <- mk_dir2()
  bad <- cr_valid() |> dplyr::mutate(record_type = "HIP")
  expect_error(
    suppressWarnings(suppressMessages(
      write_hip(bad, path = d, type = "CR", split = TRUE))),
    "record_type = PMT")
})
