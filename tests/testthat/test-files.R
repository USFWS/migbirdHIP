# This test file was generated with AI assistance using Claude Opus 4.8 in
# Perplexity on July 10, 2026.

# Covered exported functions in R/files.R: fileCheck, fileRename
#
# Both functions operate on real directories and rename/report files, so tests
# build isolated temp directories, populate them, and assert messages and the
# resulting file names. No bundled fixture is needed.
make_tmpdir <- function() {
  d <- tempfile("files_")
  dir.create(d)
  d
}

# fileCheck ---------------------------------------------------------------

test_that("fileCheck reports all files new when there is no overlap", {
  raw <- make_tmpdir()
  processed <- make_tmpdir()
  on.exit(unlink(c(raw, processed), recursive = TRUE), add = TRUE)

  file.create(file.path(raw, "AA20260101.txt"))

  expect_message(fileCheck(raw, processed), "All files are new")
})

test_that("fileCheck reports overlap when a raw txt matches a processed csv", {
  raw <- make_tmpdir()
  processed <- make_tmpdir()
  on.exit(unlink(c(raw, processed), recursive = TRUE), add = TRUE)

  file.create(file.path(raw, "AA20260101.txt"))
  file.create(file.path(processed, "AA20260101.csv"))

  suppressMessages(
    invisible(
      capture.output(
        expect_message(
          fileCheck(raw, processed),
          "already been written"
        )
      )
    )
  )
})

test_that("fileCheck accepts paths with a trailing slash", {
  raw <- make_tmpdir()
  processed <- make_tmpdir()
  on.exit(unlink(c(raw, processed), recursive = TRUE), add = TRUE)

  file.create(file.path(raw, "AA20260101.txt"))

  expect_message(
    fileCheck(paste0(raw, "/"), paste0(processed, "/")),
    "All files are new"
  )
})

# fileRename --------------------------------------------------------------

test_that("fileRename fails on a bad year", {
  d <- make_tmpdir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_error(fileRename(d, 1999))
})

test_that("fileRename uppercases lowercase state abbreviations", {
  d <- make_tmpdir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.create(file.path(d, "ia20260910.txt"))

  suppressMessages(
    invisible(
      capture.output(
        expect_message(fileRename(d, as.numeric(REF_CURRENT_SEASON)), "Lowercase state abbreviations")
      )
    )
  )

  expect_true("IA20260910.txt" %in% list.files(d))
  expect_false("ia20260910.txt" %in% list.files(d))
})

test_that("fileRename converts 5-digit Julian names to YYYYMMDD", {
  d <- make_tmpdir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.create(file.path(d, "IA123.txt"))

  suppressMessages(
    invisible(
      capture.output(
        expect_message(fileRename(d, as.numeric(REF_CURRENT_SEASON)), "Julian dates changed")
      )
    )
  )

  # The Julian name must be replaced by a 10-character (2 state + 8 date) name.
  expect_equal(length(list.files(d, pattern = "^IA[0-9]{8}\\.txt$")), 1L)
  expect_false("IA123.txt" %in% list.files(d))
})

test_that("fileRename reports unresolved names it cannot fix", {
  d <- make_tmpdir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  file.create(file.path(d, "garbage.txt"))

  suppressMessages(
    invisible(
      capture.output(
        expect_message(fileRename(d, as.numeric(REF_CURRENT_SEASON)), "Unresolved issue")
      )
    )
  )
})

test_that("fileRename warns when run for a non-current year", {
  d <- make_tmpdir()
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  # A correctly-named file so no rename/unresolved message competes.
  file.create(file.path(d, "IA20260910.txt"))

  suppressMessages(
    invisible(
      capture.output(
        expect_message(fileRename(d, 2024), "Are you sure")
      )
    )
  )
})
