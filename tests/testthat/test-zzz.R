test_that("startup message is correct", {
  local_version <- packageVersion("migbirdHIP")

  suppressMessages(
    expect_message(
      migbirdHIP:::.onAttach(),
      paste0(
        "migbirdHIP v", local_version, "\nCompatible with ",
        names(REF_RELEASES[REF_RELEASES == local_version]), " HIP data"
      )
    )
  )
})

test_that("startup message returns error for bad package version", {
  suppressMessages(
    expect_message(
      migbirdHIP:::.onAttach(test = "1.3.9"),
      "Local package version does not exist in REF_RELEASES."
    )
  )
})

test_that("startup message returns error for bad package version", {
  suppressMessages(
    expect_message(
      migbirdHIP:::.onAttach(test = "5.0.0"),
      "Local package version does not exist in REF_RELEASES."
    )
  )
})

test_that("startup message returns message for dev package version", {
  local_version <- "1.4.12"

  suppressMessages(
    expect_message(
      migbirdHIP:::.onAttach(test = local_version),
      paste0(
        "migbirdHIP",
        " v",
        local_version,
        " is an incomplete package version.",
        "\nThis version is NOT fully finalized for any season of HIP data!"
      )
    )
  )
})

test_that("startup message returns message for past season package version", {
  local_version <- "1.3.0"
  season <- names(REF_RELEASES[REF_RELEASES == local_version])
  remote_version <- packageVersion("migbirdHIP")

  suppressMessages(
    expect_message(
      migbirdHIP:::.onAttach(test = local_version),
      packageStartupMessage(
        "A newer version of migbirdHIP (v",
        remote_version,
        ") is available.\nYou are currently using v",
        local_version,
        ", which was released for the ",
        season,
        " season.\nUpdate with pak::pak('USFWS/migbirdHIP')"
      )
    )
  )
})
