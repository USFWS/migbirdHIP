#' @importFrom utils packageVersion

.onAttach <- function(libname, pkgname = "migbirdHIP") {

  local_version <- packageVersion(pkgname)
  season <- names(REF_RELEASES[REF_RELEASES == local_version])

  try({
    desc_url <-
      paste0(
        "https://raw.githubusercontent.com/USFWS/migbirdHIP/refs/heads/main/",
        "DESCRIPTION")
    remote_desc <- readLines(desc_url, warn = FALSE)
    remote_version <-
      gsub("Version: ", "", remote_desc[grep("^Version: ", remote_desc)])

    # If the local package version is not in the REF_RELEASES internal object,
    # return a message.
    if (!local_version %in% REF_RELEASES) {
      packageStartupMessage(
        "Local package version does not exist in REF_RELEASES.")
      # If the local package version is considered a "development" version,
      # notify the user that the version was not fully finalized.
    } else if (season == "dev" & local_version %in% REF_RELEASES) {
      packageStartupMessage(
        "migbirdHIP",
        " v",
        local_version,
        " is an incomplete package version.",
        "\nThis version is NOT fully finalized for any season of HIP data!"
      )
      # If the local version is older than the most up-to-date release on
      # GitHub, tell the user to update the package.
    } else if (local_version < remote_version) {
      packageStartupMessage(
        "A newer version of migbirdHIP (v",
        remote_version,
        ") is available.",
        "\nYou are currently using v",
        local_version,
        ", which was released for the ",
        season,
        " season.\nUpdate with pak::pak('USFWS/migbirdHIP')"
      )
      # If the local version is equivalent to the most up-to-date release on
      # GitHub, tell the user that it's current.
    } else if (local_version == remote_version &
               season != "dev" &
               local_version %in% REF_RELEASES) {
      packageStartupMessage("migbirdHIP",
                            " v",
                            local_version,
                            "\nCompatible with ",
                            season,
                            " HIP data")
    }
  }, silent = TRUE)
}
