#' @importFrom utils packageVersion

.onAttach <- function(libname, pkgname = "migbirdHIP") {
  version <- packageVersion(pkgname)
  season <- names(REF_RELEASES[REF_RELEASES == version])

  if (!version %in% REF_RELEASES) {
    packageStartupMessage("ERROR: Update variables.R with new package version.")
  } else if (season == "dev" & version %in% REF_RELEASES) {
    packageStartupMessage(
      "migbirdHIP",
      " v",
      version,
      "\nIncomplete package version;",
      " NOT fully finalized for any season of HIP data"
    )
   } else {
     packageStartupMessage("migbirdHIP",
                           " v",
                           version,
                           "\nCompatible with ",
                           season,
                           " HIP data")
  }
}
