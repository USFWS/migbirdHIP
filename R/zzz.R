#' @importFrom utils packageVersion

.onAttach <- function(libname, pkgname = "migbirdHIP") {
  version <- packageVersion(pkgname)
  season <- names(REF_RELEASES[REF_RELEASES == version])

  if (season == "dev") {
    packageStartupMessage("migbirdHIP", " v", version,
                          "\nIncomplete package version;",
                          " NOT fully finalized for any season of HIP data")
  } else {
    packageStartupMessage("migbirdHIP", " v", version,
                          "\nCompatible with ", season, " HIP data")
  }
}
