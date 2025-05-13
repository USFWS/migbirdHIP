.onAttach <- function(libname, pkgname = "migbirdHIP") {
  version <- packageVersion(pkgname)
  season <- names(REF_RELEASES[REF_RELEASES == version])

  packageStartupMessage("migbirdHIP", " v", version,
                        "\nCompatible with ", season, " HIP data")

}
