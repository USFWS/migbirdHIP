#' Write an R markdown report with shiny plots from template
#'
#' Create documentation with shiny figures that summarizes HIP download data at a download cycle or season total scale.
#'
#' @importFrom rmarkdown run
#'
#' @param path File path to the folder containing HIP .txt files
#' @param type Type of report. One of the following options may be supplied:
#' \itemize{
#' \item dl_shiny - for a download cycle summary report
#' \item season_shiny - for a summary of the entire HIP season}
#' @param yr The year in which the Harvest Information Program data were collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

writeShiny <-
  function(path, type, yr){

    # Create Rmd
    rmarkdown::run(
      file =
        # Use the specified template
        system.file(
          "templates",
          paste0(type, ".Rmd"),
          package = "migbirdHarvestData"),
      # Include the specified parameters so the functions can run
      render_args = list(params = list(comp_path = path, year = yr))
    )

  }
