#' Write an R markdown report from template
#'
#' Create documentation with figures and tables that summarizes HIP download data at a download cycle or season total scale.
#'
#' @import rmarkdown
#' @import stringr
#'
#' @param path File path to the folder containing HIP .txt files
#' @param type Type of report. One of the following options may be supplied:
#' \itemize{
#' \item dl_report - for a download cycle summary report
#' \item season_report - for a summary of the entire HIP season}
#' @param yr The year in which the Harvest Information Program data were collected
#' @param dir Folder in which to save the completed report
#' @param file What the report file should be named
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

writeReport <-
  function(path, type, yr, dir, file){

    render(
      input =
        system.file(
          "templates",
          paste0(type, ".Rmd"),
          package = "migbirdHarvestData"),
      params =
        list(
          comp_path = path,
          year = yr),
      output_file = file,
      output_dir = dir,
      quiet = TRUE)

  }
