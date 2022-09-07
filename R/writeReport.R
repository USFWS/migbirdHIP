#' Write an R markdown report from template
#'
#' Create documentation with figures and tables that summarizes HIP download data at a download cycle or season total scale.
#'
#' @importFrom rmarkdown render
#'
#' @param path File path to the folder containing HIP .txt files
#' @param type Type of report. One of the following options may be supplied:
#' \itemize{
#' \item dl_report - for a download cycle summary report
#' \item season_report - for a summary of the entire HIP season}
#' @param yr The year in which the Harvest Information Program data were collected
#' @param dl Download cycle (when running a download report)
#' @param corrected_path Path to directory that contains output .csv files
#' @param future_path Path to directory that contains future data .csv files
#' @param past_path Path to directory that contains past data .csv files
#' @param dir Folder in which to save the completed report
#' @param file What the report file should be named
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

writeReport <-
  function(path, type, yr, dl = NA, corrected_path, future_path = NA, past_path = NA, dir, file){

    # Create Rmd for download
    if(type == "dl_report"){
      render(
        input =
          # Use the specified template
          system.file(
            "templates",
            paste0(type, ".Rmd"),
            package = "migbirdHIP"),
        # Include the specified parameters so the functions can run
        params =
          list(
            comp_path = path,
            final_path = corrected_path,
            future_path = future_path,
            past_path = past_path,
            dl = dl,
            year = yr),
        output_file = file,
        output_dir = dir,
        # Don't show lengthy knitting status text in console
        quiet = TRUE)
    }
    else if(type == "season_report"){
      render(
        input =
          # Use the specified template
          system.file(
            "templates",
            paste0(type, ".Rmd"),
            package = "migbirdHIP"),
        # Include the specified parameters so the functions can run
        params =
          list(
            comp_path = path,
            proc_path = corrected_path,
            year = yr),
        output_file = file,
        output_dir = dir,
        # Don't show lengthy knitting status text in console
        quiet = TRUE)
    }
    else{
      message("Indicated type must be 'dl_report' or 'season_report'.")
    }

  }
