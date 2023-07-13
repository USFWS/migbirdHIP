#' Write an R markdown report from template
#'
#' Create documentation with figures and tables that summarizes HIP download data at a download cycle scale.
#'
#' @importFrom quarto quarto_render
#' @importFrom rmarkdown render
#'
#' @param path File path to the folder containing HIP .txt files
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
  function(path, yr, dl = NA, corrected_path, future_path = NA, past_path = NA, dir, file){

    #quarto_render(
    render(
      input =
        # Use the specified template
        system.file(
          "templates",
          paste0(type, ".Rmd"),
          package = "migbirdHIP"),
      # Include the specified parameters so the functions can run
      #execute_params =
      params =
        list(
          comp_path = path,
          final_path = corrected_path,
          future_path = future_path,
          past_path = past_path,
          dl = dl,
          year = yr),
      output_dir = dir,
      output_file = file,
      # Don't show lengthy knitting status text in console
      quiet = TRUE)
    }
