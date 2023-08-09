#' Write an R markdown report from template
#'
#' Create documentation with figures and tables that summarizes HIP data at a download cycle scale.
#'
#' @importFrom quarto quarto_render
#' @importFrom stringr str_detect
#'
#' @param raw_path File path to the folder containing HIP .txt files
#' @param temp_path Path to directory that contains output .csv files
#' @param future_path Path to directory that contains future data .csv files
#' @param past_path Path to directory that contains past data .csv files
#' @param year The year in which the Harvest Information Program data were collected
#' @param dl Download cycle
#' @param dir Folder in which to save the rendered report
#' @param file What the report file should be named (do not include an extension or suffix indicating file type, this is done automatically)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

writeReport <-
  function(raw_path, temp_path, future_path, past_path, year, dl, dir, file){

    # Add a final "/" to path if not included already
    if(!str_detect(raw_path, "\\/$")) {
      raw_path <- paste0(raw_path, "/")
    }
    # Add a final "/" to temp_path if not included already
    if(!str_detect(temp_path, "\\/$")) {
      temp_path <- paste0(temp_path, "/")
    }
    # Add a final "/" to future_path if not included already
    if(!str_detect(future_path, "\\/$")) {
      future_path <- paste0(future_path, "/")
    }
    # Add a final "/" to past_path if not included already
    if(!str_detect(past_path, "\\/$")) {
      past_path <- paste0(past_path, "/")
    }
    # Add a final "/" to dir if not included already
    if(!str_detect(dir, "\\/$")) {
      dir <- paste0(dir, "/")
    }

    # Copy .qmd template from R pacakge directory to output directory; a
    # workaround required due to quarto's inability to render a document
    # anywhere except the directory where the .qmd file is located (and it can't
    # be rendered in the package directory!)
    invisible(
      file.copy(
        from =
          system.file(
            "templates",
            "dl_report.qmd",
            package = "migbirdHIP"),
        to = dir,
        overwrite = TRUE)
    )

    # Render report
    quarto_render(
      input = paste0(dir, "dl_report.qmd"),
      # Include the specified parameters so the functions can run
      execute_params =
        list(
          raw_path = raw_path,
          temp_path = temp_path,
          future_path = future_path,
          past_path = past_path,
          dl = dl,
          year = year),
      output_format = "html",
      # Don't show lengthy knitting status text in console
      quiet = F)

    # Delete the template from the directory
    invisible(file.remove(paste0(dir, "dl_report.qmd")))

    # Rename the output
    invisible(
      file.rename(
        from = paste0(dir, "dl_report.html"),
        to = paste0(dir, file, ".html"))
    )

  }
