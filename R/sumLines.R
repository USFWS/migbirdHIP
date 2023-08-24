#' Sum lines of new data
#'
#' Get the sum of the number of lines in new download files.
#'
#' @importFrom stringr str_detect
#'
#' @param path File path to the download folder containing HIP .txt files
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

sumLines <-
  function(path) {

    # Add a final "/" if not included already
    if(!str_detect(path, "\\/$")){
      path <- paste0(path, "/")
    }

    # Create a vector of the HIP .txt files to be read from the provided
    # directory
    # For reading data from a download cycle for ALL states available
    dl_files <- list.files(path, pattern = "\\.txt|.TXT$")

    sum_lines <- c()

    for (i in seq_along(dl_files)){
      con <- file(paste0(path, dl_files[i]))
      sum_lines[i] <- length(readLines(con))
      close(con)
    }

    return(sum(data.frame(source_file = dl_files, lines = sum_lines)))
  }
