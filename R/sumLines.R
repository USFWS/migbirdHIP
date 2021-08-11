#' Sum lines of new data
#'
#' Get the sum of the number of lines in new download files.
#'
#' @importFrom dplyr as_tibble
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#'
#' @param path File path to the download folder containing HIP .txt files
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

sumLines <-
  function(path) {

    # Add a final "/" if not included already
    if(!str_detect(path, "\\/$")){
      path <- paste0(path, "/")
    }else{
      path <- path
    }

    # Create a tibble of the HIP .txt files to be read from the provided
    # directory
    # For reading data from a download cycle for ALL states available
    dl_files <-
      list.files(path) %>%
      as_tibble() %>%
      mutate(value = str_replace(value, "TXT", "txt")) %>%
      # Keep only txt files
      filter(str_detect(value, "(?<=\\.)txt$")) %>%
      pull()

    sum_lines <- c()

    for (i in seq_along(dl_files)){
      con <- file(paste0(path, dl_files[i]))
      sum_lines[i] <- length(readLines(con))
      close(con)
    }

    return(sum(sum_lines))
  }
