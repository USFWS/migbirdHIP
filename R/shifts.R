#' Check for frame shifts
#'
#' Find and print any rows that have a line shift error with number of positions shifted.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_length
#' @importFrom stringr str_extract
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#'
#' @param data A raw data table created by \code{\link{read_hip}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

shiftCheck <-
  function(data){

    shifted_data <-
      shiftFinder(data) |>
      mutate(
        n_shift =
          str_length(
            str_extract(issue_date, "^[0-9]+(?=\\/)")) - 2) |>
      relocate(record_key, .before = "title") |>
      relocate(source_file, .before = "record_key") |>
      select(record_key, n_shift)

    if(nrow(shifted_data) > 0){
      return(shifted_data)
    } else {
      message("No line shifts detected.")
    }
  }

#' Find frame shifts
#'
#' The internal \code{shiftFinder} function finds any rows that have a line shift error.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @param data A raw data table created by \code{\link{read_hip}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

shiftFinder <-
  function(data){
    filter(data, !str_detect(birth_date, "^[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}$"))
  }
