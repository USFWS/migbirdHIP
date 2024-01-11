#' Check for frame shifts
#'
#' Find and print any rows that have a line shift error with number of positions shifted.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_length
#' @importFrom stringr str_extract
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyr unite
#' @importFrom stringr str_remove_all
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
      # Summarize the line shifts to help with manual fixing
      shift_summary <-
        data |>
        filter(record_key %in% shifted_data$record_key) |>
        unite(shifted, title:zip, sep =  "", remove = F) |>
        mutate(
          culprit = str_remove_all(shifted, "[A-Z]|[0-9]|\\'|\\/|\\-|\\s")) |>
        select(-shifted) |>
        relocate(
          all_of(c("source_file", "record_key", "culprit")), .before = "title")

      return(shift_summary)
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
