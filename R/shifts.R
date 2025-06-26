#' Check for frame shifts
#'
#' Find and print any rows that have a line shift error with number of positions
#' shifted.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_length
#' @importFrom stringr str_extract
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom tidyr unite
#' @importFrom stringr str_remove_all
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

shiftCheck <-
  function(raw_data){

    shifted_data <-
      shiftFinder(raw_data) |>
      mutate(
        n_shift =
          str_length(
            str_extract(.data$issue_date, "^[0-9]+(?=\\/)")) - 2) |>
      relocate("record_key", .before = "title") |>
      relocate("source_file", .before = "record_key") |>
      select(c("record_key", "n_shift"))

    if(nrow(shifted_data) > 0){
      # Summarize the line shifts to help with manual fixing
      shift_summary <-
        raw_data |>
        filter(.data$record_key %in% shifted_data$record_key) |>
        unite("shifted", "title":"zip", sep =  "", remove = F) |>
        mutate(
          culprit =
            str_remove_all(.data$shifted, "[A-Z]|[0-9]|\\'|\\/|\\-|\\s")) |>
        select(-"shifted") |>
        relocate(
          all_of(c("source_file", "record_key", "culprit")), .before = "title")

      return(shift_summary)
    } else {
      message("No line shifts detected.")
    }
  }

#' Find frame shifts
#'
#' The internal \code{shiftFinder} function finds any rows that have a line
#' shift error.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

shiftFinder <-
  function(raw_data) {
    filter(
      raw_data,
      !str_detect(.data$birth_date, "^[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}$"))
  }
