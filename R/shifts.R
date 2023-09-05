#' Correct frame shifts
#'
#' Shift record values to the left by a designated number of characters.
#'
#' @importFrom rlang :=
#' @importFrom dplyr slice
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr lead
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom dplyr bind_cols
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr everything
#'
#' @param data A cleaned data table created by \code{\link{clean}}
#' @param record_id The ID of the line to be adjusted
#' @param first_col First (leftmost) column affected by frame shift
#' @param last_col Last (rightmost) column affected by frame shift
#' @param n_shift Number of characters that the line needs to be adjusted by
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

shiftFix <-
  function(data, record_id, first_col, last_col, n_shift){

    # List of columns to be corrected
    col_list <-
      data |>
      slice(1) |>
      select(!!sym(first_col):!!sym(last_col)) |>
      names()

    # Shift correction for first and last columns
    firstlastfix <-
      data |>
      filter(record_key == record_id) |>
      select(!!sym(first_col):!!sym(last_col))

    # Can't pipe a ., use "firstlastfix" table name instead
    firstlastfix <-
      firstlastfix |>
      # Fix the first column in the line shift
      mutate(
        !!sym(first_col) :=
          paste0(
            firstlastfix[[1]],
            str_extract(firstlastfix[[2]], paste0("^.{", n_shift, "}")))) |>
      # Fix the last column in the line shift
      mutate(
        !!sym(last_col) :=
          str_remove(
            firstlastfix[[length(col_list)]], paste0("^.{", n_shift, "}"))) |>
      select(!!sym(first_col), !!sym(last_col))

    # Shift correction for all middle columns
    unshifted <-
      # Bind the cleaned up record back together
      bind_cols(
        # All cols left of the first shifted column
        data |>
          filter(record_key == record_id) |>
          select(title:!!sym(first_col)) |>
          select(-!!sym(first_col)),
        # Middle cols
        data |>
          filter(record_key == record_id) |>
          select(!!sym(first_col):!!sym(last_col)) |>
          # Fix the middle columns
          # This must be done by pivoting, since there's no rowwise lag, lead,
          # or column indexing in tidyverse
          pivot_longer(cols = everything()) |>
          mutate(
            value2 =
              paste0(
                str_remove(value, paste0("^.{", n_shift, "}")),
                str_extract(lead(value), paste0("^.{", n_shift, "}")))) |>
          select(-value) |>
          pivot_wider(names_from = name, values_from = value2)  |>
          # Replace the first and last col values with the correct values
          mutate(
            !!sym(first_col) := firstlastfix[[1]],
            !!sym(last_col) := firstlastfix[[2]]),
        # All cols right of the last shifted column
        data |>
          filter(record_key == record_id) |>
          select(!!sym(last_col):record_key) |>
          select(-!!sym(last_col))
      )

    return(unshifted)

  }

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
