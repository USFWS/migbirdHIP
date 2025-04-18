#' Pull bad data
#'
#' Create a tibble of error data by state or field. Data are reported using a threshold of proportion of error.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom stringr str_detect
#'
#' @param proofed_data A proofed data table created by \code{\link{proof}}
#' @param type Type of tibble to report. Acceptable values include:
#'  \itemize{
#'  \item state
#'  \item field
#'  }
#' @param threshold Value above which errors should be tabulated
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

redFlags <-
  function(proofed_data, type, threshold = 0) {

    # Fail if incorrect type supplied
    stopifnot("Error: Please supply 'state' or 'field' for `type` parameter." = type %in% c("state", "field"))

    # Fail if incorrect threshold supplied
    stopifnot("Error: `threshold` parameter must be numeric." = is.numeric(threshold))
    stopifnot("Error: Please supply a value between 0 and 1 for the `threshold` parameter." = (0 <= threshold & threshold <= 1))

    if (type == "state") {

      # State red flags
      rf <-
        errorLevel_errors_state(proofed_data) |>
        mutate(
          flag =
            ifelse(
              proportion > threshold,
              paste0("error > ", threshold),
              NA)) |>
        # Filter out errors that didn't exceed the threshold
        filter(!is.na(flag)) |>
        arrange(desc(proportion))

      if (nrow(rf) > 0) {
        return(rf)
      } else {
        message("No states with error exceeding the threshold.")
      }
    } else if (type == "field") {

      # Field red flags
      rf <-
        errorLevel_errors_field(proofed_data) |>
        mutate(
          count_correct = total - count_errors,
          flag =
            ifelse(
              proportion > threshold,
              paste0("error > ", threshold),
              NA)) |>
        select(-total) |>
        relocate(count_correct, .before = proportion) |>
        # Filter out errors that didn't exceed the threshold
        filter(!is.na(flag)) |>
        arrange(desc(proportion))

      if (nrow(rf) > 0) {
        return(rf)
      } else {
        message("No fields with error exceeding the threshold.")
      }
    } else {
      message("Error: Invalid type provided.")
    }
  }

