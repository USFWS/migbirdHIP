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
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param type Type of tibble to report. Acceptable values include:
#'  \itemize{
#'  \item state
#'  \item field
#'  }
#' @param threshold Value above which errors should be tabulated
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

redFlags <-
  function(x, type, threshold = 0) {
    if (type == "state") {

      # State red flags
      rf <-
        errorLevel_errors_states(x) |>
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
        errorLevel_errors_field(x) |>
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

