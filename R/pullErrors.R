#' Pull flagged errors
#'
#' Pull and view errors that have been flagged for a particular field. This function allows you to easily see what the \code{\link{proof}} function has determined to be unacceptable data.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom stringr str_detect
#' @importFrom rlang sym
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param field Field that should be pulled. One of the fields from the following list may be supplied:
#' \itemize{
#' \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks, registration_year, email}
#' @param distinct If FALSE, returns all error values; if TRUE (default), only returns unique values.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

pullErrors <-
  function(x, field, distinct = TRUE){

    acceptable_fields <-
      names(x)[match("title", names(x)):match("email", names(x))]

    if(!field %in% acceptable_fields) {
      message(
        paste0(
          "Error! Please provide a value for `field` that is one of:\n",
          paste(acceptable_fields, collapse = ", "), ".")
      )
    } else {

      pulled_error <-
        x |>
        select(!!sym(field), errors) |>
        filter(str_detect(errors, field)) |>
        select(!!sym(field))

      if(nrow(pulled_error) == 0) {

        message("Success! All values are correct.")

      } else {

        if(distinct == TRUE) {
          pulled_error <- distinct(pulled_error)
        }

        return(pull(pulled_error))

      }
    }
  }
