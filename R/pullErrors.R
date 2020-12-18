#' Pull flagged errors
#'
#' Pull and view errors that have been flagged for a particular field. This function allows you to easily see what the \code{\link{proof}} function has determined to be unacceptable data.
#'
#' @import dplyr
#' @import stringr
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param error Field that should be pulled. One of the fields from the following list may be supplied:
#' \itemize{
#' \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe_bag, rails_gallinules_bag, cranes, band_tailed_pigeon, brant, seaducks, registration_year, email}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

pullErrors <-
  # x = proofed data tibble, error = field you want to check
  function(x, error){

    pulled_error <-
      x %>%
      select(error, errors) %>%
      filter(str_detect(errors, error)) %>%
      select(error) %>%
      distinct() %>%
      pull()

    if(is_empty(pulled_error)) message("Success! All values are correct.")

    else(return(pulled_error))

  }
