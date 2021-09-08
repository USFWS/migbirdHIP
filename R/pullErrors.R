#' Pull flagged errors
#'
#' Pull and view errors that have been flagged for a particular field. This function allows you to easily see what the \code{\link{proof}} function has determined to be unacceptable data.
#'
#' @importFrom magrittr %<>%
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom stringr str_detect
#' @importFrom purrr is_empty
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param error Field that should be pulled. One of the fields from the following list may be supplied:
#' \itemize{
#' \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe_bag, rails_gallinules_bag, cranes, band_tailed_pigeon, brant, seaducks, registration_year, email}
#' @param distinct If FALSE, returns all error vvalues ; if TRUE (default), only returns unique values.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

pullErrors <-
  function(x, error, distinct = TRUE){

    pulled_error <-
      x %>%
      select(error, errors) %>%
      filter(str_detect(errors, error)) %>%
      select(error)

    if(distinct == TRUE){
      pulled_error %<>%
        distinct() %>%
        pull()
      }
    else{
      pulled_error %<>%
        pull()
      }
    if(is_empty(pulled_error)){
      message("Success! All values are correct.")
      }
    else{
      return(pulled_error)
      }
  }
