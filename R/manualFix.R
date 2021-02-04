#' Manually correct data
#'
#' Replace incorrect cell contents with a desired value.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#'
#' @param x A corrected data table created by \code{\link{correct}}
#' @param field Field that contains value(s) to be corrected
#' \itemize{
#' \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe_bag, rails_gallinules_bag, cranes, band_tailed_pigeon, brant, seaducks, registration_year, email}
#' @param error_value Verbatim cell contents of value to be corrected
#' @param correct_value Desired value to replace error_value
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

manualFix <-
  function(x, field, error_value, correct_value){

    new_x <-
      x %>%
        mutate(
          !!sym(field) :=
            ifelse(
              !!sym(field) == error_value,
              correct_value,
              !!sym(field)
              )
          )

    return(new_x)
  }
