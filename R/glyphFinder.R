#' Find non-UTF-8 glyphs/character
#'
#' Pull and view non-UTF-8 characters in a field when R throws error 'invalid UTF-8 byte sequence detected'.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom rlang sym
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#'
#' @param data HIP data table at any point during pre-processing
#' @param field Field that should be checked for non-UTF-8 characters. One of the fields from the following list may be supplied:
#' \itemize{
#' \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks, registration_year, email}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

glyphFinder <-
  function(data, field){
    data %>%
      select(source_file, record_key, !!sym(field)) %>%
      mutate(check = is.na(iconv(!!sym(field), "UTF-8", "UTF-8"))) %>%
      filter(check == TRUE) %>%
      arrange(!!sym(field)) %>%
      select(-check)
  }
