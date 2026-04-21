#' Question if correct year supplied
#'
#' Internal function that questions if the year supplied is intended to be
#' different than the current season year by returning a message to the console.
#'
#' @param year The year in which the Harvest Information Program data were
#'   collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

questionYear <-
  function(year) {
    if (year != REF_CURRENT_SEASON) {
      message("Are you sure you want to run this using year = ", year, "?")
    }
  }
