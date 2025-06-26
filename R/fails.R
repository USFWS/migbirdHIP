#' Fail if incorrect year supplied
#'
#' Internal function that fails if an incorrect year is provided.
#'
#' @importFrom assertthat assert_that
#' @importFrom rlang is_integerish
#'
#' @param year The year in which the Harvest Information Program data were
#'   collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

failyear <-
  function(year) {
    assertthat::assert_that(
      is_integerish(year),
      year >= 2020,
      year <= REF_CURRENT_SEASON,
      msg =
        paste0(
          "`year` must be a whole number between 2020 and ",
          REF_CURRENT_SEASON,
          "."
        )
    )
  }
