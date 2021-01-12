#' Investigate data
#'
#' After checking the data with \code{\link{validate}}, look into reported repeated values to determine if they are worrisome.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#'
#' @param x The object created after tidying data with \code{\link{tidy}}
#' @param dl_state The download state in question
#' @param dl_date The download date in question
#' @param species The bird group in question
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

investigate <-
  function(x, dl_state, dl_date, species){

    investigated_x <-
      x %>%
      filter(dl_state == dl_state & dl_date == dl_date) %>%
      select(species) %>%
      distinct()

    return(investigated_x)
  }
