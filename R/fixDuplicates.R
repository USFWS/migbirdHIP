#' Fix duplicates
#'
#' Consolidate duplicate records that stem from HIP data and special permit information into one row. If other duplicates exist not due to this specific reason, delete the records from the data.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import ggplot2
#'
#' @param x A proofed data table created by \code{\link{tidy}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

fixDuplicates <-
  function(x){

    duplicates <-
      x %>%
      # Create a row key
      mutate(hunter_key = paste0("hunter_", row_number())) %>%
      # Group by registrant information; name, city, state, birthday, dl_state
      group_by(
        firstname,
        lastname,
        city,
        state,
        birth_date,
        dl_state) %>%
      # Identify duplicates
      mutate(
        duplicate =
          ifelse(
            n() > 1,
            "duplicate",
            "1")) %>%
      ungroup() %>%
      # Filter out non-duplicate records
      filter(duplicate == "duplicate") %>%
      # Sort tibble
      arrange(
        firstname,
        lastname,
        city,
        state,
        birth_date,
        dl_state)

  }
