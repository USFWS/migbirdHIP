#' Validate data
#'
#' After tidying the data with \code{\link{tidy}}, check to make sure the data don't have any erroneously repeated values.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#'
#' @param x The object created after tidying data with \code{\link{tidy}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

validate <-
  function(x){

    validated_x <-
      x %>%
      select(dl_state, dl_date, contains("bag")) %>%
      group_by(dl_state, dl_date) %>%
      suppressMessages(summarize(across(contains("bag"), ~n_distinct(.)))) %>%
      mutate(
        # Create uniformity so the next step has something to paste with
        uniformity = NA,
        # Paste col name into "uniformity" if it has only one value per dl_state
        # and dl_date (the grouping values)
        uniformity =
          case_when(
            as.numeric(length(unique(ducks_bag))) == 1 ~
              paste0(uniformity, "-ducks_bag"),
            as.numeric(length(unique(geese_bag))) == 1 ~
              paste0(uniformity, "-geese_bag"),
            as.numeric(length(unique(dove_bag))) == 1 ~
              paste0(uniformity, "-dove_bag"),
            as.numeric(length(unique(woodcock_bag))) == 1 ~
              paste0(uniformity, "-woodcock_bag"),
            as.numeric(length(unique(coots_snipe_bag))) == 1 ~
              paste0(uniformity, "-coots_snipe_bag"),
            as.numeric(length(unique(rails_gallinules_bag))) == 1 ~
              paste0(uniformity, "-rails_gallinules_bag"),
            as.numeric(length(unique(cranes_bag))) == 1 ~
              paste0(uniformity, "-cranes_bag"),
            as.numeric(length(unique(bt_pigeon_bag))) == 1 ~
              paste0(uniformity, "-bt_pigeon_bag"),
            as.numeric(length(unique(brant_bag))) == 1 ~
              paste0(uniformity, "-brant_bag"),
            as.numeric(length(unique(seaducks_bag))) == 1 ~
              paste0(uniformity, "-seaducks_bag"),
            TRUE ~ NA_character_
          ),
        # Remove unnecessary leading NA- strings from the first step
        uniformity = str_remove(uniformity, "NA\\-")
      ) %>%
      filter(!is.na(uniformity)) %>%
      select(dl_state, dl_date, uniformity) %>%
      distinct()

    if (nrow(validated_x) != 0) {

      return(validated_x)
      message("Warning: Uniform value detected across one or more fields, please review.")

    }
    else{
      message("Data are good to go!")

    }
  }
