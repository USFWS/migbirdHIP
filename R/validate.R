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
        uniformity =
          case_when(
            as.numeric(length(unique(ducks_bag))) == 1 ~ "ducks_bag",
            as.numeric(length(unique(geese_bag))) == 1 ~ "geese_bag",
            as.numeric(length(unique(dove_bag))) == 1 ~ "dove_bag",
            as.numeric(length(unique(woodcock_bag))) == 1 ~ "woodcock_bag",
            as.numeric(length(unique(coots_snipe_bag))) == 1 ~ "coots_snipe_bag",
            as.numeric(length(unique(rails_gallinules_bag))) == 1 ~ "rails_gallinules_bag",
            as.numeric(length(unique(cranes_bag))) == 1 ~ "cranes_bag",
            as.numeric(length(unique(bt_pigeon_bag))) == 1 ~ "bt_pigeon_bag",
            as.numeric(length(unique(brant_bag))) == 1 ~ "brant_bag",
            as.numeric(length(unique(seaducks_bag))) == 1 ~ "seaducks_bag",
            TRUE ~ NA_character_
          )
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
