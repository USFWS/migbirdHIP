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
      select(
        dl_state,
        dl_date,
        matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) %>%
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
            as.numeric(length(unique(coots_snipe))) == 1 ~
              paste0(uniformity, "-coots_snipe"),
            as.numeric(length(unique(rails_gallinules))) == 1 ~
              paste0(uniformity, "-rails_gallinules"),
            as.numeric(length(unique(cranes))) == 1 ~
              paste0(uniformity, "-cranes"),
            as.numeric(length(unique(band_tailed_pigeon))) == 1 ~
              paste0(uniformity, "-band_tailed_pigeon"),
            as.numeric(length(unique(brant))) == 1 ~
              paste0(uniformity, "-brant"),
            as.numeric(length(unique(seaducks))) == 1 ~
              paste0(uniformity, "-seaducks"),
            TRUE ~ NA_character_
          ),
        # Remove unnecessary leading NA- strings from the first step
        uniformity = str_remove(uniformity, "NA\\-"),
        # How many values are uniform within the group?
        n_uniform = n()
      ) %>%
      filter(!is.na(uniformity)) %>%
      select(dl_state, dl_date, uniformity) %>%
      distinct()

    if (nrow(validated_x) != 0) {

      return(validated_x)
      message(
        paste0(
          "Warning: Uniform value detected across one or more fields, ",
          "please review.")
        )

    }
    else{
      message("Data are good to go!")

    }
  }
