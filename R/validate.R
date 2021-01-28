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
#' @param type Type of validation to perform. Acceptable values include:
#'  \itemize{
#'  \item vertical - Checks for repetition vertically in species and/or bag fields, grouped by dl_state and dl_date
#'  \item horizontal - Checks for repetition horizontally, across each record
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

validate <-
  function(x, type){
    if(type == "vertical"){

      # Vertical validation

      validated_x <-
        x %>%
        select(
          dl_state,
          dl_date,
          matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) %>%
        group_by(dl_state, dl_date) %>%
        suppressMessages(summarize(across(contains("bag"), ~n_distinct(.)))) %>%
        mutate(
          # Create vertical uniformity col so the next step has something to paste
          # with
          v_uniformity = NA,
          # Paste col name into "uniformity" if it has only one value per dl_state
          # and dl_date (the grouping values)
          v_uniformity =
            ifelse(
              as.numeric(length(unique(ducks_bag))) == 1,
              paste0(v_uniformity, "-ducks_bag"),
              v_uniformity),
          v_uniformity =
            ifelse(
              as.numeric(length(unique(geese_bag))) == 1,
              paste0(v_uniformity, "-geese_bag"),
              v_uniformity),
          v_uniformity =
            ifelse(
              as.numeric(length(unique(dove_bag))) == 1,
              paste0(v_uniformity, "-dove_bag"),
              v_uniformity),
          v_uniformity =
            ifelse(
              as.numeric(length(unique(woodcock_bag))) == 1,
              paste0(v_uniformity, "-woodcock_bag"),
              v_uniformity),
          v_uniformity =
            ifelse(
              as.numeric(length(unique(coots_snipe))) == 1,
              paste0(v_uniformity, "-coots_snipe"),
              v_uniformity),
          v_uniformity =
            ifelse(
              as.numeric(length(unique(rails_gallinules))) == 1,
              paste0(v_uniformity, "-rails_gallinules"),
              v_uniformity),
          v_uniformity =
            ifelse(
              as.numeric(length(unique(cranes))) == 1,
              paste0(v_uniformity, "-cranes"),
              v_uniformity),
          v_uniformity =
            ifelse(
              as.numeric(length(unique(band_tailed_pigeon))) == 1,
              paste0(v_uniformity, "-band_tailed_pigeon"),
              v_uniformity),
          v_uniformity =
            ifelse(
              as.numeric(length(unique(brant))) == 1,
              paste0(v_uniformity, "-brant"),
              v_uniformity),
          v_uniformity =
            ifelse(
              as.numeric(length(unique(seaducks))) == 1,
              paste0(v_uniformity, "-seaducks"),
              v_uniformity),
          # Remove unnecessary leading NA- strings from the first step
          v_uniformity = str_remove(v_uniformity, "NA\\-")) %>%
        filter(!is.na(v_uniformity)) %>%
        select(dl_state, dl_date, v_uniformity) %>%
        # How many values are uniform within each group?
        mutate(n_uniform = n()) %>%
        ungroup() %>%
        distinct() %>%
        arrange(desc(n_uniform))

      if(nrow(validated_x) != 0) {

        message(
          paste0(
            "Attention: Uniform value detected within one or more fields, ",
            "please review.")
        )

        return(validated_x)

      }
      else{
        message("Data are good to go! No vertical repetition detected.")
      }
    }
    else if(type == "horizontal"){

      # Horizontal validation

      validated_x <-
        x %>%
        select(
          dl_state,
          dl_date,
          matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) %>%
        group_by(dl_state, dl_date) %>%
        suppressMessages(summarize(across(contains("bag"), ~n_distinct(.)))) %>%
        mutate(
          # Create horizontal uniformity col so the next step has something to paste
          # with
          h_uniformity = NA,

          # Remove unnecessary leading NA- strings from the first step
          h_uniformity = str_remove(h_uniformity, "NA\\-")) %>%
        filter(!is.na(h_uniformity)) %>%
        select(dl_state, dl_date, h_uniformity) %>%
        # How many values are uniform within each group?
        mutate(n_uniform = n()) %>%
        ungroup() %>%
        distinct() %>%
        arrange(desc(n_uniform))

      if(nrow(validated_x) != 0) {

        message(
          paste0(
            "Attention: Uniform value detected across one or more records, ",
            "please review.")
        )

        return(validated_x)

      }
      else{
        message("Data are good to go! No horizontal repetition detected.")
      }

    }
    else{
      message("Incorrect type of validation supplied.")
    }
  }
