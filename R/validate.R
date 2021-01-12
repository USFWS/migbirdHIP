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
          ifelse(
            as.numeric(length(unique(ducks_bag))) == 1,
            paste0(uniformity, "-ducks_bag"),
            NA),
        uniformity =
          ifelse(
            as.numeric(length(unique(geese_bag))) == 1,
            paste0(uniformity, "-geese_bag"),
            NA),
        uniformity =
          ifelse(
            as.numeric(length(unique(dove_bag))) == 1,
            paste0(uniformity, "-dove_bag"),
            NA),
        uniformity =
          ifelse(
            as.numeric(length(unique(woodcock_bag))) == 1,
            paste0(uniformity, "-woodcock_bag"),
            NA),
        uniformity =
          ifelse(
            as.numeric(length(unique(coots_snipe))) == 1,
            paste0(uniformity, "-coots_snipe"),
            NA),
        uniformity =
          ifelse(
            as.numeric(length(unique(rails_gallinules))) == 1,
            paste0(uniformity, "-rails_gallinules"),
            NA),
        uniformity =
          ifelse(
            as.numeric(length(unique(cranes))) == 1,
            paste0(uniformity, "-cranes"),
            NA),
        uniformity =
          ifelse(
            as.numeric(length(unique(band_tailed_pigeon))) == 1,
            paste0(uniformity, "-band_tailed_pigeon"),
            NA),
        uniformity =
          ifelse(
            as.numeric(length(unique(brant))) == 1,
            paste0(uniformity, "-brant"),
            NA),
        uniformity =
          ifelse(
            as.numeric(length(unique(seaducks))) == 1,
            paste0(uniformity, "-seaducks"),
            NA),
        # Remove unnecessary leading NA- strings from the first step
        uniformity = str_remove(uniformity, "NA\\-")) %>%
      filter(!is.na(uniformity)) %>%
      select(dl_state, dl_date, uniformity) %>%
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
      message("Data are good to go!")

    }
  }
