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

      validated_v <-
        x %>%
        # Subset the data
        select(
          dl_state,
          dl_date,
          matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) %>%
        group_by(dl_state, dl_date) %>%
        # Add number of rows per group
        mutate(dl_rows = n()) %>%
        # Move dl_rows to 3rd col position
        relocate(dl_rows, .after = dl_date) %>%
        ungroup() %>%
        group_by(dl_state, dl_date, dl_rows) %>%
        # Count the number of unique values in each species column
        summarize(across(ducks_bag:seaducks, ~length(unique(.x))), .groups = "keep") %>%
        pivot_longer(
          cols = !contains("dl"),
          names_to = "species_grp",
          values_to = "v_uniformity") %>%
        ungroup() %>%
        # Keep only the uniform spp groups
        filter(v_uniformity == 1) %>%
        # Report count of times uniform values are repeated
        mutate(v_uniform = dl_rows) %>%
        select(-c("dl_rows", "v_uniformity"))

      if(nrow(validated_v) != 0) {

        message(
          paste0(
            "Attention: Uniform value detected within one or more fields, ",
            "please review.")
        )

        return(validated_v)

      }
      else{
        message("Data are good to go! No vertical repetition detected.")
      }
    }
    else if(type == "horizontal"){

      # Horizontal validation

      h_test <-
        x %>%
        select(
          dl_state,
          dl_date,
          matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) %>%
        group_by(dl_state, dl_date) %>%
        # Paste all of the species group values together
        unite(h_string, !contains("dl"), sep = "-") %>%
        ungroup() %>%
        # Convert string to vector
        mutate(
          h_string = str_split(h_string, "-"),
          rowkey = row_number())

      validated_h <-
        h_test %>%
        group_by(rowkey) %>%
        mutate(h_validate = length(unique(h_test$h_string[[rowkey]]))) %>%
        ungroup() %>%
        select(-c("h_string", "rowkey")) %>%
        mutate(h_uniform = ifelse(h_validate == "1", "uniform", "non-uniform")) %>%
        select(-h_validate) %>%
        filter(h_uniform == "uniform") %>%
        group_by(dl_state, dl_date) %>%
        summarize(h_uniform = n(), .groups = "keep") %>%
        ungroup()

      if(nrow(validated_h) != 0) {

        message(
          paste0(
            "Attention: Uniform value detected across one or more records, ",
            "please review.")
        )

        return(validated_h)

      }
      else{
        message("Data are good to go! No horizontal repetition detected.")
      }

    }
    else{
      message("Incorrect type of validation supplied.")
    }
  }
