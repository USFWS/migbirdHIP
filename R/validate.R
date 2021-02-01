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
#' @param period Time period in which to group the data. The function uses dl_date automatically, but either of the following may be supplied:
#'  \itemize{
#'  \item dl_date - Date the HIP data were downloaded
#'  \item dl_cycle - Download cycle that the HIP data belong to
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

validate <-
  function(x, type, period = NA){
    if(type == "vertical"){

      # Vertical validation

      # Without time period specification
      if(is.na(period)){
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
      }

      # With time period specification: dl_date or dl_cycle
      else if(period == "dl_date" | period == "dl_cycle"){
        validated_v <-
          x %>%
          # Subset the data
          select(
            dl_state,
            {{period}},
            matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) %>%
          group_by(dl_state, !!sym(period)) %>%
          # Add number of rows per group
          mutate(dl_rows = n()) %>%
          # Move dl_rows to 3rd col position
          relocate(dl_rows, .after = {{period}}) %>%
          ungroup() %>%
          group_by(dl_state, !!sym(period), dl_rows) %>%
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
      }

      # Return for uniformity detected
      if(nrow(validated_v) != 0) {
       return(validated_v)
      }
      # Return for no uniformities
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

      # Return for uniformity detected
      if(nrow(validated_h) != 0) {
        return(validated_h)
      }
      # Return for no uniformities
      else{
        message("Data are good to go! No horizontal repetition detected.")
      }
    }

    # Return for incorrect type given
    else{
      message("Incorrect type of validation supplied.")
    }
  }
