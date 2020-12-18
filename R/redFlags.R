#' Pull bad data
#'
#' Create a tibble of error data by state or field. Data are reported using a threshold of proportion of error.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param type Type of tibble to report. Acceptable values include:
#'  \itemize{
#'  \item state
#'  \item field
#'  }
#' @param threshold Value above which errors should be tabulated
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

redFlags <-
  function(x, type, threshold = 0){

    states_provinces_and_canada <-
      c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI",
        "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
        "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
        "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
        "WV", "WI", "WY", "AS", "GU", "MP", "PR", "VI", "UM", "FM", "MH", "PW",
        "AA", "AE", "AP", "CM", "CZ", "NB", "PI", "TT", "ON", "QC", "NS", "NB",
        "MB", "BC", "PE", "SK", "AB", "NL")

    if(type == "state"){
      suppressWarnings(
        suppressMessages(
          x %>%
            select(errors, state) %>%
            mutate(temp_key = row_number()) %>%
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            pivot_longer(1:25, names_to = "name") %>%
            select(-name) %>%
            rename(errors = value) %>%
            mutate(
              state =
                ifelse(
                  state %in% states_provinces_and_canada,
                  state,
                  "Foreign")) %>%
            group_by(state) %>%
            summarize(
              count_errors = sum(!is.na(errors)),
              count_correct = sum(is.na(errors))) %>%
            ungroup() %>%
            mutate(
              proportion = count_errors / (count_errors + count_correct)) %>%
            mutate(
              flag =
                ifelse(
                  proportion > threshold,
                  paste0("error > ", threshold),
                  NA)) %>%
            filter(!is.na(flag)) %>%
            arrange(desc(proportion))
        )
      )
    }
    else if(type == "field"){
      suppressWarnings(
        suppressMessages(
          x %>%
            select(errors) %>%
            separate_rows(errors, sep = "-") %>%
            group_by(errors) %>%
            summarize(count_errors = sum(!is.na(errors))) %>%
            ungroup() %>%
            mutate(
              count_correct = nrow(x) - count_errors,
              proportion = count_errors / nrow(x)) %>%
            mutate(
              flag =
                ifelse(
                  proportion > threshold,
                  paste0("error > ", threshold),
                  NA)
            ) %>%
            filter(!is.na(flag)) %>%
            arrange(desc(proportion))
        )
      )
    }
    else{
      message("Error: Invalid type provided.")
    }

  }
