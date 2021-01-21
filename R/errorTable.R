#' Table of errors
#'
#' Create a tibble of existing errors in the data, customizing the output with value specifications.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param loc Which location the error data should be tabulated by. Acceptable values include:
#'  \itemize{
#'  \item One of the following abbreviations:
#'  \itemize{
#'  \item AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY, AS, GU, MP, PR, VI, UM, FM, MH, PW, AA, AE, AP, CM, CZ, NB, PI, TT, ON, QC, NS, NB, MB, BC, PE, SK, AB, NL}
#'  \item all - All states, provinces, and/or territories that exist in the data
#'  \item none - Table will not include location in its output
#'  }
#' @param field Field the error data should be tabulated by. Acceptable values include:
#'  \itemize{
#'  \item One of the following fields:
#'  \itemize{
#'  \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe_bag, rails_gallinules_bag, cranes, band_tailed_pigeon, brant, seaducks, registration_year, email}
#'  \item all - All fields in the data
#'  \item none - Table will not include field in its output
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

errorTable <-
  function(x, loc = "all", field = "all"){

    states_provinces_and_canada <-
      c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI",
        "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
        "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
        "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
        "WV", "WI", "WY", "AS", "GU", "MP", "PR", "VI", "UM", "FM", "MH", "PW",
        "AA", "AE", "AP", "CM", "CZ", "NB", "PI", "TT", "ON", "QC", "NS", "NB",
        "MB", "BC", "PE", "SK", "AB", "NL")

    summary_table <-
      suppressWarnings(
        suppressMessages(
          if(loc == "all" & field == "all") {
            # Summary table of errors by state and field
            x %>%
              select(errors, dl_state) %>%
              filter(!is.na(errors)) %>%
              mutate(temp_key = row_number()) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              rename(errors = value) %>%
              filter(!is.na(errors)) %>%
              mutate(temp_key = 1) %>%
              group_by(dl_state, errors) %>%
              summarize(error_count = sum(temp_key)) %>%
              ungroup() %>%
              filter(dl_state %in% states_provinces_and_canada) %>%
              rename(error = errors)
          }
          else if(loc == "all" & field == "none"){
            # Summary table of errors by state only
            x %>%
              select(errors, dl_state) %>%
              filter(!is.na(errors)) %>%
              mutate(temp_key = row_number()) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              rename(errors = value) %>%
              filter(!is.na(errors)) %>%
              mutate(temp_key = 1) %>%
              group_by(dl_state) %>%
              summarize(error_count = sum(temp_key)) %>%
              ungroup() %>%
              filter(dl_state %in% states_provinces_and_canada)
          }
          else if(loc == "none" & field == "all"){
            # Summary table of errors by field name
            x %>%
              select(errors, dl_state) %>%
              filter(!is.na(errors)) %>%
              mutate(temp_key = row_number()) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              rename(errors = value) %>%
              filter(!is.na(errors)) %>%
              mutate(temp_key = 1) %>%
              group_by(errors) %>%
              summarize(error_count = sum(temp_key)) %>%
              ungroup() %>%
              rename(error = errors)
          }
          else if(loc == "all" & !str_detect(field, "none|all")){
            # Summary table across all states for a particular field
            x %>%
              select(errors, dl_state) %>%
              filter(!is.na(errors)) %>%
              mutate(temp_key = row_number()) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              rename(errors = value) %>%
              filter(!is.na(errors)) %>%
              mutate(temp_key = 1) %>%
              group_by(errors) %>%
              summarize(error_count = sum(temp_key)) %>%
              ungroup() %>%
              rename(error = errors) %>%
              filter(error == field)
          }
          else if(!str_detect(loc, "none|all") & field == "all"){
            # Summary table for a particular state with all fields
            if(loc %in% states_provinces_and_canada){
              x %>%
                select(errors, dl_state) %>%
                filter(dl_state == loc) %>%
                filter(!is.na(errors)) %>%
                mutate(temp_key = row_number()) %>%
                separate(errors, into = as.character(c(1:25)), sep = "-") %>%
                pivot_longer(1:25, names_to = "name") %>%
                select(-name) %>%
                rename(errors = value) %>%
                filter(!is.na(errors)) %>%
                mutate(temp_key = 1) %>%
                group_by(dl_state, errors) %>%
                summarize(error_count = sum(temp_key)) %>%
                ungroup() %>%
                rename(error = errors)}
            else{
              warning("Invalid location.")
            }
          }
          else if(!str_detect(loc, "none|all") & field == "none"){
            # Summary table for a particular state with all fields
            if(loc %in% states_provinces_and_canada){
              x %>%
                select(errors, dl_state) %>%
                filter(dl_state == loc) %>%
                filter(!is.na(errors)) %>%
                mutate(temp_key = row_number()) %>%
                separate(errors, into = as.character(c(1:25)), sep = "-") %>%
                pivot_longer(1:25, names_to = "name") %>%
                select(-name) %>%
                rename(errors = value) %>%
                filter(!is.na(errors)) %>%
                mutate(temp_key = 1) %>%
                group_by(dl_state) %>%
                summarize(error_count = sum(temp_key)) %>%
                ungroup() %>%
                rename(total_errors = error_count)
            }
            else{
              warning("Invalid location.")
            }
          }
          else if(
            !str_detect(loc, "none|all") & !str_detect(field, "none|all")){
            # Summary table for a particular state and particular field name
            if(loc %in% states_provinces_and_canada){
              x %>%
                select(errors, dl_state) %>%
                filter(dl_state == loc) %>%
                filter(!is.na(errors)) %>%
                mutate(temp_key = row_number()) %>%
                separate(errors, into = as.character(c(1:25)), sep = "-") %>%
                pivot_longer(1:25, names_to = "name") %>%
                select(-name) %>%
                rename(errors = value) %>%
                filter(!is.na(errors)) %>%
                mutate(temp_key = 1) %>%
                group_by(dl_state, errors) %>%
                summarize(error_count = sum(temp_key)) %>%
                ungroup() %>%
                rename(error = errors) %>%
                filter(error == field)
            }
            else{
              warning("Invalid location.")
            }
          }
          else{
            NULL
          }
        )
      )

    return(summary_table)
  }
