#' Table of errors
#'
#' Create a tibble of existing errors in the data, customizing the output with value specifications.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom tidyr separate
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom stringr str_detect
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

    # String of 49 continental US states that provide HIP data to FWS
    acceptable_49_dl_states <-
      c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL",
        "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
        "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR",
        "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
        "WY")

    summary_table <-
      # Suppress warning: "Expected 25 pieces. Missing pieces filled with `NA`
      # in ... rows". We start by splitting errors for plotting purposes; if
      # there are less than the full amount of errors in a row, the warning
      # happens.
      suppressWarnings(
        # Suppress message from summarize: "`summarise()` regrouping output by
        # 'dl_state' (override with `.groups` argument)
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
              filter(dl_state %in% acceptable_49_dl_states) %>%
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
              filter(dl_state %in% acceptable_49_dl_states)
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

            if(loc %in% acceptable_49_dl_states){
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
              message("Invalid location.")
            }
          }
          else if(!str_detect(loc, "none|all") & field == "none"){

            # Summary table for a particular state with all fields

            if(loc %in% acceptable_49_dl_states){
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
              message("Invalid location.")
            }
          }
          else if(
            !str_detect(loc, "none|all") & !str_detect(field, "none|all")){

            # Summary table for a particular state and particular field name

            if(loc %in% acceptable_49_dl_states){
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
              message("Invalid location.")
            }
          }
          else{
            NULL
          }
        )
      )

    return(summary_table)
  }
