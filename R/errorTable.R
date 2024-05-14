#' Table of errors
#'
#' Create a tibble of existing errors in the data, customizing the output with value specifications.
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr starts_with
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr count
#' @importFrom dplyr ungroup
#' @importFrom stringr str_detect
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param loc Which location the error data should be tabulated by. Acceptable values include:
#'  \itemize{
#'  \item One of the following abbreviations:
#'  \itemize{
#'  \item AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY}
#'  \item all - All states, provinces, and/or territories that exist in the data
#'  \item none - Table will not include location in its output
#'  }
#' @param field Field the error data should be tabulated by. Acceptable values include:
#'  \itemize{
#'  \item If loc = "none", field must be "all". Otherwise, choose one of the following values:
#'  \itemize{
#'  \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks, registration_yr, email}
#'  \item all - All fields in the data
#'  \item none - Table will not include field in its output
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

errorTable <-
  function(x, loc = "all", field = "all"){

    # Fail if incorrect loc supplied
    stopifnot("Error: Incorrect value supplied for `loc` parameter. Please supply a two-letter state abbreviation of a `dl_state` value contained within the data, 'all', or 'none'." = loc %in% c(unique(x$dl_state), "all", "none"))

    # Fail if incorrect field supplied
    stopifnot("Error: Incorrect value supplied for `field` parameter. Please supply one of: all, none, title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks, registration_yr, email." = field %in% c("all", "none", "title", "firstname", "middle", "lastname", "suffix", "address", "city", "state", "zip", "birth_date", "issue_date", "hunt_mig_birds", "ducks_bag", "geese_bag", "dove_bag", "woodcock_bag", "coots_snipe", "rails_gallinules", "cranes", "band_tailed_pigeon", "brant", "seaducks", "registration_yr", "email"))

    if (loc == "none" & field != "all"){
      message("Error! If `loc = 'none'` then `field` must be 'all'.")
    } else {

      # Initial summary table
      initial_tbl <-
        x |>
        select(errors, dl_state) |>
        filter(!is.na(errors)) |>
        # Pull errors apart, delimited by hyphens
        separate_wider_delim(
          errors, delim = "-", names_sep = "_", too_few = "align_start") |>
        # Transform errors into a single column
        pivot_longer(starts_with("errors"), names_to = "name") |>
        select(dl_state, errors = value) |>
        filter(!is.na(errors))

      summary_table <-
        if(loc == "all" & field == "all") {

          # Summary table of errors by state and field
          initial_tbl |>
            group_by(dl_state, errors) |>
            count() |>
            ungroup() |>
            rename(error = errors, error_count = n)

        } else if(loc == "all" & field == "none") {

          # Summary table of errors by state only
          initial_tbl |>
            group_by(dl_state) |>
            count() |>
            ungroup() |>
            rename(error_count = n)

        } else if(loc == "none" & field == "all") {

          # Summary table of errors by field name
          initial_tbl |>
            group_by(errors) |>
            count() |>
            ungroup() |>
            rename(error = errors, error_count = n)

        } else if(loc == "all" & !str_detect(field, "none|all")) {

          # Summary table across all states for a particular field
          initial_tbl |>
            group_by(errors) |>
            count() |>
            ungroup() |>
            rename(error = errors, error_count = n) |>
            filter(error == field)

        } else if(!str_detect(loc, "none|all") & field == "all") {

          # Summary table for a particular state with all fields
          initial_tbl |>
            filter(dl_state == loc) |>
            group_by(dl_state, errors) |>
            count() |>
            ungroup() |>
            rename(error = errors, error_count = n)

        } else if(!str_detect(loc, "none|all") & field == "none") {

          # Summary table for a particular state with all fields
          initial_tbl |>
            filter(dl_state == loc) |>
            group_by(dl_state) |>
            count() |>
            ungroup() |>
            rename(total_errors = n)

        } else if(!str_detect(loc, "none|all") & !str_detect(field, "none|all")) {

          # Summary table for a particular state and particular field name
          if(loc %in% unique(x$dl_state)) {

            statefield <-
              initial_tbl |>
              filter(dl_state == loc & errors == field)

            if(nrow(statefield) > 0) {

              statefield |>
                group_by(dl_state, errors) |>
                count() |>
                ungroup() |>
                rename(error = errors) |>
                filter(error == field) |>
                rename(error_count = n)

            } else {
              message(paste0("No errors in ", field, " for ", loc, "."))
            }
          }

        } else {
          NULL
        }

      if(!is.null(summary_table)) {
        return(summary_table)
      }

    }
  }
