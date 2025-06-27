#' Table of errors
#'
#' Create a tibble of existing errors in the data, customizing the output with
#' value specifications.
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr starts_with
#' @importFrom dplyr rename
#' @importFrom dplyr count
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}} or \code{\link{correct}}
#' @param loc Which location the error data should be tabulated by. Acceptable
#'   values include:
#'  \itemize{
#'  \item a two-letter abbreviation for a US state (excluding HI)
#'  \item "all" - all states
#'  \item "none" - table will not include location in its output
#'  }
#' @param field Field the error data should be tabulated by. Acceptable values
#'   include:
#'  \itemize{
#'  \item If loc = "none", field must be "all". Otherwise, choose one of:
#'  \itemize{
#'  \item REF_ALL_FIELDS}
#'  \item "all" - all fields
#'  \item "none" - table will not include field in its output
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

errorTable <-
  function(proofed_data, loc = "all", field = "all") {
    failproofed(proofed_data)

    # Fail if incorrect loc supplied
    stopifnot("Error: Incorrect value supplied for `loc` parameter. Please supply a two-letter state abbreviation of a `dl_state` value contained within the data, 'all', or 'none'." = loc %in% c(unique(proofed_data$dl_state), "all", "none"))

    # Fail if incorrect field supplied
    stopifnot("Error: Incorrect value supplied for `field` parameter. Please supply one of: all, none, title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks, registration_yr, email." = field %in% c("all", "none", "title", "firstname", "middle", "lastname", "suffix", "address", "city", "state", "zip", "birth_date", "issue_date", "hunt_mig_birds", "ducks_bag", "geese_bag", "dove_bag", "woodcock_bag", "coots_snipe", "rails_gallinules", "cranes", "band_tailed_pigeon", "brant", "seaducks", "registration_yr", "email"))

    if (loc == "none" & field != "all"){
      message("Error! If `loc = 'none'` then `field` must be 'all'.")
    } else {

      # Initial summary table
      initial_tbl <-
        proofed_data |>
        select(c("errors", "dl_state")) |>
        filter(!is.na(.data$errors)) |>
        # Pull errors apart, delimited by hyphens
        separate_wider_delim(
          .data$errors,
          delim = "-", names_sep = "_", too_few = "align_start") |>
        # Transform errors into a single column
        pivot_longer(starts_with("errors"), names_to = "name") |>
        select(.data$dl_state, errors = .data$value) |>
        filter(!is.na(.data$errors))

      summary_table <- errorTableSummary(proofed_data, initial_tbl, loc, field)

      if(!is.null(summary_table)) {
        return(summary_table)
      }

    }
  }

#' Summary table of errors
#'
#' Internal function used inside of \code{\link{errorTable}}.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr count
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}} or \code{\link{correct}}
#' @param initial_tbl \code{\link{errorTable}} intermediate object
#' @param loc Which location the error data should be tabulated by. Acceptable
#'   values include:
#'  \itemize{
#'  \item a two-letter abbreviation for a US state (excluding HI)
#'  \item "all" - all states
#'  \item "none" - table will not include location in its output
#'  }
#' @param field Field the error data should be tabulated by. Acceptable values
#'   include:
#'  \itemize{
#'  \item If loc = "none", field must be "all". Otherwise, choose one of:
#'  \itemize{
#'  \item REF_ALL_FIELDS}
#'  \item "all" - all fields
#'  \item "none" - table will not include field in its output
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

errorTableSummary <-
  function(proofed_data, initial_tbl, loc, field) {
    failproofed(proofed_data)

    if (loc == "all" & field == "all") {
      # Summary table of errors by state and field
      initial_tbl |>
        count(.data$dl_state, .data$errors) |>
        rename(error = .data$errors, error_count = .data$n)

    } else if (loc == "all" & field == "none") {
      # Summary table of errors by state only
      initial_tbl |>
        count(.data$dl_state) |>
        rename(error_count = .data$n)

    } else if (loc == "none" & field == "all") {
      # Summary table of errors by field name
      initial_tbl |>
        count(.data$errors) |>
        rename(error = .data$errors, error_count = .data$n)

    } else if (loc == "all" & !str_detect(field, "none|all")) {
      # Summary table across all states for a particular field
      initial_tbl |>
        count(.data$errors) |>
        rename(error = .data$errors, error_count = .data$n) |>
        filter(.data$error == field)

    } else if (!str_detect(loc, "none|all") & field == "all") {
      # Summary table for a particular state with all fields
      initial_tbl |>
        filter(.data$dl_state == loc) |>
        count(.data$dl_state, .data$errors) |>
        rename(error = .data$errors, error_count = .data$n)

    } else if (!str_detect(loc, "none|all") & field == "none") {
      # Summary table for a particular state with all fields
      initial_tbl |>
        filter(.data$dl_state == loc) |>
        count(.data$dl_state) |>
        rename(total_errors = .data$n)

    } else if (!str_detect(loc, "none|all") & !str_detect(field, "none|all")) {
      # Summary table for a particular state and particular field name
      if (loc %in% unique(proofed_data$dl_state)) {
        statefield <-
          initial_tbl |>
          filter(.data$dl_state == loc & .data$errors == field)

        if (nrow(statefield) > 0) {
          statefield |>
            count(.data$dl_state, .data$errors) |>
            rename(error = .data$errors) |>
            filter(.data$error == field) |>
            rename(error_count = .data$n)

        } else {
          message(paste0("No errors in ", field, " for ", loc, "."))
        }
      }
    } else {
      NULL
    }
  }

#' Pull flagged errors
#'
#' Pull and view errors that have been flagged for a particular field. This
#' function allows you to easily see what the \code{\link{proof}} function has
#' determined to be unacceptable data.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom stringr str_detect
#' @importFrom rlang sym
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}}
#' @param field Field that should be pulled. One of the fields from the
#'   following list may be supplied:
#' \itemize{
#' \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks, registration_yr, email}
#' @param unique If FALSE, returns all error values; if TRUE (default), only
#'   returns unique values.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

pullErrors <-
  function(proofed_data, field, unique = TRUE){
    failproofed(proofed_data)

    # Fail if incorrect field supplied
    stopifnot("Error: Incorrect value supplied for `field` parameter." = field %in% REF_ALL_FIELDS)

    # Fail if incorrect unique supplied
    stopifnot("Error: Please supply TRUE or FALSE for `unique` parameter." = unique %in% c(TRUE, FALSE, T, F))

    pulled_error <-
      proofed_data |>
      select(!!sym(field), .data$errors) |>
      filter(str_detect(.data$errors, field)) |>
      select(!!sym(field))

    if (nrow(pulled_error) == 0) {
      message("Success! All values are correct.")
    } else {
      if (unique == TRUE) {
        pulled_error <- distinct(pulled_error)
      }
      return(pull(pulled_error))
    }
  }
