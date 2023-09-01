#' Flag errors
#'
#' After cleaning the data with \code{\link{clean}}, compare each field to an expected range of values and flag non-conforming values in a new "errors" column.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @importFrom dplyr row_number
#' @importFrom dplyr bind_rows
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr left_join
#' @importFrom dplyr reframe
#' @importFrom stringr str_detect
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr n
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#'
#' @param x The object created after cleaning data with \code{\link{clean}}
#' @param year The year in which the Harvest Information Program data were collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

proof <-
  function(x, year){

    # Combine US State, District and Territory abbreviations with Canada
    # abbreviations
    states_provinces_and_canada <-
      paste(c(datasets::state.abb, abbr_usa, abbr_canada), collapse = "|")

    # Create a record key so that the errors can be joined in later
    keyed_x <-
      x |>
      mutate(temp_key = paste0("row_", row_number()))

    markup <-
      bind_rows(
        # Title should be 1 or 2, no other values
        keyed_x |>
          filter(!str_detect(title, "0|1|2")) |>
          mutate(error = "title"),
        # First name
        keyed_x |>
          mutate(
            error =
              case_when(
                # First name should be >1 letter
                str_detect(firstname, "^[A-Z]{1}$") ~ "firstname",
                # First name should not contain 1 letter followed by a name
                str_detect(firstname, "^[A-Z]{1}\\s[A-Z]+$") ~ "firstname",
                # First name should not contain a name followed by 1 letter
                str_detect(firstname, "^[A-Z]+\\s[A-Z]{1}$") ~ "firstname",
                # Only non-alpha characters allowed are spaces, apostrophes, and
                # hyphens. No numbers, commas, parentheses, etc allowed
                str_detect(firstname, "[^A-Z\\s\\-\\']") ~ "firstname",
                # No full names (detect using 2+ spaces)
                str_detect(
                  firstname, "^[A-Z]+\\s[A-Z]+\\s[A-Z]+$") ~ "firstname",
                # Should not be "blank" or "inaudible"
                str_detect(
                  firstname, "^[INAUDIBLE|BLANK|TEST|USER|RESIDENT]$") ~
                  "firstname",
                TRUE ~ NA_character_)),
        # Middle name should only be 1 letter of the alphabet
        keyed_x |>
          filter(!str_detect(middle, "^[A-Z]{1}$")) |>
          mutate(error = "middle"),
        # Last name
        keyed_x |>
          mutate(
            error =
              case_when(
                # Last name should be >1 letter
                str_detect(lastname, "^[A-Z]{1}$") ~ "lastname",
                # Only non-alpha characters allowed are spaces, periods,
                # hyphens, and apostrophes. No numbers, commas, parentheses, etc
                str_detect(lastname, "[^A-Z\\s\\-\\.\\']") ~ "lastname",
                # No full names (detect using 2+ spaces)
                str_detect(
                  lastname, "^[A-Z]+\\s[A-Z]+\\s[A-Z]+$") ~ "lastname",
                # Should not be "inaudible"
                str_detect(lastname, "^INAUDIBLE$") ~ "lastname",
                TRUE ~ NA_character_)),
        # Suffix
        # Allows 1-20 in Roman numerals and numeric, excluding XVIII since the
        # limit is 4 characters)
        keyed_x |>
          filter(
            str_detect(
              suffix,
              "[^JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH]")) |>
          mutate(error = "suffix"),
        # Address does not contain |, tab or non-UTF8 characters
        # Any further address verification isn't really possible
        keyed_x |>
          filter(str_detect(address, "\\||\\t|[^\\x00-\\x7F]+")) |>
          mutate(error = "address"),
        # City should only contain letters, spaces (e.g. New York City), hyphens
        # (e.g. Winston-Salem, NC), or apostrophes (e.g. HI residents and
        # O'Fallon, MO)
        keyed_x |>
          filter(str_detect(city, "[^A-Za-z\\s\\-\\']")) |>
          mutate(error = "city"),
        # City MUST contain at least 3 letters
        keyed_x |>
          filter(str_detect(city, "^[A-Za-z]{1,2}$")) |>
          mutate(error = "city"),
        # State should only contain a specific list of states/provinces/etc
        keyed_x |>
          filter(!str_detect(state, states_provinces_and_canada)) |>
          mutate(error = "state"),
        # Zip code should be in the reference table
        keyed_x |>
          filter(!str_extract(zip, "^[0-9]{5}") %in% zip_code_ref$zipcode) |>
          mutate(error = "zip"),
        # Birth date should only ever be between 100 and 16 years go, since
        # hunters do not have to register if less than 16 years of age & years
        # from earlier than 100 years ago are unlikely
        keyed_x |>
          filter(
            as.numeric(
              str_extract(birth_date, "(?<=\\/)[0-9]{4}")) < year - 100 |
              as.numeric(
                str_extract(birth_date, "(?<=\\/)[0-9]{4}")) > year - 16) |>
          mutate(error = "birth_date"),
        # Hunting migratory birds should only be = 1 or 2
        keyed_x |>
          filter(!hunt_mig_birds %in% c("1", "2")) |>
          mutate(error = "hunt_mig_birds"),
        # Registration year should = survey year
        keyed_x |>
          filter(registration_yr != year) |>
          mutate(error = "registration_yr"),
        # Email
        keyed_x |>
          filter(
            !str_detect(
              email, "^[a-zA-Z0-9_.-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$")) |>
          mutate(error = "email")
      ) |>
      select(temp_key, error)

    graded_x <-
      keyed_x |>
      # Join in the error report
      left_join(markup, by = "temp_key") |>
      group_by(temp_key) |>
      # Paste errors together (some records might have more than one!)
      mutate(errors = paste(error, collapse = "-")) |>
      ungroup() |>
      select(-c(temp_key, error)) |>
      distinct() |>
      # Make the NAs "real NAs" not just strings
      mutate(
        errors =
          ifelse(
            str_detect(errors, "NA\\-|\\-NA"),
            str_remove_all(errors, "NA\\-|\\-NA"),
            errors)) |>
      # Add a second mutate here because we cannot pipe '.'
      mutate(errors = ifelse(str_detect(errors, "^NA$"), NA, errors)) |>
      as_tibble()

    # Proof the zip codes. Are they associated with the correct states?
    graded_x <-
      graded_x |>
      left_join(
        zip_code_ref |>
          distinct(zip = zipcode, zipState = state),
        by = "zip") |>
      # Add an error if the state doesn't match zipState
      mutate(
        errors =
          case_when(
            state != zipState & is.na(errors) ~ "zip",
            state != zipState & !is.na(errors) & !str_detect(errors, "zip") ~
              paste0(errors, "-zip"),
            TRUE ~ errors)
      ) |>
      select(-zipState)

    return(graded_x)

  }
