#' Flag errors
#'
#' After tidying the data with \code{\link{tidy}}, compare each field to an expected range of values and flag non-conforming values in a new "errors" column.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom dplyr as_tibble
#'
#' @param x The object created after tidying data with \code{\link{tidy}}
#' @param year The year in which the Harvest Information Program data were collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

proof <-
  function(x, year){

    states_provinces_and_canada <-
      paste0(
        "AL|AK|AZ|AR|CA|CO|CT|DE|DC|FL|GA|HI|ID|IL|IN|IA|KS|KY|LA|ME|MD|MA|",
        "MI|MN|MS|MO|MT|NE|NV|NH|NJ|NM|NY|NC|ND|OH|OK|OR|PA|RI|SC|SD|TN|TX|",
        "UT|VT|VA|WA|WV|WI|WY|AS|GU|MP|PR|VI|UM|FM|MH|PW|AA|AE|AP|CM|CZ|NB|",
        "PI|TT|ON|QC|NS|NB|MB|BC|PE|SK|AB|NL")

    # Create a record key so that the errors can be joined in later
    keyed_x <-
      x %>%
      mutate(temp_key = paste0("row_", row_number()))

    markup <-
      bind_rows(
        # Title should be 1 or 2, no other values
        keyed_x %>%
          filter(!str_detect(title, "1|2")) %>%
          mutate(error = "title"),
        # First name
        keyed_x %>%
          mutate(
            error =
              case_when(
                # First name should be >1 letter
                str_detect(firstname, "^[A-Z]{1}$") ~ "firstname",
                # First name should not contain a first initial/middle name
                str_detect(firstname, "^[A-Z]{1}(?=\\s)") ~ "firstname",
                # First name should not contain a first name/middle initial
                str_detect(firstname, ".+(?<=\\s)[A-Z]{1}$") ~ "firstname",
                # Only non-alpha characters allowed are spaces and hyphens
                # no numbers, commas, parentheses, etc
                str_detect(firstname, "[^A-Z\\s\\-]") ~ "firstname",
                # No full names (detect using 2+ spaces)
                str_detect(
                  firstname, "^.+[(?<=//s)[a-z](?=//s)].+$") ~ "firstname",
                # Should not be "blank" or "inaudible"
                str_detect(
                  firstname, "^[INAUDIBLE|BLANK|TEST|USER|RESIDENT]$") ~
                  "firstname",
                TRUE ~ NA_character_)),
        # Middle name should only be 1 letter of the alphabet
        keyed_x %>%
          filter(str_detect(middle, "[^A-Z]{1}")) %>%
          mutate(error = "middle"),
        # Last name
        keyed_x %>%
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
                  lastname, "^.+[(?<=//s)[a-z](?=//s)].+$") ~ "lastname",
                # Should not be "inaudible"
                str_detect(lastname, "^[INAUDIBLE]$") ~ "lastname",
                TRUE ~ NA_character_)),
        # Suffix
        keyed_x %>%
          filter(
            str_detect(
              suffix,
              "[^JR|SR|I|II|III|IV|V|VI|VII|1ST|2ND|3RD|4TH|5TH|6TH|7TH|8TH|9TH]")) %>%
          mutate(error = "suffix"),
        # Address does not contain |, tab or non-UTF8 characters
        # Any further address verification isn't really possible
        keyed_x %>%
          filter(
            str_detect(
              address, "\\||\\t|\\x00-\\x7F")) %>%
          mutate(error = "address"),
        # City should only contain letters
        keyed_x %>%
          filter(!str_detect(city, "[A-Z|a-z]")) %>%
          mutate(error = "city"),
        # State should only contain a specific list of states/provinces/etc
        keyed_x %>%
          filter(!str_detect(state, states_provinces_and_canada)) %>%
          mutate(error = "state"),
        # Zip code should contain 5 digits or 5 digits-4 digits
        # Edit: reference an official list against the state
        keyed_x %>%
          mutate(
            error =
              case_when(
                str_detect(zip, "^[0-9]{9}$") ~ "zip",
                str_detect(zip, "\\-0000$") ~ "zip",
                str_detect(zip, "[^[0-9]{5}|[0-9]{5}\\-[0-9]{4}]") ~ "zip",
                TRUE ~ NA_character_)),
        # Birth date should only ever be 100 - 16 years go, since hunter do not
        # have to register if less than 16 years of age
        keyed_x %>%
          filter(
            as.numeric(
              str_extract(birth_date, "(?<=\\/)[0-9]{4}")) < 1920 |
              as.numeric(
                str_extract(birth_date, "(?<=\\/)[0-9]{4}")) >= year - 16) %>%
          mutate(error = "birth_date"),
        # Issue date should only ever be the year +/- 1 of the HIP survey
        keyed_x %>%
          filter(
            as.numeric(
              str_extract(issue_date, "(?<=\\/)[0-9]{4}")) != year &
              as.numeric(
                str_extract(issue_date, "(?<=\\/)[0-9]{4}")) != year + 1 &
              as.numeric(
                str_extract(issue_date, "(?<=\\/)[0-9]{4}")) != year - 1) %>%
          mutate(error = "issue_date"),
        # Hunting migratory birds should only be = 1 or 2
        keyed_x %>%
          filter(!str_detect(hunt_mig_birds, "1|2")) %>%
          mutate(error = "hunt_mig_birds"),
        # Ducks bag should only = 1, 2, 3, 4
        # Zero is an option, but values shouldn't be zero
        keyed_x %>%
          filter(!str_detect(ducks_bag, "1|2|3|4")) %>%
          mutate(error = "ducks_bag"),
        # Geese bag should only = 0, 1, 2, 3, 4
        keyed_x %>%
          filter(!str_detect(geese_bag, "0|1|2|3|4")) %>%
          mutate(error = "geese_bag"),
        # Dove bag should only = 0, 1, 2, 3, 5
        keyed_x %>%
          filter(!str_detect(dove_bag, "0|1|2|3|5")) %>%
          mutate(error = "dove_bag"),
        # Woodcock bag should only = 0, 1, 2, 3, 5
        keyed_x %>%
          filter(!str_detect(woodcock_bag, "0|1|2|3|5")) %>%
          mutate(error = "woodcock_bag"),
        # Coots/snipe bag should only = 0, 1, 2
        keyed_x %>%
          filter(!str_detect(coots_snipe, "0|1|2")) %>%
          mutate(error = "coots_snipe"),
        # Rails/gallinules bag should only = 0, 1, 2
        keyed_x %>%
          filter(!str_detect(rails_gallinules, "0|1|2")) %>%
          mutate(error = "rails_gallinules"),
        # Cranes should only = 0, 1, 2
        keyed_x %>%
          filter(!str_detect(cranes, "0|1|2")) %>%
          mutate(error = "cranes"),
        # Band-tailed Pigeon should only = 0, 1, 2
        keyed_x %>%
          filter(!str_detect(band_tailed_pigeon, "0|1|2")) %>%
          mutate(error = "band_tailed_pigeon"),
        # Brant should only = 0, 1, 2
        keyed_x %>%
          filter(!str_detect(brant, "0|1|2")) %>%
          mutate(error = "brant"),
        # Seaducks should only = 0, 1, 2
        keyed_x %>%
          filter(!str_detect(seaducks, "0|1|2")) %>%
          mutate(error = "seaducks"),
        # Registration year should = survey year +/- 1
        keyed_x %>%
          filter(
            registration_yr != year &
              registration_yr != year + 1 &
              registration_yr != year - 1) %>%
          mutate(error = "registration_yr"),
        # Email
        keyed_x %>%
          filter(
            !str_detect(
              email, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$")) %>%
          mutate(error = "email")
      ) %>%
      select(temp_key, error)

    graded_x <-
      keyed_x %>%
      # Join in the error report
      left_join(markup, by = "temp_key") %>%
      group_by(temp_key) %>%
      # Paste errors together (some records might have more than one!)
      mutate(errors = paste(error, collapse = "-")) %>%
      ungroup() %>%
      select(-c(temp_key, error)) %>%
      distinct() %>%
      # Make the NAs "real NAs" not just strings
      mutate(
        errors =
          ifelse(
            str_detect(errors, "NA\\-|\\-NA"),
            str_remove_all(errors, "NA\\-|\\-NA"),
            errors)) %>%
      # Add a second mutate here because we cannot pipe '.'
      mutate(
        errors = ifelse(str_detect(errors, "^NA$"), NA, errors)) %>%
      as_tibble()

    return(graded_x)

  }
