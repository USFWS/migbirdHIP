#' Flag errors
#'
#' After cleaning the data with \code{\link{clean}}, compare each field to an expected range of values and flag non-conforming values in a new "errors" column.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr as_tibble
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
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_length
#'
#' @param deduplicated_data The object created after deduplicating data with \code{\link{duplicateFix}}
#' @param year The year in which the Harvest Information Program data were collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

proof <-
  function(deduplicated_data, year){

    # Fail if incorrect year supplied
    stopifnot("Error: `year` parameter must be numeric." = is.numeric(year))
    stopifnot("Error: Incorrect value supplied for `year` parameter. Please use a 4-digit year in the 2020s, e.g. 2024." = str_detect(year, "^202[0-9]{1}$"))

    # Create a record key so that the errors can be joined in later (there may
    # be more than 1 error per record, so using the record_key field will not
    # work)
    keyed_data <-
      deduplicated_data |>
      mutate(temp_key = paste0("row_", row_number()))

    markup <-
      bind_rows(
        # Test record (redundant, should be filtered out in clean)
        keyed_data |>
          filter(!!LOGIC_TEST_RECORD) |>
          mutate(error = "test_record"),
        # Title
        keyed_data |>
          filter(!title %in% REF_TITLES) |>
          mutate(error = "title"),
        # First name
        keyed_data |>
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
                TRUE ~ NA_character_)),
        # Middle initial
        keyed_data |>
          filter(!middle %in% LETTERS) |>
          mutate(error = "middle"),
        # Last name
        keyed_data |>
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
                TRUE ~ NA_character_)),
        # Suffix
        keyed_data |>
          filter(!suffix %in% REF_SUFFIXES) |>
          mutate(error = "suffix"),
        # Address does not contain |, tab or non-UTF8 characters
        # Any further address verification isn't really possible
        keyed_data |>
          filter(str_detect(address, "\\||\\t|[^\\x00-\\x7F]+")) |>
          mutate(error = "address"),
        # City names should only contain letters, spaces (e.g., New York City,
        # NY), hyphens (e.g., Winston-Salem, NC), apostrophes (e.g., O'Fallon,
        # MO), and/or periods (e.g., St. Augustine, FL)
        keyed_data |>
          filter(str_detect(city, REGEX_CITY)) |>
          mutate(error = "city"),
        # City MUST contain at least 3 letters
        keyed_data |>
          filter(str_detect(city, "^[A-Za-z]{1,2}$")) |>
          mutate(error = "city"),
        # State should only be a 2-letter abbreviation for 1) a US state, 2) as
        # US territory, or 3) a Canadian province or territory
        keyed_data |>
          filter(!state %in% REF_USA_CANADA) |>
          mutate(error = "state"),
        # Zip code should be in the reference table
        keyed_data |>
          filter(!str_extract(zip, "^[0-9]{5}") %in% zip_code_ref$zipcode) |>
          mutate(error = "zip"),
        # Birth date should only ever be between 100 and 0 years ago
        keyed_data |>
          filter(
            as.numeric(
              str_extract(birth_date, "(?<=\\/)[0-9]{4}")) < year - 100 |
              as.numeric(
                str_extract(birth_date, "(?<=\\/)[0-9]{4}")) > year - 0) |>
          mutate(error = "birth_date"),
        # Hunting migratory birds should only be = 1 or 2
        keyed_data |>
          filter(!hunt_mig_birds %in% REF_HUNT_MIG_BIRDS) |>
          mutate(error = "hunt_mig_birds"),
        # Registration year should = survey year
        keyed_data |>
          filter(registration_yr != year) |>
          mutate(error = "registration_yr"),
        # Email
        keyed_data |>
          proofBadEmails() |>
          mutate(error = "email")
      ) |>
      select(temp_key, error)

    graded_x <-
      keyed_data |>
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

#' Proof bad emails
#'
#' The internal \code{proofBadEmails} function is used inside of \code{\link{proof}} to find poorly formatted or intentionally obfuscative email addresses.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @param keyed_data An tibble used internally in \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

proofBadEmails <-
  function(keyed_data) {

    # Filter data to email addresses that do not meet expectations
    keyed_data |>
      mutate(email = tolower(email)) |>
      filter(
        !str_detect(email, REGEX_EMAIL) |
          # Obfuscative emails
          str_detect(email, REGEX_EMAIL_OBFUSCATIVE_LOCALPART) |
          str_detect(email, "\\@none") |
          str_detect(email, "\\@(no\\.com|na\\.org)$") |
          str_detect(email, "\\@(tpw|twp)") |
          # Invalid domain
          str_detect(email, "\\@example.com$") |
          # Burner domain
          str_detect(email, "\\@guerillamail") |
          # Longer than 100 characters (max length of a valid address is 254 but
          # this would be very rare; we only accept 100)
          str_length(email) > 100 |
          # Multiple sequential dots
          str_detect(email, "\\.\\.+") |
          # Dot in the place of the first character
          str_detect(email, "^\\.") |
          # Dot in the place of last character in local part
          str_detect(email, "\\.(?=\\@)") |
          # Dot is last character
          str_detect(email, "\\.$") |
          # Hyphen in first place of domain
          str_detect(email, "(?<=\\@)\\-") |
          # Bad top level domain
          str_detect(email, "(?<=\\@)gmail\\.(co|net|edu|org)$") |
          str_detect(email, "(?<=\\@)(att|comcast)\\.(com|org)$") |
          str_detect(email, "(?<=\\@)icloud\\.(net|org)$") |
          str_detect(email, "(?<=\\.)(com(\\.com)+|com(com)+|con|ccom|coom|comm|c0m|ocm|cm|om|cim|common)$") |
          # Missing top level domain endings
          str_detect(email, "(?<=\\@)(gmail|yahoo|hotmail|aol|icloud|comcast|outlook|sbcglobal|att|msn|live|bellsouth|charter|ymail|me|verizon|cox|earthlink|protonmail|pm|duck|ducks|mail)$") |
          # Missing top level domain periods
          str_detect(email, "(?<!\\.)(com|net|edu|gov|org|navymil|usnavymil|usafmil|mailmil|armymil|usarmymil|usacearmymil)$")
      )
  }

