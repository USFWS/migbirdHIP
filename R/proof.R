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
        # Title should only be NA, 0, 1, or 2
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
                # Should not be "blank" or "inaudible"
                str_detect(
                  firstname, "^[INAUDIBLE|BLANK|TEST|USER|RESIDENT]$") ~
                  "firstname",
                TRUE ~ NA_character_)),
        # Middle name should only be 1 letter of the alphabet
        keyed_data |>
          filter(!str_detect(middle, "^[A-Z]{1}$")) |>
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
                # Should not be "inaudible"
                str_detect(lastname, "^INAUDIBLE$") ~ "lastname",
                TRUE ~ NA_character_)),
        # Suffix
        # Allows 1-20 in Roman numerals and numeric, excluding XVIII since the
        # limit is 4 characters)
        keyed_data |>
          filter(!suffix %in% REF_SUFFIXES) |>
          mutate(error = "suffix"),
        # Address does not contain |, tab or non-UTF8 characters
        # Any further address verification isn't really possible
        keyed_data |>
          filter(str_detect(address, "\\||\\t|[^\\x00-\\x7F]+")) |>
          mutate(error = "address"),
        # City should only contain letters, spaces (e.g. New York City), hyphens
        # (e.g. Winston-Salem, NC), or apostrophes (e.g. HI residents and
        # O'Fallon, MO)
        keyed_data |>
          filter(str_detect(city, "[^A-Za-z\\s\\-\\']")) |>
          mutate(error = "city"),
        # City MUST contain at least 3 letters
        keyed_data |>
          filter(str_detect(city, "^[A-Za-z]{1,2}$")) |>
          mutate(error = "city"),
        # State should only contain a specific list of states/provinces/etc
        keyed_data |>
          filter(!str_detect(state, REGEX_USA_CANADA)) |>
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
          filter(!hunt_mig_birds %in% c("1", "2")) |>
          mutate(error = "hunt_mig_birds"),
        # Registration year should = survey year
        keyed_data |>
          filter(registration_yr != year) |>
          mutate(error = "registration_yr"),
        # Email
        keyed_data |>
          mutate(email = tolower(email)) |>
          filter(
            # If email doesn't fit a loose validation regular expression, mark
            # as error. Local part may contain Latin lower and uppercase
            # letters, numbers, underscores, dots, hyphens, and a plus sign
            # (consecutive dots, leading dots, etc all handled in correct
            # function even if not marked as error); must contain @; domain may
            # contain Latin lower and uppercase letters, numbers, and hyphens;
            # subdomains acceptable when separated by a dot.
            !str_detect(
              email,
              "^[a-zA-Z0-9\\_\\.\\+\\-]+\\@[a-zA-Z0-9\\-]+\\.[a-zA-Z0-9\\-\\.]+$"
              ) |
              # If email is obfuscative
              str_detect(
                email,
                "^(none|no|na|not|non|www\\.none|nomail|noemail|noreply|customer|unknown|notprovided)\\@"
                ) |
              str_detect(email, "\\@none") |
              str_detect(email, "\\@(no\\.com|na\\.org)$") |
              # Obfuscative Texas emails from @tpw or @tpwd
              str_detect(email, "\\@(tpw|twp)") |
              # If domain is invalid
              str_detect(email, "\\@example.com$") |
              # If longer than 100 characters (max length of valid address is
              # 254 but this would be very rare)
              str_length(email) > 100 |
              # If there are multiple .
              str_detect(email, "\\.\\.+") |
              # If there is a dot in the place of the first character
              str_detect(email, "^\\.") |
              # If there is a dot in the place of last character in local part
              str_detect(email, "\\.(?=\\@)") |
              # If dot is last character
              str_detect(email, "\\.$") |
              # Hyphen in first place of domain
              str_detect(email, "(?<=\\@)\\-") |
              # Bad top level domain
              str_detect(email, "(?<=\\@)gmail\\.(co|net|edu|org)$") |
              str_detect(email, "(?<=\\@)att\\.(com|org)$") |
              str_detect(email, "(?<=\\@)comcast\\.(com|org)$") |
              str_detect(email, "(?<=\\@)icloud\\.(net|org)$") |
              str_detect(
                email,
                "(?<=\\.)(com(\\.com)+|com(com)+|con|ccom|coom|comm|c0m|ocm|cm|om|cim|common)$"
              )
            ) |>
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
