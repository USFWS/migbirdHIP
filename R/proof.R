#' Proof data and flag errors
#'
#' After cleaning the data with \code{\link{clean}}, compare each field to an
#' expected range of values and flag non-conforming values in a new "errors"
#' column.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr as_tibble
#' @importFrom dplyr distinct
#' @importFrom dplyr case_when
#' @importFrom dplyr full_join
#' @importFrom dplyr left_join
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom rlang .data
#'
#' @param deduplicated_data The object created after deduplicating data with
#'   \code{\link{duplicateFix}}
#' @param year The year in which the Harvest Information Program data were
#'   collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

proof <-
  function(deduplicated_data, year) {
    failYear(year)
    questionYear(year)

    # Create a record key so that the errors can be joined in later (there may
    # be more than 1 error per record, so using the record_key field will not
    # work)
    keyed_data <-
      deduplicated_data |>
      mutate(temp_key = paste0("row_", row_number()))

    markup <-
      bind_rows(
        # Title
        getBadTitle(keyed_data) |> mutate(error = "title"),
        # First name
        getBadFirstName(keyed_data) |> mutate(error = "firstname"),
        # Middle initial
        getBadMiddle(keyed_data) |> mutate(error = "middle"),
        # Last name
        getBadLastName(keyed_data) |> mutate(error = "lastname"),
        # Suffix
        getBadSuffix(keyed_data) |> mutate(error = "suffix"),
        # Address
        getBadAddress(keyed_data) |> mutate(error = "address"),
        # City
        getBadCity(keyed_data) |> mutate(error = "city"),
        # State
        getBadState(keyed_data) |> mutate(error = "state"),
        # Zip code
        getBadZIP(keyed_data) |> mutate(error = "zip"),
        # Birth date
        getBadBirthDate(keyed_data, year) |> mutate(error = "birth_date"),
        # hunt_mig_birds
        getBadHuntMigBirds(keyed_data) |> mutate(error = "hunt_mig_birds"),
        # Registration year
        getBadRegYear(keyed_data, year) |> mutate(error = "registration_yr"),
        # Email
        proofBadEmails(keyed_data) |> mutate(error = "email")
      ) |>
      select(c("temp_key", "error"))

    graded_x <-
      keyed_data |>
      # Join in the error report
      full_join(markup, by = "temp_key") |>
      # Paste errors together (some records might have more than one!)
      mutate(errors = paste(.data$error, collapse = "-"), .by = "temp_key") |>
      select(-c("temp_key", "error")) |>
      distinct() |>
      # Make the NAs "real NAs" not just strings
      mutate(
        errors =
          ifelse(
            str_detect(.data$errors, "NA\\-|\\-NA"),
            str_remove_all(.data$errors, "NA\\-|\\-NA"),
            .data$errors)) |>
      # Add a second mutate here because we cannot pipe '.'
      mutate(errors =
               ifelse(str_detect(.data$errors, "^NA$"), NA, .data$errors)) |>
      as_tibble()

    # Proof the zip codes. Are they associated with the correct states?
    graded_x <-
      graded_x |>
      left_join(
        REF_ZIP_CODE |>
          distinct(zip = .data$zipcode, zipState = .data$state),
        by = "zip") |>
      # Add an error if the state doesn't match zipState
      mutate(
        errors =
          case_when(
            .data$state != .data$zipState & is.na(.data$errors) ~ "zip",
            .data$state != .data$zipState & !is.na(.data$errors) &
              !str_detect(.data$errors, "zip") ~
              paste0(.data$errors, "-zip"),
            TRUE ~ .data$errors)
      ) |>
      select(-"zipState")

    return(graded_x)

  }

#' Proof bad emails
#'
#' The internal \code{proofBadEmails} function is used inside of
#' \code{\link{proof}} to find poorly formatted or intentionally obfuscative
#' email addresses. Email addresses that match \code{REGEX_EMAIL} or are
#' \code{NA} are considered good emails.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringr str_length
#' @importFrom stringr str_extract
#' @importFrom dplyr select
#'
#' @param keyed_data An tibble used internally in \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

proofBadEmails <-
  function(keyed_data) {

    # Filter data to email addresses that do not meet expectations
    keyed_data |>
      mutate(
        email = tolower(.data$email),
        tld =
          paste0(".", str_extract(.data$email, "(?<=\\.)[a-zA-Z0-9\\-]+$"))) |>
      filter(
        !is.na(.data$email) &
          (!str_detect(.data$email, REGEX_EMAIL) |
             # Obfuscative emails
             str_detect(.data$email, REGEX_EMAIL_OBFUSCATIVE_LOCALPART) |
             str_detect(.data$email, REGEX_EMAIL_OBFUSCATIVE_DOMAIN) |
             str_detect(.data$email, REGEX_EMAIL_REPEATED_CHAR) |
             str_detect(.data$email, REGEX_EMAIL_OBFUSCATIVE_TPWD) |
             str_detect(.data$email, REGEX_EMAIL_OBFUSCATIVE_WALMART) |
             .data$email %in% REF_EMAIL_OBFUSCATIVE_ADDRESS |
             # Longer than 100 characters (max length of a valid address is 254
             # but this would be very rare; we only accept 100)
             str_length(.data$email) > 100 |
             # Domain typo
             str_detect(
               .data$email,
               paste0(
                 "(?<=\\@)(gmaill|gmai|gamil|gmial|gmal|gmil|gail|gmali|gmall|",
                 "glail|gmaim|gamil|gimal|gmai|gmaii|gnail)\\.com$")) |
             str_detect(
               .data$email,
               paste0(
                 "(?<=\\@)(yahooo+|ahoo|yhoo|yaho|yahoh|yahohh|yyahoo|ayahoo)",
                 "\\.com$")) |
             str_detect(.data$email, "(?<=\\@)(attt+|at|aatt)\\.net$") |
             str_detect(.data$email, "(?<=\\@)(iclould|icoud|icould)\\.com$") |
             str_detect(.data$email, "(?<=\\@)(hatmail)\\.com$") |
             str_detect(
               .data$email,
               paste0(
                 "(?<=\\@)(sbcgobal|sbcglobel|sbcgloble|sbcgolbal|sbcglobe|sbc",
                 "globl|sbcgloabl|sbcgloabal|sbcgloal|sbcgobel|sbcglbal|sbcglo",
                 "b|sbcgoble|sbclobal|sbc\\.gobal|sbcglabal|sbcglibal|sbcgllob",
                 "al|sbcgloba|sbcglobale|sbcglobol|sbcglobsl|spcglobal)\\.net$")
               ) |
             str_detect(
               .data$email,
               paste0(
                 "(?<=\\@)(concast|commcast|comacast|cmcast|compcast|conmcast|",
                 "c0mcast|comcst|comacst)\\.net$")) |
             # Popular domain doesn't have matching top level domain
             str_detect(.data$email, "(?<=\\@)gmail(?!\\.com$)") |
             str_detect(
               .data$email, "(?<=\\@)(yahoo|hotmail|live|outlook)\\.gov$") |
             str_detect(
               .data$email,
               "(?<=\\@)(icloud|aol|msn|ymail|me|mac)\\.(?!com$)") |
             str_detect(
               .data$email,
               paste0(
                 "(?<=\\@)(att|cox|comcast|sbcglobal|bellsouth|verizon|earthli",
                 "nk|charter)\\.(?!net$)")) |
             str_detect(
               .data$email, "(?<=\\@)proton\\.(?!(me|mail\\.com|mail\\.ch)$)") |
             str_detect(.data$email, "(?<=\\@)protonmail\\.(?!(com|ch)$)") |
             str_detect(.data$email, "(?<=\\@)pm\\.(?!me$)") |
             # Bad top level domain
             str_detect(.data$email, "(?<=\\.)com(\\.com)+$") |
             !.data$tld %in% REF_EMAIL_TLDS |
             # Missing top level domain endings
             str_detect(
               .data$email,
               paste0(
                 "(?<=\\@)(gmail|yahoo|hotmail|aol|icloud|comcast|outlook|sbcg",
                 "lobal|att|msn|live|bellsouth|charter|ymail|me|verizon|cox|ea",
                 "rthlink|protonmail|pm|duck|ducks|mail)$")) |
             # Missing top level domain periods
             str_detect(
               .data$email,
               paste0(
                 "(?<!\\.)(com|net|edu|gov|org|navymil|usnavymil|usafmil|mailm",
                 "il|armymil|usarmymil|usacearmymil)$"))
          )
      ) |>
      select(-"tld")
  }

#' Get bad title values
#'
#' The internal \code{getBadTitle} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{title} values that are not expected. Used by \code{\link{qTitle}}.
#'
#' @importFrom dplyr filter
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadTitle <-
  function(data) {
    data |>
      # Convert first name to upper case
      mutate(firstname = str_to_upper(.data$firstname)) |>
      # Identify bad title values or bad title assignments
      filter(!.data$title %in% REF_TITLES | !!LOGIC_BAD_TITLE_ASSIGNMENT)
  }

#' Get bad first name values
#'
#' The internal \code{getBadFirstName} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{firstname} values that are not expected. Used by
#' \code{\link{qFirstName}}.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadFirstName <-
  function(data) {
    data |>
      filter(!str_detect(.data$firstname, REGEX_FIRSTNAME))
  }

#' Get bad middle initial values
#'
#' The internal \code{getBadMiddle} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{middle} values that are not expected.  Used by \code{\link{qMiddle}}.
#'
#' @importFrom dplyr filter
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadMiddle <-
  function(data) {
    data |>
      filter(!.data$middle %in% c(LETTERS, NA))
  }

#' Get bad last name values
#'
#' The internal \code{getBadLastName} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{lastname} values that are not expected. Used by
#' \code{\link{qLastName}}.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadLastName <-
  function(data) {
    data |>
      filter(!str_detect(.data$lastname, REGEX_LASTNAME))
  }

#' Get bad suffix values
#'
#' The internal \code{getBadSuffix} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{suffix} values that are not expected. Used by \code{\link{qSuffix}}.
#'
#' @importFrom dplyr filter
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadSuffix <-
  function(data) {
    data |>
      filter(!.data$suffix %in% c(REF_SUFFIXES, NA))
  }

#' Get bad address values
#'
#' The internal \code{getBadAddress} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{address} values that are not expected. Used by \code{\link{qAddress}}.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadAddress <-
  function(data) {
    data |>
      filter(str_detect(.data$address, REGEX_BAD_ADDRESS))
  }

#' Get bad city values
#'
#' The internal \code{getBadCity} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{city} values that are not expected. Used by \code{\link{qCity}}.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadCity <-
  function(data) {
    data |>
      filter(!str_detect(.data$city, REGEX_CITY))
  }

#' Get bad state values
#'
#' The internal \code{getBadState} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{state} values that are not expected. Used by \code{\link{qState}}.
#'
#' @importFrom dplyr filter
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadState <-
  function(data) {
    data |>
      filter(!.data$state %in% REF_USA_CANADA)
  }

#' Get bad zip code values
#'
#' The internal \code{getBadZIP} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{zip} values that are not expected. Used by \code{\link{qZIP}}.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_extract
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadZIP <-
  function(data) {
    data |>
      filter(
        !str_extract(.data$zip, "^[0-9]{5}") %in% REF_ZIP_CODE$zipcode)
  }

#' Get bad birth date values
#'
#' The internal \code{getBadBirthDate} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{birth_date} values that are not expected. We expect birth year to be
#' between 0 and 100 years ago. Used by \code{\link{qBirthDate}}.
#'
#' @importFrom lubridate today
#' @importFrom lubridate mdy
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom dplyr select
#'
#' @param data Harvest Information Program registration data
#' @param year The year in which the Harvest Information Program data were
#'   collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadBirthDate <-
  function(data, year) {

    today <- today()

    oldest_date_allowed <-
      mdy(paste0("09/01/", as.numeric(REF_CURRENT_SEASON) - 100))

    data |>
      mutate(bday = mdy(.data$birth_date, quiet = TRUE)) |>
      filter(
        # Correct format
        !str_detect(.data$birth_date, REGEX_DATE_FORMAT) |
        # Birth date too old
        .data$bday < oldest_date_allowed |
          # Date is after today's date
          .data$bday > today |
          # Date does not exist
          is.na(.data$bday)
        ) |>
      select(-"bday")
  }

#' Get bad hunt mig birds values
#'
#' The internal \code{getBadHuntMigBirds} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{hunt_mig_birds} values that are not expected.
#'
#' @importFrom dplyr filter
#'
#' @param data Harvest Information Program registration data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadHuntMigBirds <-
  function(data) {
    data |>
      filter(!.data$hunt_mig_birds %in% REF_HUNT_MIG_BIRDS)
  }

#' Get bad registration year values
#'
#' The internal \code{getBadRegistrationYear} function is used inside of
#' \code{\link{proof}} and \code{\link{qualityMessages}} to filter to
#' \code{registration_yr} values that are not expected. We expect the
#' registration_yr to equal the current hunting season.
#'
#' @importFrom dplyr filter
#'
#' @param data Harvest Information Program registration data
#' @param year The year in which the Harvest Information Program data were
#'   collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

getBadRegYear <-
  function(data, year) {
    data |>
      filter(.data$registration_yr != year)
  }
