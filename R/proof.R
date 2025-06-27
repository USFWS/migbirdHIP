#' Proof data and flag errors
#'
#' After cleaning the data with \code{\link{clean}}, compare each field to an
#' expected range of values and flag non-conforming values in a new "errors"
#' column.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr as_tibble
#' @importFrom dplyr distinct
#' @importFrom dplyr case_when
#' @importFrom dplyr full_join
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
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
          filter(!.data$title %in% REF_TITLES) |>
          mutate(error = "title"),
        # First name
        keyed_data |>
          filter(!str_detect(.data$firstname, REGEX_FIRSTNAME)) |>
          mutate(error = "firstname"),
        # Middle initial
        keyed_data |>
          filter(!.data$middle %in% c(LETTERS, NA)) |>
          mutate(error = "middle"),
        # Last name
        keyed_data |>
          filter(!str_detect(.data$lastname, REGEX_LASTNAME)) |>
          mutate(error = "lastname"),
        # Suffix
        keyed_data |>
          filter(!.data$suffix %in% c(REF_SUFFIXES, NA)) |>
          mutate(error = "suffix"),
        # Address
        keyed_data |>
          filter(str_detect(.data$address, REGEX_BAD_ADDRESS)) |>
          mutate(error = "address"),
        # City
        keyed_data |>
          filter(!str_detect(.data$city, REGEX_CITY)) |>
          mutate(error = "city"),
        # State
        keyed_data |>
          filter(!.data$state %in% REF_USA_CANADA) |>
          mutate(error = "state"),
        # Zip code should be in the reference table
        keyed_data |>
          filter(
            !str_extract(.data$zip, "^[0-9]{5}") %in% REF_ZIP_CODE$zipcode) |>
          mutate(error = "zip"),
        # Birth date should only ever be between 100 and 0 years ago
        keyed_data |>
          filter(
            as.numeric(
              str_extract(.data$birth_date, "(?<=\\/)[0-9]{4}")) < year - 100 |
              as.numeric(
                str_extract(
                  .data$birth_date, "(?<=\\/)[0-9]{4}")) > year - 0) |>
          mutate(error = "birth_date"),
        # hunt_mig_birds
        keyed_data |>
          filter(!.data$hunt_mig_birds %in% REF_HUNT_MIG_BIRDS) |>
          mutate(error = "hunt_mig_birds"),
        # registration_yr should = survey year
        keyed_data |>
          filter(.data$registration_yr != year) |>
          mutate(error = "registration_yr"),
        # Email
        keyed_data |>
          proofBadEmails() |>
          mutate(error = "email")
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
            .data$state != .data$zipState & is.na(.data$errors) ~
              "zip",
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
#' email addresses.
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
        !str_detect(.data$email, REGEX_EMAIL) |
          # Obfuscative emails
          str_detect(.data$email, REGEX_EMAIL_OBFUSCATIVE_LOCALPART) |
          str_detect(.data$email, REGEX_EMAIL_OBFUSCATIVE_DOMAIN) |
          str_detect(.data$email, REGEX_EMAIL_REPEATED_CHAR) |
          str_detect(.data$email, REGEX_EMAIL_OBFUSCATIVE_TPWD) |
          str_detect(.data$email, REGEX_EMAIL_OBFUSCATIVE_WALMART) |
          .data$email %in% REF_EMAIL_OBFUSCATIVE_ADDRESS |
          # Longer than 100 characters (max length of a valid address is 254 but
          # this would be very rare; we only accept 100)
          str_length(.data$email) > 100 |
          # Domain typo
          str_detect(
            .data$email,
            paste0(
              "(?<=\\@)(gmaill|gmai|gamil|gmial|gmal|gmil|gail|gmali|gmall|gla",
              "il|gmaim|gamil|gimal|gmai|gmaii|gnail)\\.com$")) |
          str_detect(
            .data$email,
            paste0(
              "(?<=\\@)(yahooo+|ahoo|yhoo|yaho|yahoh|yahohh|yyahoo|ayahoo)\\.c",
              "om$")) |
          str_detect(.data$email, "(?<=\\@)(attt+|at|aatt)\\.net$") |
          str_detect(.data$email, "(?<=\\@)(iclould|icoud|icould)\\.com$") |
          str_detect(.data$email, "(?<=\\@)(hatmail)\\.com$") |
          str_detect(
            .data$email,
            paste0(
              "(?<=\\@)(sbcgobal|sbcglobel|sbcgloble|sbcgolbal|sbcglobe|sbcglo",
              "bl|sbcgloabl|sbcgloabal|sbcgloal|sbcgobel|sbcglbal|sbcglob|sbcg",
              "oble|sbclobal|sbc\\.gobal|sbcglabal|sbcglibal|sbcgllobal|sbcglo",
              "ba|sbcglobale|sbcglobol|sbcglobsl|spcglobal)\\.net$")) |
          str_detect(
            .data$email,
            paste0(
              "(?<=\\@)(concast|commcast|comacast|cmcast|compcast|conmcast|c0m",
              "cast|comcst|comacst)\\.net$")) |
          # Popular domain doesn't have matching top level domain
          str_detect(.data$email, "(?<=\\@)gmail(?!\\.com$)") |
          str_detect(
            .data$email, "(?<=\\@)yahoo\\.(?!(com|co\\.uk|fr|es|ca|de)$)") |
          str_detect(
            .data$email, "(?<=\\@)hotmail\\.(?!(com|co\\.uk|fr|es|ca|de)$)") |
          str_detect(
            .data$email,
            "(?<=\\@)(icloud|aol|outlook|msn|live|ymail|me|mac)\\.(?!com$)") |
          str_detect(
            .data$email,
            paste0(
              "(?<=\\@)(att|cox|comcast|sbcglobal|bellsouth|verizon|earthlink|",
              "charter)\\.(?!net$)")) |
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
              "(?<=\\@)(gmail|yahoo|hotmail|aol|icloud|comcast|outlook|sbcglob",
              "al|att|msn|live|bellsouth|charter|ymail|me|verizon|cox|earthlin",
              "k|protonmail|pm|duck|ducks|mail)$")) |
          # Missing top level domain periods
          str_detect(
            .data$email,
            paste0(
              "(?<!\\.)(com|net|edu|gov|org|navymil|usnavymil|usafmil|mailmil|",
              "armymil|usarmymil|usacearmymil)$"))
      ) |>
      select(-"tld")
  }
