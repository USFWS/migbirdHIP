#' Run checks for common or catastrophic data quality issues
#'
#' The \code{qualityCheck} function reports data quality issues to the user.
#'
#' @param raw_data The product of \code{\link{read_hip}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

qualityCheck <-
  function(raw_data) {

    # Return messages
    qualityMessages(raw_data)
  }

#' Return messages to console for common or catastrophic data quality issues
#'
#' The internal \code{qualityMessages} function is used inside of
#' \code{\link{qualityCheck}} to return messages for missing PII, missing email
#' addresses, all-zero bag records, non-numeric bag values, NAs in dl_state, and
#' NAs in dl_date.
#'
#' @param raw_data The product of \code{\link{read_hip}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qualityMessages <-
  function(raw_data) {

    # Return a message for records with blank or NA values in firstname,
    # lastname, state, or birth date
    missingPIIMessage(raw_data)

    # Return a message if all emails are missing from a file
    missingEmailsMessage(raw_data)

    # Return a message if "TEST" is found in firstname or lastname field
    testRecordMessage(raw_data)

    # Return a message if any record contains all-zero bag values
    zeroBagsMessage(raw_data)

    # Return a message if any record contains all-NA bag values
    naBagsMessage(raw_data)

    # Return a message if any record contains a bag value that is not a
    # 1-digit number
    nonDigitBagsMessage(raw_data)

    # Return a message if any record does not have 2 for hunt_mig_birds
    huntMigBirdsMessage(raw_data)

    # Return a message if there is an NA in dl_state
    dlStateNAMessage(raw_data)

    # Return a message if there is an NA in dl_date
    dlDateNAMessage(raw_data)

    # Return a message if in-line permit does not have hunt_mig_birds == 2
    inLinePermitDNHMessage(raw_data)

    # Return a message if permit file state/species bag values are not 0
    permitFileBagsMessage(raw_data)

    # Return a message for bad registration years
    badRegYearMessage(raw_data)

    # -------

    # Return a message for bad title assignments
    badTitlesMessage()

    # Return a message if more than half the registrations of a file have an
    # address in another state
    stateMismatchMessage()

    # Return a message if only one unique issue_date is provided in a file
    repeatedIssueDateMessage()

    # Return a message if the birth_date values in a file do not have a range
    # greater than 1 year
    birthDateMessage()

  }

#' Return message for records with blank or NA values in firstname, lastname,
#' state, or birth date
#'
#' The internal \code{missingPIIMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr reframe
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

missingPIIMessage <-
  function(raw_data) {

    # Return a message for records with blank or NA values in firstname,
    # lastname, state, or birth date
    raw_nas <-
      raw_data |>
      mutate(n_total = n(), by = "dl_state") |>
      filter(
        !!LOGIC_MISSING_PII |
          !!LOGIC_MISSING_ADDRESSES |
          !!LOGIC_MISSING_CITY_ZIP_EMAIL) |>
      group_by(.data$dl_state) |>
      reframe(n = n(), proportion = round(.data$n / .data$n_total, 2)) |>
      distinct() |>
      filter(.data$n >= 100 | .data$proportion >= 0.1)

    if (nrow(raw_nas) > 0) {
      message(
        paste(
          "Error:", nrow(raw_nas), "registrations are missing critical",
          "combinations of PII (making up >10% of a file and/or >100 records)."
        )
      )

      print(raw_nas)
    }
  }

#' Return message if all emails are missing from a file
#'
#' The internal \code{missingEmailsMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

missingEmailsMessage <-
  function(raw_data) {

    # Return a message if all emails are missing from a file
    missing <-
      raw_data |>
      summarize(n_emails = length(unique(.data$email)), .by = "source_file") |>
      filter(.data$n_emails == 1)

    if (nrow(missing) > 0) {
      message(
        paste("Error:", nrow(missing), "files are missing 100% of emails.")
      )

      print(missing |> select(.data$source_file))
    }
  }

#' Return message if test record is found
#'
#' The internal \code{testRecordMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

testRecordMessage <-
  function(raw_data) {

    # Return a message if test records are found
    bad_test_records <-
      raw_data |>
      # Convert firstname, lastname, and suffix to upper case
      namesToUppercase() |>
      # Identify test record through searching first name and last name
      filter(!!LOGIC_TEST_RECORD)

    if (nrow(bad_test_records) > 0) {
      message(
        paste(
          "Error:", nrow(bad_test_records), "test records detected; these",
          "records will be filtered out.")
      )

      print(
        bad_test_records |>
          select(c("source_file", "record_key", "firstname", "lastname")))
    }
  }

#' Return message if any record has a "0" in every bag field
#'
#' The internal \code{zeroBagsMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

zeroBagsMessage <-
  function(raw_data) {

    # Return a message if any record has a "0" in every bag field
    zero_bags <-
      raw_data |>
      # Find any records that have a "0" in every bag field
      filter(!!LOGIC_ZERO_BAGS)

    if (nrow(zero_bags) > 0) {
      message(
        paste(
          "Error:", nrow(zero_bags), "records have a '0' in every bag field;",
          "these records will be filtered out.")
      )

      print(zero_bags |> select(c("source_file", "record_key")))
    }
  }

#' Return message if any record has an NA in every bag field
#'
#' The internal \code{naBagsMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

naBagsMessage <-
  function(raw_data) {

    # Return a message if any record has an NA in every bag field
    NA_bags <-
      raw_data |>
      # Find any records that have an NA in every bag field
      filter(if_all(all_of(REF_FIELDS_BAG), \(x) is.na(x)))

    if (nrow(NA_bags) > 0) {
      message(
        paste(
          "Error:", nrow(NA_bags), "records have an NA in every bag field;",
          "these records will be filtered out.")
      )

      print(NA_bags |> select(c("source_file", "record_key")))
    }
  }

#' Return message if any record contains a bag value that is not a 1-digit
#' number
#'
#' The internal \code{nonDigitBagsMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_any
#' @importFrom dplyr all_of
#' @importFrom stringr str_detect
#' @importFrom tidyr unite
#' @importFrom dplyr matches
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

nonDigitBagsMessage <-
  function(raw_data) {

    # Return a message if any record contains a bag value that is not a 1-digit
    # number
    nondigit_bags <-
      raw_data |>
      filter(!!LOGIC_NONDIGIT_BAGS)

    if (nrow(nondigit_bags) > 0) {
      message(
        paste(
          "Error:", nrow(nondigit_bags), "records detected with a value other",
          "than a single digit; these records will be filtered out.")
      )
      print(
        nondigit_bags |>
          unite("bags", matches(REF_FIELDS_BAG), sep = " ") |>
          select(c("source_file", "record_key", "bags"))
      )
    }
  }

#' Return message if records with bad values for hunt_mig_birds are detected
#'
#' The internal \code{huntMigBirdsMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom rlang .data
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

huntMigBirdsMessage <-
  function(raw_data) {

    # Return a message if records contain a hunt_mig_birds value that isn't 2
    bad_hunty <-
      raw_data |>
      filter(.data$hunt_mig_birds != 2)

    if (nrow(bad_hunty) > 0) {
      message(
        paste(
          "Error:", nrow(bad_hunty), "records detected with a value other than",
          "2 for hunt_mig_birds.")
      )
      print(bad_hunty |>
              count(.data$source_file, .data$hunt_mig_birds))
    }
  }
#' Return message if there is an NA in dl_state
#'
#' The internal \code{dlStateNAMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

dlStateNAMessage <-
  function(raw_data) {

    NA_dlState <-
      raw_data |>
      filter(is.na(.data$dl_state))

    # Return a message if there is an NA in dl_state
    if (nrow(NA_dlState) > 0) {
      message(
        paste(
          "Error:", nrow(NA_dlState), "NA values detected in dl_state."))

      print(raw_data |>
              distinct(.data$dl_state, .data$source_file) |>
              filter(is.na(.data$dl_state)))
    }
  }

#' Return message if there is an NA in dl_date
#'
#' The internal \code{dlDateNAMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

dlDateNAMessage <-
  function(raw_data) {

    NA_dlDate <-
      raw_data |>
      filter(is.na(.data$dl_date))

    # Return a message if there is an NA in dl_date
    if (nrow(NA_dlDate) > 0) {
      message(
        paste(
          "Error:", nrow(NA_dlDate), "NA values detected in dl_date"))

      print(raw_data |>
              select(.data$dl_date, .data$source_file) |>
              filter(is.na(.data$dl_date)) |>
              distinct())
    }
  }

#' In-line permit did-not-hunt message
#'
#' The internal \code{inLinePermitDNHMessage} function returns a message for
#' in-line permit records from OR or WA that indicate they did not hunt.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom rlang .data
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

inLinePermitDNHMessage <-
  function(raw_data) {

    # If any OR or WA hunt_mig_birds != "2" for presumed solo permit, return a
    # message
    inline_pmt_dnh <-
      raw_data |>
      filter(!is.na(.data$band_tailed_pigeon) &
               !is.na(.data$brant) &
               !is.na(.data$seaducks)) |>
      filter(!!LOGIC_INLINE_PMT_DNH) |>
      count(
        .data$source_file, .data$hunt_mig_birds, .data$band_tailed_pigeon,
        .data$brant, .data$seaducks)

    if (nrow(inline_pmt_dnh) > 0) {
      message(
        paste(
          "Error:", sum(inline_pmt_dnh$n), "in-line permit records",
          "from OR and/or WA do not contain 2 for hunt_mig_birds; they will be",
          "edited."
        )
      )

      print(inline_pmt_dnh)
    }
  }

#' Permit file non-zero bag values message
#'
#' The internal \code{permitFileBagsMessage} function returns a message for
#' records from permit file state/species combinations with non-zero bag values.
#'
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom purrr list_rbind
#' @importFrom rlang .data
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

permitFileBagsMessage <-
  function(raw_data) {

    non_zero <-
      map(
        1:nrow(REF_PMT_FILES),
        \(x) {
          raw_data |>
            filter(.data$dl_state == REF_PMT_FILES$dl_state[x] &
                     !!sym(REF_PMT_FILES$spp[x]) != "0") |>
            count(.data$source_file, !!sym(REF_PMT_FILES$spp[x])) |>
            rename(strata = !!sym(REF_PMT_FILES$spp[x])) |>
            mutate(spp = REF_PMT_FILES$spp[x], .before = "strata")
        }
      ) |>
      list_rbind()

    if (nrow(non_zero) > 0) {
      message(
        paste(
          "Error:", sum(non_zero$n), "records with non-zero bag values for",
          "permit species from permit file states; they will be edited.")
      )

      print(non_zero)
    }
  }

#' Bad registration_yr message
#'
#' The internal \code{badRegYearMessage} function returns a message for
#' \code{registration_yr} values that are not equal to REF_CURRENT_SEASON or
#' REF_CURRENT_SEASON + 1.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom rlang .data
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

badRegYearMessage <-
  function(raw_data) {

    badyr <-
      raw_data |>
      filter(
        !.data$registration_yr %in%
          c(REF_CURRENT_SEASON,
            as.character(as.numeric(REF_CURRENT_SEASON) + 1))) |>
      count(.data$source_file, .data$registration_yr)

    if (nrow(badyr) > 0) {
      message(
        paste0(
          "Error: ", nrow(badyr), " files did not submit a valid ",
          "registration_yr value; the registration_yr must be equal to ",
          REF_CURRENT_SEASON, " or ", as.numeric(REF_CURRENT_SEASON) + 1, "."
        )
      )

      print(badyr)
    }
  }
