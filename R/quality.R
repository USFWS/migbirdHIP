#' Run checks for common or catastrophic data quality issues
#'
#' The \code{qualityCheck} function reports data quality issues to the user.
#'
#' @param raw_data The product of \code{\link{read_hip}}
#' @param year The year in which the Harvest Information Program data were
#'   collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

qualityCheck <-
  function(raw_data, year) {
    failYear(year)
    questionYear(year)

    # Return messages
    qualityMessages(raw_data, year)
  }

#' Return messages to console for common or catastrophic data quality issues
#'
#' The internal \code{qualityMessages} function is used inside of
#' \code{\link{qualityCheck}} to return messages for missing PII, missing email
#' addresses, all-zero bag records, non-numeric bag values, NAs in dl_state, NAs
#' in dl_date, bad title assignments, high proportions of non-resident hunters
#' in a file, inter-state duplicates, low range of issue dates in a file, and
#' low range of birth dates in a file.
#'
#' @importFrom dplyr n
#' @importFrom dplyr mutate
#'
#' @param raw_data The product of \code{\link{read_hip}}
#' @param year The year in which the Harvest Information Program data were
#'   collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qualityMessages <-
  function(raw_data, year) {
    failYear(year)
    questionYear(year)

    raw2 <- raw_data |> mutate(file_size = n(), .by = "source_file")

    # Check field values
    qTitle(raw2)
    qFirstName(raw2)
    qMiddle(raw2)
    qLastName(raw2)
    qSuffix(raw2)
    qAddress(raw2)
    qCity(raw2)
    qState(raw2)
    qZIP(raw2)
    qBirthDate(raw2, year)
    qHuntMigBirds(raw_data)
    qRegistrationYear(raw_data)
    qDLstate(raw_data)
    qDLdate(raw_data)

    # Check bag value quality:
    # Uses helpers zeroBagsMesage(), naBagsMessage(), and nonDigitBagsMessage()
    qBags(raw_data)

    # Check record quality
    testRecordMessage(raw_data)

    # Check permit quality
    # Return a message if in-line permit does not have hunt_mig_birds == 2
    inLinePermitDNHMessage(raw_data)
    # Return a message if permit file state/species bag values are not 0
    permitFileBagsMessage(raw_data)

    # Return a message for records with blank or NA values in firstname,
    # lastname, state, or birth date
    missingPIIMessage(raw_data)

    # Return a message if all emails are missing from a file
    missingEmailsMessage(raw_data)

    # Return a message if a large proportion of registrations in a file have an
    # address in another state
    nonResidentMessage(raw_data)

    # Return a message if duplicates are found across states
    interStateDuplicatesMessage(raw_data)

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
#' @inheritParams clean
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

#' Return message if all emails are missing or are exactly the same in a file
#' with more than 10 registrations
#'
#' The internal \code{missingEmailsMessage} function is used inside of
#' \code{\link{qualityMessages}}
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

missingEmailsMessage <-
  function(raw_data) {

    # Return a message if all emails are missing, or exactly the same, from a
    # file with more than 10 registrations
    missing <-
      raw_data |>
      summarize(
        file_size = n(),
        n_emails = length(unique(.data$email)),
        .by = "source_file") |>
      filter(.data$file_size > 10 & .data$n_emails == 1)

    if (nrow(missing) > 0) {
      message(
        paste("Error:", nrow(missing), "files are missing 100% of emails.")
      )

      print(missing |> select("source_file"))
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
#' @inheritParams clean
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

#' In-line permit did-not-hunt message
#'
#' The internal \code{inLinePermitDNHMessage} function returns a message for
#' in-line permit records from OR or WA that indicate they did not hunt.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom rlang .data
#'
#' @inheritParams clean
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
#' @inheritParams clean
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


#' High non-resident hunter proportion message
#'
#' The internal \code{nonResidentMessage} function returns a message for files
#' with 10% or more of \code{state} values that do not match \code{dl_state}.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr distinct
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

nonResidentMessage <-
  function(raw_data) {

    # States with 10% or more nonresident registrations
    nonresidents <-
      raw_data |>
      mutate(state_total = n(), .by = "dl_state") |>
      mutate(
        nonresident_total = n(),
        nonresident_total_prop = .data$nonresident_total / .data$state_total,
        .by = c("source_file", "dl_state", "state")) |>
      distinct(
        .data$source_file, .data$state, .data$dl_state, .data$nonresident_total,
        .data$state_total, .data$nonresident_total_prop) |>
      arrange(desc(.data$nonresident_total)) |>
      filter(.data$dl_state != .data$state) |>
      summarize(
        nonresident_total_sum = sum(.data$nonresident_total),
        state_total = unique(.data$state_total),
        nonresident_prop_overall =
          .data$nonresident_total_sum / .data$state_total,
        contributing =
          paste0(
            .data$state,
            " (",
            round(.data$nonresident_total_prop, 3) * 100, "%)",
            collapse = ", "),
        .by = "source_file"
      ) |>
      # Only return states with 10% or more non-resident registrations
      filter(.data$nonresident_prop_overall >= 0.1)

    if (nrow(nonresidents) > 0) {
      message(paste("Error: High non-resident proportions."))

      nonresidents |>
        mutate(
          nonresident_prop =
            paste0(round(.data$nonresident_prop_overall, 3) * 100, "%"),
          contributing =
            ifelse(
              # Condition: If there are 3 or more commas
              str_count(.data$contributing, ",") >= 3,
              str_replace(
                .data$contributing, "^((?:[^,]*,){2}[^,]*),.*", "\\1"),
              .data$contributing
            )
        ) |>
        arrange(desc(.data$nonresident_prop_overall)) |>
        select(
          "source_file",
          nonresident_n = "nonresident_total_sum",
          "nonresident_prop",
          "contributing") |>
        print()
    }

  }

#' Inter-state duplicates message
#'
#' The internal \code{interStateDuplicatesMessage} function returns a message
#' for duplicates found among files submitted from different states.
#'
#' @importFrom dplyr count
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom dplyr mutate
#' @importFrom dplyr cur_group_id
#' @importFrom dplyr distinct
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom stringr str_extract
#' @importFrom dplyr filter_out
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr as_tibble
#' @importFrom rlang .data
#'
#' @param raw_data The product of \code{\link{read_hip}}
#' @param n_threshold A whole number; the threshold below which inter-state
#'   duplicates will not be reported, unless \code{p_threshold} is exceeded.
#'   Defaults to n = 100 inter-state duplicates.
#' @param p_threshold A number ranging from 0 to 1; the threshold below which
#'   inter-state duplicates will not be reported, unless \code{n_threshold} is
#'   exceeded. Defaults to 0.05, or inter-state duplicates exceeding more than
#'   5% of the number of submitted registrations by the first download state OR
#'   the second download state.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

interStateDuplicatesMessage <-
  function(raw_data, n_threshold = 100, p_threshold = 0.05) {

    raw_data <- lazy_dt(raw_data)

    dlst_big_n <-
      raw_data |>
      count(.data$dl_state, name = "dlst_size") |>
      as_tibble()

    interstateduplicates <-
      raw_data |>
      # Select relevant fields
      select("firstname", "lastname", "birth_date", "state", "dl_state") |>
      # Filter to duplicate registrations using first name, last name, address
      # state, and birth date fields
      filter(
        n() > 1, .by = c("firstname", "lastname", "state", "birth_date")) |>
      # Label groups of duplicates using first name, last name, address state,
      # and birth date
      mutate(
        dup_id = cur_group_id(),
        .by = c("firstname", "lastname", "state", "birth_date")) |>
      # Distinct to remove duplicates submitted in the same file
      distinct() |>
      # Filter to return registrations for the same person in 2+ different
      # dl_states
      filter(
        length(unique(.data$dl_state)) > 1,
        .by = c("firstname", "lastname", "state", "birth_date")) |>
      # Paste the dl_state values together in a new field
      summarize(
        dl_states = paste0(.data$dl_state, collapse = " & "),
        .by = "dup_id") |>
      # Count the new field values to report inter-state duplicates
      count(.data$dl_states) |>
      arrange(desc(.data$n))

    isd_summary <-
      interstateduplicates |>
      mutate(
        st1 = str_extract(.data$dl_states, "^[A-Z]{2}"),
        st2 = str_extract(.data$dl_states, "(?<=[A-Z]{2}\\s\\&\\s)[A-Z]{2}"),
        prop_1 =
          round(
            .data$n / dlst_big_n$dlst_size[dlst_big_n$dl_state == .data$st1],
            3),
        prop_2 =
          round(
            .data$n / dlst_big_n$dlst_size[dlst_big_n$dl_state == .data$st2],
            3),
        .by = "dl_states"
      ) |>
      select(-c("st1", "st2")) |>
      filter(!(
        .data$n < n_threshold &
          .data$prop_1 < p_threshold &
          .data$prop_2 < p_threshold)
      ) |>
      as_tibble()

    if (nrow(isd_summary) > 0) {
      message(paste("Error: Inter-state duplicates detected."))
      print(isd_summary)
    }
  }

#' Quality summary
#'
#' The internal \code{qSummary} function ...
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param bad_data Must contain file_size field
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qSummary <-
  function(bad_data) {

    bad_data |>
      summarize(n_bad = n(), .by = c("source_file", "file_size")) |>
      mutate(prop_bad = round(.data$n_bad / .data$file_size, 3)) |>
      arrange(desc(.data$prop_bad)) |>
      filter(.data$prop_bad > 0.01 | .data$n_bad > 100)
  }

#' Bad title message
#'
#' The internal \code{qTitle} function returns a message for \code{title} values
#' that are not assigned to the correct registrations. Uses internal helper
#' \code{\link{getBadTitle}}.
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qTitle <-
  function(raw_data) {

    q <-
      getBadTitle(raw_data) |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad title values detected.")
      print(q)
    }

  }


#' Bad first name message
#'
#' The internal \code{qFirstName} function returns a message for bad
#' \code{firstname} values. Uses internal helper \code{\link{getBadFirstName}}.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_upper
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qFirstName <-
  function(raw_data) {

    q <-
      raw_data |>
      mutate(firstname = str_to_upper(.data$firstname)) |>
      getBadFirstName() |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad first name values detected.")
      print(q)
    }

  }

#' Bad middle initial message
#'
#' The internal \code{qMiddle} function returns a message for bad \code{middle}
#' values. Uses internal helper \code{\link{getBadMiddle}}.
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qMiddle <-
  function(raw_data) {

    q <-
      getBadMiddle(raw_data) |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad middle initial values detected.")
      print(q)
    }

  }

#' Bad last name message
#'
#' The internal \code{qLastName} function returns a message for bad
#' \code{lastname} values. Uses internal helper \code{\link{getBadLastName}}.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_upper
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qLastName <-
  function(raw_data) {

    q <-
      raw_data |>
      mutate(lastname = str_to_upper(.data$lastname)) |>
      getBadLastName() |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad last name values detected.")
      print(q)
    }

  }

#' Bad suffix message
#'
#' The internal \code{qSuffix} function returns a message for bad \code{suffix}
#' values. Uses internal helper \code{\link{getBadSuffix}}.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_upper
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qSuffix <-
  function(raw_data) {

    q <-
      raw_data |>
      mutate(suffix = str_to_upper(.data$suffix)) |>
      getBadSuffix() |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad suffix values detected.")
      print(q)
    }

  }

#' Bad address message
#'
#' The internal \code{qAddress} function returns a message for bad
#' \code{address} values. Uses internal helper \code{\link{getBadAddress}}.
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qAddress <-
  function(raw_data) {

    q <-
      getBadAddress(raw_data) |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad address values detected.")
      print(q)
    }

  }

#' Bad city message
#'
#' The internal \code{qCity} function returns a message for bad \code{city}
#' values. Uses internal helper \code{\link{getBadCity}}.
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qCity <-
  function(raw_data) {

    q <-
      getBadCity(raw_data) |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad city values detected.")
      print(q)
    }

  }

#' Bad state message
#'
#' The internal \code{qState} function returns a message for bad \code{state}
#' values. Uses internal helper \code{\link{getBadState}}.
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qState <-
  function(raw_data) {

    q <-
      getBadState(raw_data) |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad state values detected.")
      print(q)
    }

  }

#' Bad zip code message
#'
#' The internal \code{qZIP} function returns a message for bad \code{zip}
#' values. Uses internal helper \code{\link{getBadZIP}}.
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qZIP <-
  function(raw_data) {

    q <-
      getBadZIP(raw_data) |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad zip code values detected.")
      print(q)
    }

  }

#' Bad birth dates message
#'
#' The internal \code{qBirthDate} function returns a message for
#' \code{birth_date} values that are incorrectly formatted or improbable. Uses
#' internal helper \code{\link{getBadBirthDate}}.
#'
#' @inheritParams qualityMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qBirthDate <-
  function(raw_data, year) {
    failYear(year)
    questionYear(year)

    q <-
      getBadBirthDate(raw_data, year) |>
      qSummary()

    if (nrow(q) > 0) {
      message("Bad birth date values detected.")
      print(q)
    }

  }

#' Return message if records with bad values for hunt_mig_birds are detected
#'
#' The internal \code{qHuntMigBirds} function is used inside of
#' \code{\link{qualityMessages}}.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qHuntMigBirds <-
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

#' Bad bag messages
#'
#' The internal \code{qBags} function returns a messages for bad bag values,
#' using internal helper functions \code{\link{zeroBagsMessage}},
#' \code{\link{naBagsMessage}}, and \code{\link{nonDigitBagsMessage}}.
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qBags <-
  function(raw_data) {

    # Return a message if any record contains all-zero bag values
    zeroBagsMessage(raw_data)

    # Return a message if any record contains all-NA bag values
    naBagsMessage(raw_data)

    # Return a message if any record contains a bag value that is not a
    # 1-digit number
    nonDigitBagsMessage(raw_data)

  }

#' Return message if any record has a "0" in every bag field
#'
#' The internal \code{zeroBagsMessage} function is used inside of
#' \code{\link{qualityMessages}}.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#'
#' @inheritParams clean
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
#' \code{\link{qualityMessages}}.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#'
#' @inheritParams clean
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
#' \code{\link{qualityMessages}}.
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
#' @inheritParams clean
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

#' Bad registration_yr message
#'
#' The internal \code{qRegistrationYear} function returns a message for
#' \code{registration_yr} values that are not equal to REF_CURRENT_SEASON or
#' REF_CURRENT_SEASON + 1.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

qRegistrationYear <-
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
