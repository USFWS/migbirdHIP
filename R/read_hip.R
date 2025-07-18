#' Read in data
#'
#' Compile data from state-exported text files by providing a path to the
#' download directory.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr rename
#' @importFrom dplyr cur_group_id
#' @importFrom dplyr row_number
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom readr read_fwf
#' @importFrom readr fwf_widths
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#' @importFrom assertthat assert_that
#'
#' @param path File path to the folder containing HIP .txt files
#' @param unique Return a distinct frame? Defaults to TRUE
#' @param state When specified, reads in download data from a specified state.
#'   Must match a two-letter abbreviation for a US state (excluding HI).
#' @param season If set as TRUE, selects only folders starting with "DL" in a a
#'   season's upper-level directory
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

read_hip <-
  function(path, unique = TRUE, state = NA, season = FALSE) {
    failTF(unique)
    failTF(season)

    # Add a final "/" if not included already
    if (!str_detect(path, "\\/$")) {
      path <- paste0(path, "/")
    }

    # Error for possibly wrong path
    if (str_detect(path, "DL") & season == TRUE) {
      message("Are you sure you supplied a season path?")
    }

    # Fail if incorrect state supplied
    assert_that(
      state %in% c(NA, REF_ABBR_49_STATES),
      msg =
        paste0(
          "`state` one of acceptable state abbreviations: ",
          REF_ABBR_49_STATES, " or `NA`.")
    )

    # Create a tibble of the HIP .txt files to be read from the provided
    # directory
    file_list <-
      listFiles(path, season) |>
      # Don't process permit files
      ignorePermits() |>
      # Don't process hold files
      ignoreHolds() |>
      # Don't process lifetime files
      ignoreLifetime() |>
      # Identify blank files
      idBlankFiles() |>
      # Drop blank files
      dropBlankFiles()

    # Filter files to include only specified state (state param NA by default)
    if (!is.na(state)) {
      file_list <- filter(file_list, str_detect(.data$filepath, state))
    }

    # Create a vector of file paths
    file_list_vector <- file_list$filepath

    # Stop if there are no files to read in
    stopifnot("No file(s) to read in." = length(file_list_vector) != 0)

    # Stop if any file name date is formatted as MMDDYYYY or DDMMYYYY
    date_test <- checkFileNameDateFormat(file_list_vector)
    stopifnot("Incorrect date format in file name." = is.null(date_test))

    # Stop if any file name contains a state abbreviation not found in the list
    # of 49 continental US states
    state_test <- checkFileNameStateAbbr(file_list_vector)
    stopifnot("Bad state abbreviation in file name." = is.null(state_test))

    # Read in HIP data
    raw_data <-
      map(
        seq_along(file_list_vector),
        function(i) {
          # Compile each state's file into one table
          read_fwf(
            file_list_vector[i],
            fwf_widths(c(1, 15, 1, 20, 3, 60, 20, 2, 10, 10, 10,
                         1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, NA)),
            col_types = "cccccccccccccccccccccccc",
            na = c("N/A", "")) |>
            mutate(
              # Add the download state as a column
              dl_state =
                str_extract(
                  file_list_vector[i], "[A-Z]{2}(?=[0-9]{8}\\.txt)"),
              # Add the download date as a column
              dl_date =
                str_extract(
                  file_list_vector[i], "(?<=[A-Z]{2})[0-9]{8}(?=\\.txt)"),
              # Add the source file as a column
              source_file =
                str_remove(file_list_vector[i], path),
              # Add the download cycle as a column
              dl_cycle =
                str_extract(file_list_vector[i], "(?<=DL).+(?=\\/)"))
        }) |>
      # Row bind data from each file into one tibble
      list_rbind() |>
      # Rename columns
      rename(
        title = 1,
        firstname = 2,
        middle = 3,
        lastname = 4,
        suffix = 5,
        address = 6,
        city = 7,
        state = 8,
        zip = 9,
        birth_date = 10,
        issue_date = 11,
        hunt_mig_birds = 12,
        ducks_bag = 13,
        geese_bag = 14,
        dove_bag = 15,
        woodcock_bag = 16,
        coots_snipe = 17,
        rails_gallinules = 18,
        cranes = 19,
        band_tailed_pigeon = 20,
        brant = 21,
        seaducks = 22,
        registration_yr = 23,
        email = 24) |>
      # Add a download key
      mutate(
        dl_key =
          paste0("dl_", cur_group_id()), .by = c("dl_date", "dl_state"))

    # Remove exact duplicates
    if (unique == TRUE) {
      raw_data <-
        raw_data |>
        distinct() |>
        # Add a record key
        mutate(record_key = paste0("record_", row_number()))
    }

    # Return messages to console for important issues
    readMessages(raw_data)

    return(raw_data)
  }

#' List files
#'
#' The internal \code{listFiles} function is used inside of
#' \code{\link{read_hip}} and creates a tibble of the HIP .txt files to be read
#' in from the provided directory.
#'
#' @importFrom dplyr tibble
#'
#' @param path File path to the folder containing HIP .txt files
#' @param season If set as TRUE, selects only folders starting with "DL" in a a
#'   season's upper-level directory
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

listFiles <-
  function(path, season) {

    # Create a tibble of the HIP .txt files to be read from the provided
    # directory
    tibble(
      filepath =
        list.files(
          path,
          recursive = {{season}},
          pattern = "*\\.txt$",
          ignore.case = TRUE,
          full.names = TRUE)
    )
  }

#' Ignore permit files
#'
#' The internal \code{ignorePermits} function is used inside of
#' \code{\link{read_hip}} to filter out permit files from the file list.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#' @param filelist The file list tibble created by \code{\link{listFiles}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

ignorePermits <-
  function(filelist) {

    # Don't process permit files
    filelist |>
      filter(!str_detect(.data$filepath, "permit"))
  }

#' Ignore hold files
#'
#' The internal \code{ignoreHolds} function is used inside of
#' \code{\link{read_hip}} to filter out hold files from the file list.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#' @param filelist The file list tibble created by \code{\link{listFiles}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

ignoreHolds <-
  function(filelist) {

    # Don't process hold files
    filelist |>
      filter(!str_detect(.data$filepath, "hold"))
  }

#' Ignore lifetime files
#'
#' The internal \code{ignoreLifetime} function is used inside of
#' \code{\link{read_hip}} to filter out lifetime files from the file list.
#'
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @param filelist The file list tibble created by \code{\link{listFiles}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

ignoreLifetime <-
  function(filelist) {
    # Don't process lifetime files
    filelist |>
      filter(!str_detect(.data$filepath, "lifetime"))
  }

#' Identify blank files
#'
#' The internal \code{idBlankFiles} function is used inside of
#' \code{\link{read_hip}} to identify files in the list that contain no data.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#'
#' @param filelist The file list tibble created by \code{\link{listFiles}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

idBlankFiles <-
  function(filelist) {

    # Identify blank files
    filelist |>
      mutate(
        filepath = str_replace(.data$filepath, "TXT", "txt"),
        check = ifelse(file.size(.data$filepath) == 0, "blank", ""))
  }

#' Drop blank files
#'
#' The internal \code{dropBlankFiles} function is used inside of
#' \code{\link{read_hip}} to return an error message if blank files exist in the
#' directory, and remove them from the file list so they are not read in.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom rlang .data
#'
#' @param filelist The file list tibble created by \code{\link{listFiles}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

dropBlankFiles <-
  function(filelist) {

    # Error for blank files
    if ("blank" %in% filelist$check) {
      message("Error: One or more files are blank in the directory.")
      print(filter(filelist, .data$check == "blank"))
    }

    # Filter out blank files from the paths list
    filelist_without_blanks <-
      filelist |>
      filter(.data$check != "blank")

    return(filelist_without_blanks)
  }

#' Check HIP file name date formatting
#'
#' The internal \code{checkFileNameDateFormat} function is used inside of
#' \code{\link{read_hip}} to return an error message if any file does not have a
#' date formatted as YYYYMMDD.
#'
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#'
#' @param file_list_vector A file list vector
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

checkFileNameDateFormat <-
  function(file_list_vector) {

    # Extract all dates from file names in the file_list_vector
    dl_date_test <-
      str_extract(file_list_vector, "(?<=[A-Z]{2})[0-9]{8}(?=\\.txt)")

    if (FALSE %in% str_detect(dl_date_test, "^202") &
        TRUE %in% str_detect(dl_date_test, "^[0-9]{4}202")) {
      message(
        paste0(
          "Error: MMDDYYYY or DDMMYYYY format suspected in dl_date.",
          " Please fix the source file name(s)."
        )
      )

      bad_dates <- dl_date_test[str_detect(dl_date_test, "^[0-9]{4}202") &
                                  !str_detect(dl_date_test, "^202")]

      for (i in seq_along(bad_dates)) {
        print(
          file_list_vector[str_detect(file_list_vector, bad_dates[i])]
        )
      }
      return("error")
    } else {
      return(NULL)
    }
  }

#' Check HIP file name state abbreviations
#'
#' The internal \code{checkFileNameStateAbbr} function is used inside of
#' \code{\link{read_hip}} to return an error message if any file does not have
#' an state abbreviation from the expected 49 continental states.
#'
#' @importFrom stringr str_extract
#'
#' @param file_list_vector A file list vector
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

checkFileNameStateAbbr <-
  function(file_list_vector) {

    # Extract all state abbreviations from file names in the file_list_vector
    state_test <-
      unique(str_extract(file_list_vector, "[A-Z]{2}(?=[0-9]{8}\\.txt$)"))

    # Return a message if there is a dl_state not found in the list of 49
    # continental US states
    if (FALSE %in%
        (state_test %in% REF_ABBR_49_STATES)
    ) {
      message(
        paste(
          "Error: One or more files contains a state abbreviation not in the",
          "list of expected 49 continental US states."
        )
      )

      print(state_test[!state_test %in% REF_ABBR_49_STATES])

      return("error")
    } else {
      return(NULL)
    }
  }

#' Return messages to console for common or catastrophic read_hip issues
#'
#' The internal \code{readMessages} function is used inside of
#' \code{\link{read_hip}} to return messages for missing PII, missing email
#' addresses, all-zero bag records, non-numeric bag values, NAs in dl_state, and
#' NAs in dl_date.
#'
#' @param raw_data The product of \code{\link{read_hip}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

readMessages <-
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

    # Return a message if there is an NA in dl_state
    dlStateNAMessage(raw_data)

    # Return a message if there is an NA in dl_date
    dlDateNAMessage(raw_data)

    # Return a message if in-line permit does not have hunt_mig_birds == 2
    inLinePermitDNHMessage(raw_data)

    # Return a message for bad registration years
    badRegYearMessage(raw_data)

  }

#' Return message for records with blank or NA values in firstname, lastname,
#' state, or birth date
#'
#' The internal \code{missingPIIMessage} function is used inside of
#' \code{\link{readMessages}}
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr reframe
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @inheritParams readMessages
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
          "Error: NA values detected in one or more ID fields",
          "(firstname, lastname, state, birth date) for >10% of a file",
          "and/or >100 records."
        )
      )

      print(raw_nas)
    }
  }

#' Return message if all emails are missing from a file
#'
#' The internal \code{missingEmailsMessage} function is used inside of
#' \code{\link{readMessages}}
#'
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @inheritParams readMessages
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
      message("Error: One or more files are missing 100% of emails.")

      print(missing |> select(.data$source_file))
    }
  }

#' Return message if test record is found
#'
#' The internal \code{testRecordMessage} function is used inside of
#' \code{\link{readMessages}}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @inheritParams readMessages
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
          "Error: One or more records contain 'TEST' in first name and last",
          "name fields."
        )
      )

      print(
        bad_test_records |>
          select(c("source_file", "record_key", "firstname", "lastname")))
    }
  }

#' Return message if any record has a "0" in every bag field
#'
#' The internal \code{zeroBagsMessage} function is used inside of
#' \code{\link{readMessages}}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#'
#' @inheritParams readMessages
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
          "Error: One or more records has a '0' in every bag field; these",
          "records will be filtered out in clean()."
        )
      )

      print(zero_bags |> select(c("source_file", "record_key")))
    }
  }

#' Return message if any record has an NA in every bag field
#'
#' The internal \code{naBagsMessage} function is used inside of
#' \code{\link{readMessages}}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#'
#' @inheritParams readMessages
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
      message("Error: One or more records has an NA in every bag field.")

      print(NA_bags |> select(c("source_file", "record_key")))
    }
  }

#' Return message if any record contains a bag value that is not a 1-digit
#' number
#'
#' The internal \code{nonDigitBagsMessage} function is used inside of
#' \code{\link{readMessages}}
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
#' @inheritParams readMessages
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
          "Error: One or more records detected with a value other than a",
          "single digit; these records will be filtered out in clean()."
        )
      )
      print(
        nondigit_bags |>
          unite("bags", matches(REF_FIELDS_BAG), sep = " ") |>
          select(c("source_file", "record_key", "bags"))
      )
    }
  }

#' Return message if there is an NA in dl_state
#'
#' The internal \code{dlStateNAMessage} function is used inside of
#' \code{\link{readMessages}}
#'
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @inheritParams readMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

dlStateNAMessage <-
  function(raw_data) {

    # Return a message if there is an NA in dl_state
    if (TRUE %in% is.na(raw_data$dl_state)) {
      message("Error: One or more more NA values detected in dl_state.")

      print(raw_data |>
              distinct(.data$dl_state, .data$source_file) |>
              filter(is.na(.data$dl_state)))
    }
  }

#' Return message if there is an NA in dl_date
#'
#' The internal \code{dlDateNAMessage} function is used inside of
#' \code{\link{readMessages}}
#'
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @inheritParams readMessages
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

dlDateNAMessage <-
  function(raw_data) {

    # Return a message if there is an NA in dl_date
    if (TRUE %in% is.na(raw_data$dl_date)) {
      message("Error: One or more more NA values detected in dl_date.")

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
#' @inheritParams readMessages
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
          "from OR and/or WA do not contain hunt_mig_birds == 2; they will be",
          "edited in clean()."
        )
      )

      print(inline_pmt_dnh)
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
#' @inheritParams readMessages
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
      count(.data$dl_state, .data$registration_yr)

    if (nrow(badyr) > 0) {
      message(
        paste0(
          "Error: ", nrow(badyr), " records do not have a valid ",
          "registration_yr value; the registration_yr must be equal to ",
          REF_CURRENT_SEASON, " or ", as.numeric(REF_CURRENT_SEASON) + 1, "."
        )
      )

      print(badyr)
    }
  }
