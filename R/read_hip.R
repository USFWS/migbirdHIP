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
    starttime <- Sys.time()

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

    # Return message informing run time
    endtime <- Sys.time()
    readTimeMessage(raw_data, starttime, endtime)

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

#' Read time message
#'
#' The internal \code{readTimeMessage} function returns a message to the console
#' informing the user about how long it took to read in the data with
#' \code{\link{read_hip}}.
#'
#' @importFrom lubridate time_length
#'
#' @param raw_data The object created after reading in data with
#'   \code{\link{read_hip}}
#' @param starttime Start time
#' @param endtime End time
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

readTimeMessage <-
  function(raw_data, starttime, endtime) {
    readtime <- time_length(endtime - starttime, unit = "second")

    if(readtime > 60) {
      message(
        paste(
          "Time to read in", length(unique(raw_data$source_file)), "files:",
          as.character(
            round(time_length(endtime - starttime, unit = "minute"), 1)),
          "min")
      )
    } else {
      message(
        paste(
          "Time to read in", length(unique(raw_data$source_file)), "files:",
          as.character(
            round(readtime, 0)),
          "sec")
      )
    }
  }
