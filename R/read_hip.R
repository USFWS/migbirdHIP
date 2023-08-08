#' Read in data
#'
#' Compile data from state-exported text files by providing a path to the download directory.
#'
#' @importFrom tibble as_tibble_col
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
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
#' @importFrom stringr str_replace
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#'
#' @param path File path to the folder containing HIP .txt files
#' @param unique Return a distinct frame? Defaults to TRUE
#' @param state When specified, reads in download data from a specified state. Must match one of the following two-letter abbreviations:
#' \itemize{
#' \item AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY}
#' @param season If set as TRUE, selects only folders starting with "DL" in a a season's upper-level directory
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

read_hip <-
  function(path, unique = TRUE, state = NA, season = FALSE) {

    # Add a final "/" if not included already
    if(!str_detect(path, "\\/$")) {
      path <- paste0(path, "/")
    }
    # Error for possibly wrong path
    if(str_detect(path, "DL") & season == TRUE) {
      message("Are you sure you supplied a season path?")
    }
    # Error for bad state
    if(!state %in% c(NA, datasets::state.abb[datasets::state.abb != "HI"])) {
      message(
        paste0(
          "Error: Please supply a valid 2-letter state abbreviation for ",
          "`state` parameter."))
    }
    # Error for bad season
    if(!season %in% c(TRUE, FALSE, T, F)) {
      message("Error: Please supply TRUE or FALSE for `season` parameter.")
    }

    # Create a tibble of the HIP .txt files to be read from the provided
    # directory
    files <-
      list.files(
        path, recursive = {{season}}, pattern = "*\\.txt$", ignore.case = TRUE,
        full.names = TRUE) |>
      as_tibble_col(column_name = "filepath") |>
      # Don't process permit files
      filter(!str_detect(filepath, "permit")) |>
      # Identify blank files
      mutate(
        filepath = str_replace(filepath, "TXT", "txt"),
        check =
          ifelse(
            file.size(filepath) == 0,
            "blank",
            ""))

    # Filter files to include only specified state
    if(!is.na(state)) {
      files <- filter(files, str_detect(filepath, state))
    }
    # Error for blank files
    if("blank" %in% files$check) {
      message("Error: One or more files are blank in the directory.")
      print(filter(files, check == "blank"))
    }

    # Filter out blank files from the paths list
    files <- filter(files, check != "blank") |> pull(filepath)

    # Message if there are no files to read in
    if(length(files) == 0) {
      message(
        paste0(
          "Error: No file(s) to read in. Did you specify a state that did not ",
          "submit data?"))
    } else {

      # Read data from filepaths
      pulled_data <-
        map(
          1:length(files),
          function(i) {
            # Compile each state's file into one table
            read_fwf(
              files[i],
              fwf_widths(c(1, 15, 1, 20, 3, 60, 20, 2, 10, 10, 10,
                           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, NA)),
              col_types = "cccccccccccccccccccccccc",
              na = c("N/A", "")) |>
              mutate(
                # Add the download state as a column
                dl_state =
                  str_extract(
                    files[i], "[A-Z]{2}(?=[0-9]{8}\\.txt)"),
                # Add the download date as a column
                dl_date =
                  str_extract(
                    files[i], "(?<=[A-Z]{2})[0-9]{8}(?=\\.txt)"),
                # Add the source file as a column
                source_file =
                  str_remove(files[i], path),
                # Add the download cycle as a column
                dl_cycle =
                  str_extract(files[i], "(?<=DL).+(?=\\/)"))
          }) |>
        list_rbind() |>
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
        group_by(dl_date, dl_state) |>
        mutate(dl_key = paste0("dl_", cur_group_id())) |>
        ungroup() |>
        # Add a record key
        mutate(record_key = paste0("record_", row_number()))

      # Remove exact duplicates
      if(unique == TRUE){
        pulled_data <- distinct(pulled_data)
      }

      # Return a message for records with blank or NA values in firstname,
      # lastname, state, or birth date
      raw_nas <-
        pulled_data |>
        group_by(dl_state) |>
        mutate(n_total = n()) |>
        ungroup() |>
        filter(
          is.na(firstname)|is.na(lastname)|is.na(state)|is.na(birth_date)) |>
        group_by(dl_state) |>
        reframe(n = n(), prop = round(n/n_total, 2)) |>
        distinct() |>
        filter(n >= 100 | prop >= 0.1)

      if(nrow(raw_nas) > 0) {
        message(
          paste0("Error: NA values detected in one or more ID fields ",
                 "(firstname, lastname, state, birth date) for >10% of a file ",
                 "and/or >100 records."))

        print(raw_nas)
      }

      # Return a message if there is an NA in dl_state
      if(TRUE %in% is.na(pulled_data$dl_state)) {
        message(
          paste0("Error: One or more more NA values detected in dl_state."))

        print(
          pulled_data |>
            distinct(dl_state, source_file) |>
            filter(is.na(dl_state))
        )
      }

      # Return a message if there is an NA in dl_date
      if(TRUE %in% is.na(pulled_data$dl_date)) {
        message(
          paste0("Error: One or more more NA values detected in dl_date."))

        print(
          pulled_data |>
            select(dl_date, source_file) |>
            filter(is.na(dl_date)) |>
            distinct())
      }

      # Return a message if all emails are missing from a file
      if(nrow(
        pulled_data |>
        group_by(source_file) |>
        summarize(n_emails = length(unique(email))) |>
        ungroup() |>
        filter(n_emails == 1)) > 0) {
        message(
          paste0("Error: One or more files are missing 100% of emails."))

        print(
          pulled_data |>
            group_by(source_file) |>
            summarize(n_emails = length(unique(email))) |>
            ungroup() |>
            filter(n_emails == 1) |>
            select(source_file))
      }

      # Check if all dl_states are acceptable
      # States in the data
      dl_states_in_data <-
        distinct(pulled_data, dl_state)

      # Return a message if there is a dl_state not found in the list of 49
      # continental US states
      if(FALSE %in%
         (dl_states_in_data |> pull(dl_state) %in%
          datasets::state.abb[datasets::state.abb != "HI"])) {
        message(
          paste0("Error: One or more dl_state values do not belong in the ",
                 "list of expected 49 continental US states."))

        print(
          dl_states_in_data |>
            filter(
              !dl_state %in% datasets::state.abb[datasets::state.abb != "HI"]
            ) |>
            pull()
        )
      }

      return(pulled_data)
    }
  }
