#' Read in data
#'
#' Compile data from state-exported text files by providing a path to the download directory.
#'
#' @importFrom magrittr %<>%
#' @importFrom dplyr %>%
#' @importFrom tibble as_tibble_col
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr across
#' @importFrom dplyr vars
#' @importFrom dplyr matches
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr rename
#' @importFrom dplyr mutate_all
#' @importFrom dplyr cur_group_id
#' @importFrom dplyr row_number
#' @importFrom purrr map_dfr
#' @importFrom purrr map_df
#' @importFrom readr read_fwf
#' @importFrom readr fwf_widths
#' @importFrom readr guess_encoding
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
    if(!str_detect(path, "\\/$")){
      path <- paste0(path, "/")
    }else{
      path <- path
    }

    # Create a tibble of the HIP .txt files to be read from the provided
    # directory
    files <-
      # Read data for a specific state from a download cycle
      if(!is.na(state) & season == FALSE){
        list.files(
          path, recursive = FALSE, pattern = "*\\.txt$", ignore.case = TRUE,
          full.names = TRUE) %>%
          as_tibble_col(column_name = "filepath") %>%
          mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
          # Keep only files from the specified state
          filter(str_detect(filepath, state)) %>%
          # Filter out blank files
          mutate(
            check =
              ifelse(
                file.size(filepath) == 0,
                "blank",
                filepath))
      }else if(!is.na(state) & season == TRUE){
        # Read data for a specific state across the whole season
        list.files(
          path, recursive = TRUE, pattern = "*\\.txt$", ignore.case = TRUE,
          full.names = TRUE) %>%
          as_tibble_col(column_name = "filepath") %>%
          mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
          # Keep only files from the specified state
          filter(str_detect(filepath, state)) %>%
          # Don't process permit files
          filter(!str_detect(filepath, "permit")) %>%
          # Don't process removed files
          filter(!str_detect(filepath, "removed")) %>%
          # Filter out blank files
          mutate(
            check =
              ifelse(
                file.size(filepath) == 0,
                "blank",
                filepath))
      }else if(is.na(state) & season == FALSE){
        # Read data from a download cycle for all states
        list.files(
          path, recursive = FALSE, pattern = "*\\.txt$", ignore.case = TRUE,
          full.names = TRUE) %>%
          as_tibble_col(column_name = "filepath") %>%
          mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
          # Filter out blank files
          mutate(
            check =
              ifelse(
                file.size(filepath) == 0,
                "blank",
                filepath))
      }else if(is.na(state) & season == TRUE){
        # Read in all data from the season
        list.files(
          path, recursive = TRUE, pattern = "*\\.txt$", ignore.case = TRUE,
          full.names = TRUE) %>%
          as_tibble_col(column_name = "filepath") %>%
          mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
          # Don't process permit files
          filter(!str_detect(filepath, "permit")) %>%
          # Don't process removed files
          filter(!str_detect(filepath, "removed")) %>%
          # Filter out blank files
          mutate(
            check =
              ifelse(
                file.size(filepath) == 0,
                "blank",
                filepath))
      }else{
        message(
          paste0("Error: `state` must be a two-letter abbreviation or NA. `sea",
                 "son` must be TRUE or FALSE."))
      }

    if(nrow(files %>% filter(check == "blank")) > 0){
      message("Warning: One or more files are blank in the directory.")
      print(files %>% filter(check == "blank"))
    }

    files %<>%
      filter(check != "blank") %>%
      select(-check)

    if(nrow(files) == 0){
      message(
        paste0("No file(s) to read in. Did you specify a state that did not",
               " submit data?"))
      }else{
      # Check encodings of the files that will be read
      checked_encodings <-
        map_dfr(1:nrow(files),
                function(i) {
                  guess_encoding(pull(files[i, ])) %>%
                    mutate(filepath = pull(files[i, ]))
                }) %>%
        group_by(filepath) %>%
        filter(str_detect(encoding, "UTF\\-16") |
                 confidence < 1 | n() > 1) %>%
        ungroup() %>%
        filter(encoding != "UTF-8") %>%
        select(filepath, encoding, confidence)
      print(checked_encodings)

      # Read data from filepaths
      pulled_data <-
        map_df(
          1:nrow(files),
          function(i) {
            # Compile each state's file into one table
            read_fwf(
              pull(files[i,]),
              fwf_widths(c(1, 15, 1, 20, 3, 60, 20, 2, 10, 10, 10,
                           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, NA)),
              col_types = "cccccccccccccccccccccccc",
              na = c("N/A", "")) %>%
              rename(
                title = X1,
                firstname = X2,
                middle = X3,
                lastname = X4,
                suffix = X5,
                address = X6,
                city = X7,
                state = X8,
                zip = X9,
                birth_date = X10,
                issue_date = X11,
                hunt_mig_birds = X12,
                ducks_bag = X13,
                geese_bag = X14,
                dove_bag = X15,
                woodcock_bag = X16,
                coots_snipe = X17,
                rails_gallinules = X18,
                cranes = X19,
                band_tailed_pigeon = X20,
                brant = X21,
                seaducks = X22,
                registration_yr = X23,
                email = X24) %>%
              mutate(
                # Add the download state as a column
                dl_state =
                  str_extract(
                    pull(files[i, ]), "[A-Z]{2}(?=[0-9]{8}\\.txt)"),
                # Add the download date as a column
                dl_date =
                  str_extract(
                    pull(files[i, ]), "(?<=[A-Z]{2})[0-9]{8}(?=\\.txt)"),
                # Add the source file as a column
                source_file =
                  str_remove(pull(files[i, ]), path),
                source_file =
                  str_remove(source_file, "^\\/"),
                # Add the download cycle as a column
                dl_cycle =
                  str_extract(pull(files[i, ]), "(?<=DL).+(?=\\/)"))
          })

      pulled_data %<>%
        # Add a download key
        group_by(dl_date, dl_state) %>%
        mutate(dl_key = paste0("dl_", cur_group_id())) %>%
        ungroup() %>%
        # Add a record key
        mutate(record_key = paste0("record_", row_number()))

      # Remove duplicates (or not)
      if(unique == TRUE){
        pulled_data <-
          pulled_data %>%
          distinct()}

      # Return a message for records with blank or NA values in firstname,
      # lastname, state, or birth date
      if(TRUE %in% is.na(pulled_data$firstname) |
         TRUE %in% is.na(pulled_data$lastname) |
         TRUE %in% is.na(pulled_data$state) |
         TRUE %in% is.na(pulled_data$birth_date)){
        message(
          paste0("Error: One or more NA values detected in ID fields ",
                 "(firstname, lastname, state, birth date)."))

        print(
          pulled_data %>%
            filter(
              is.na(firstname)|
                is.na(lastname)|
                is.na(state)|
                is.na(birth_date)) %>%
            select(dl_state, firstname, lastname, state, birth_date) %>%
            rename(X = state) %>%
            mutate_at(
              vars(matches("firstname|lastname|X|birth_date")),
              ~ifelse(is.na(.), "1", NA) %>% as.numeric(.)) %>%
            mutate(
              sum =
                rowSums(
                  across(matches("firstname|lastname|X|birth_date")),
                  na.rm = T),
              prop = sum/4) %>%
            group_by(dl_state) %>%
            summarize(
              mean_prop = mean(prop),
              n = n()
            ) %>%
            ungroup() %>%
            arrange(desc(mean_prop)))}

      # Return a message if there is an NA in dl_state
      if(TRUE %in% is.na(pulled_data$dl_state)){
        message(
          paste0("Error: One or more more NA values detected in dl_state."))

        print(
          pulled_data %>%
            select(dl_state, source_file) %>%
            filter(is.na(dl_state)) %>%
            distinct())}

      # Return a message if there is an NA in dl_date
      if(TRUE %in% is.na(pulled_data$dl_date)){
        message(
          paste0("Error: One or more more NA values detected in dl_date."))

        print(
          pulled_data %>%
            select(dl_date, source_file) %>%
            filter(is.na(dl_date)) %>%
            distinct())}

      # Return a message if all emails are missing from a file
      if(nrow(
           pulled_data %>%
             group_by(source_file) %>%
             summarize(n_emails = length(unique(email))) %>%
             ungroup() %>%
             filter(n_emails == 1)) > 0){
        message(
          paste0("Error: One or more files are missing 100% of emails."))

        print(
          pulled_data %>%
            group_by(source_file) %>%
            summarize(n_emails = length(unique(email))) %>%
            ungroup() %>%
            filter(n_emails == 1) %>%
            select(source_file))}

      # Check if all dl_states are acceptable

      # String of 49 continental US states
      acceptable_49_dl_states <-
        c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID",
          "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
          "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND",
          "OH", "OK", "OR","PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT",
          "VA", "WA", "WV", "WI", "WY")

      # String of states in the data
      dl_states_in_data <-
        pulled_data %>%
        select(dl_state) %>%
        distinct() %>%
        pull()

      # Return a message if there is a dl_state not found in the list of 49
      # continental US states
      if(FALSE %in% (dl_states_in_data %in% acceptable_49_dl_states) == TRUE){
        message(
          paste0("Error: One or more dl_state values do not belong in the ",
                 "list of expected 49 continental US states."))

        print(
          dl_states_in_data[!dl_states_in_data %in% acceptable_49_dl_states]
          )}

      return(pulled_data)
    }
  }
