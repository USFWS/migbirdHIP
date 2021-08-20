#' Read in data
#'
#' Compile data from state-exported text files by providing a path to the download directory.
#'
#' @importFrom dplyr as_tibble
#' @importFrom dplyr transmute
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr distinct
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
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
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
      # For reading data from a download cycle with a specific state
      if(!is.na(state) & season == FALSE){
        list.files(path, recursive = FALSE)  %>%
          as_tibble() %>%
          transmute(filepath = as.character(value)) %>%
          mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
          # Keep only txt files
          filter(str_detect(filepath, "(?<=\\.)txt$")) %>%
          # Keep only files from the specified state
          filter(str_detect(filepath, state)) %>%
          # Create new complete file paths
          mutate(filepath = paste0(path, filepath)) %>%
          # Filter out blank files
          mutate(
            filepath =
              ifelse(
                file.size(filepath) == 0,
                "blank",
                filepath)) %>%
          filter(filepath != "blank")}
    # For reading data from a download cycle for ALL states available
    else if(is.na(state) & season == FALSE){
      list.files(path, recursive = FALSE)  %>%
        as_tibble() %>%
        transmute(filepath = as.character(value)) %>%
        mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
        # Keep only txt files
        filter(str_detect(filepath, "(?<=\\.)txt$")) %>%
        # Create new complete file paths
        mutate(filepath = paste0(path, filepath)) %>%
        # Filter out blank files
        mutate(
          filepath =
            ifelse(
              file.size(filepath) == 0,
              "blank",
              filepath)) %>%
        filter(filepath != "blank")}
    # For reading in all data from the season
    else if(season == TRUE){
      list.files(path, recursive = TRUE)  %>%
        as_tibble() %>%
        # Disregard folders that do not begin with "DL"
        filter(str_detect(value, "^DL")) %>%
        transmute(filepath = as.character(value)) %>%
        mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
        # Keep only txt files
        filter(str_detect(filepath, "(?<=\\.)txt$")) %>%
        # Create new complete file paths
        mutate(filepath = paste0(path, filepath)) %>%
        # Filter out blank files
        mutate(
          filepath =
            ifelse(
              file.size(filepath) == 0,
              "blank",
              filepath)) %>%
        filter(filepath != "blank")
    }
    if(nrow(files) == 0) {
      message("No file(s) to read in. Did you specify a state that did not submit data?")
    }
    else{

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
      print(checked_encodings
      )

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
              col_types = "cccccccccccccccccccccccc") %>%
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
                  str_extract(pull(files[i, ]), "(?<=DL).+(?=\\/)")
              )
          })

      # Remove duplicates (or not)
      if(unique == TRUE){
        pulled_data <-
          pulled_data %>%
          distinct()}

      # Return a message for records with blank or NA values in firstname,
      # lastname, state, or birth date
      if(TRUE %in% is.na(pulled_data$X2) |
         TRUE %in% is.na(pulled_data$X4) |
         TRUE %in% is.na(pulled_data$X8) |
         TRUE %in% is.na(pulled_data$X10)){
        message(
          paste0("Error: One more more NA values detected in ID fields ",
                 "(firstname, lastname, state, birth date)."))

        print(
          pulled_data %>%
            filter(
              is.na(X2)|
                is.na(X4)|
                is.na(X8)|
                is.na(X10)) %>%
            select(dl_state, X2, X4, X8, X10))}

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
          dl_states_in_data %>%
            filter(!dl_state %in% acceptable_49_dl_states))}

      return(pulled_data)
    }
  }
