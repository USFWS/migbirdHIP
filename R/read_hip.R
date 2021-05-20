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
#' @param state When specified, reads in download data from a specified state. Must match one of the following two-letter abbreviations:
#' \itemize{
#' \item AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY, AS, GU, MP, PR, VI, UM, FM, MH, PW, AA, AE, AP, CM, CZ, NB, PI, TT, ON, QC, NS, NB, MB, BC, PE, SK, AB, NL}
#' @param season If set as TRUE, selects only folders starting with "DL" in a a season's upper-level directory
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

read_hip <-
  function(path, state = NA, season = FALSE) {

    # Create a tibble of the HIP .txt files to be read from the provided
    # directory
    files <-
      # For reading data from a download cycle with a specific state
      if(!is.na(state) & season == FALSE){
        list.files(path, recursive = "TRUE")  %>%
        as_tibble() %>%
        transmute(filepath = as.character(value)) %>%
        mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
        # Keep only txt files
        filter(str_detect(filepath, "(?<=\\.)txt$")) %>%
        # Keep only files from the specified state
        filter(str_detect(filepath, state)) %>%
        # Create new complete file paths
        mutate(filepath = paste(path, filepath, sep = "/")) %>%
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
        list.files(path, recursive = "TRUE")  %>%
          as_tibble() %>%
          transmute(filepath = as.character(value)) %>%
          mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
          # Keep only txt files
          filter(str_detect(filepath, "(?<=\\.)txt$")) %>%
          # Create new complete file paths
          mutate(filepath = paste(path, filepath, sep = "/")) %>%
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
        list.files(path, recursive = "TRUE")  %>%
          as_tibble() %>%
          # Disregard folders that do not begin with "DL"
          filter(str_detect(value, "^DL")) %>%
          transmute(filepath = as.character(value)) %>%
          mutate(filepath = str_replace(filepath, "TXT", "txt")) %>%
          # Keep only txt files
          filter(str_detect(filepath, "(?<=\\.)txt$")) %>%
          # Create new complete file paths
          mutate(filepath = paste(path, filepath, sep = "/")) %>%
          # Filter out blank files
          mutate(
            filepath =
              ifelse(
                file.size(filepath) == 0,
                "blank",
                filepath)) %>%
          filter(filepath != "blank")
      }

    if(nrow(files) == 0){
      message("No file(s) to read in. Did you specify a state that did not submit data?")
    }

    else{
      # Check encodings of the files that will be read
      checked_encodings <-
        map_dfr(
          1:nrow(files),
          function(i) {
            guess_encoding(pull(files[i,])) %>%
              mutate(filepath = pull(files[i,]))
          }
        ) %>%
        group_by(filepath) %>%
        filter(str_detect(encoding, "UTF\\-16") | confidence < 1| n() > 1) %>%
        ungroup() %>%
        filter(encoding != "UTF-8") %>%
        select(filepath, encoding, confidence)

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
          }) %>%
        # Remove duplicates
        distinct()

      # Check if all dl_states are acceptable

      # String of 49 continental US states
      acceptable_49_dl_states <-
        c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "ID", "IL",
          "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
          "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR",
          "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
          "WY")

      # String of states in the data
      dl_states_in_data <-
        pulled_data %>%
        select(dl_state) %>%
        distinct() %>%
        pull()

      # If there is a dl_state not found in the list of 49 continental US states,
      # return a message reporting the problem
      if(FALSE %in% (dl_states_in_data %in% acceptable_49_dl_states) == TRUE){
        message(
          paste0("One or more dl_state values do not belong in the list of ",
                 "expected 49 continental US states."))
        return(pulled_data)
      }else{
        return(pulled_data)
      }
    }

  }
