#' Read in data
#'
#' Compile data from state-exported text files by providing a path to the download directory.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import readr
#' @import purrr
#' @import tibble
#'
#' @param path File path to the folder containing HIP .txt files
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

read_hip <-
  function(path) {

    # Create a tibble of the HIP .txt files to be read from the provided
    # directory
    files <-
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
      filter(filepath != "blank")

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
      select(dl_states) %>%
      distinct() %>%
      pull()

    # If there is a dl_state not found in the list of 49 continental US states,
    # return a message reporting the problem
    if(FALSE %in% (dl_states_in_data %in% acceptable_49_dl_states) == TRUE){
      warning(
        paste0("One or more dl_state values do not belong in the list of",
               "expected 49 continental US states."))
    }else{
      return(pulled_data)
      }

  }
