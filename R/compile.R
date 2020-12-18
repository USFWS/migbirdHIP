#' Read in data
#'
#' Compile data from all state-exported text files by providing a path to the download directory. If the error "Directory contains file(s) with non-UTF-8 encoding." is shown, use \code{\link{compile_to_utf8}} instead.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import readr
#' @import purrr
#' @import tibble
#'
#' @param path The path to the raw data file to be checked
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

compile <-
  function(path) {

    path <- path

    state_files <-
      list.files(path, recursive = "TRUE")  %>%
      as_tibble() %>%
      transmute(filename = as.character(value)) %>%
      mutate(filename = str_replace(filename, "TXT", "txt")) %>%
      # Keep only txt files
      filter(str_detect(filename, "(?<=\\.)txt$")) %>%
      # Create new complete file paths
      mutate(filename = paste(path, filename, sep = "/")) %>%
      # Filter out blank files
      mutate(
        filename =
          ifelse(
            file.size(filename) == 0,
            "blank",
            filename)) %>%
      filter(filename != "blank")

    # Check encodings

    checked_state_files <-
      state_files %>%
      # Compile each file's encoding into one table
      bind_cols(
        map_dfr(
          1:nrow(state_files),
          function(i) {
            # Since guess_encoding can report more than one guess, we unselect
            # the confidence column and  pull the first row of the tibble
            # with slice_head to make a column of the same length of
            # state_files for joining. This allows us to report exactly which
            # file(s) is/are UTF-16.
            slice_tail(
              guess_encoding(pull(state_files[i, ]))
            )
          })
      ) %>%
      filter(str_detect(encoding, "UTF\\-16") | confidence < 1)

    if(nrow(checked_state_files) != 0){

      print(checked_state_files)
      message("Error: Directory contains file(s) with non-UTF-8 encoding.")

    }

    else{
      # Pull the data

      pulled_data <-
        # If .txt files don't contain all the expected cols, warnings are thrown
        # Suppress warnings because data integrity is fine
        suppressWarnings(
          map_df(
            1:nrow(state_files),
            function(i) {
              # Compile each state's file into one table
              read_fwf(
                pull(state_files[i,]),
                fwf_widths(c(1, 15, 1, 20, 3, 60, 20, 2, 10, 10, 10,
                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, NA)),
                col_types = "cccccccccccccccccccccccc") %>%
                mutate(
                  # Add the download's state as a column
                  dl_state =
                    str_extract(
                      pull(state_files[i, ]), "[A-Z]{2}(?=[0-9]{8}\\.txt)"),
                  # Add the download's date as a column
                  dl_date =
                    str_extract(
                      pull(state_files[i, ]), "(?<=[A-Z]{2})[0-9]{8}(?=\\.txt)"),
                  dl_cycle =
                    str_extract(path, "(?<=\\/DL).+$"),
                  source_file =
                    str_remove(pull(state_files[i, ]), path),
                  # Add another mutate because can't pipe '.'
                  source_file =
                    str_remove(source_file, "^\\/")
                )
            })
        ) %>%
        # Remove duplicates
        distinct()

      return(pulled_data)
    }
  }
