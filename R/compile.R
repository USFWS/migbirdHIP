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
#' @param path The path to the data .txt files
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

    # Check encodings

    checked_state_files <-
      map_dfr(
        1:nrow(state_files),
        function(i) {
          guess_encoding(pull(state_files[i,])) %>%
            mutate(filepath = pull(state_files[i,]))
        }
      ) %>%
      group_by(filepath) %>%
      filter(str_detect(encoding, "UTF\\-16") | confidence < 1| n() > 1) %>%
      ungroup() %>%
      filter(encoding != "UTF-8") %>%
      select(filepath, encoding, confidence)

    # Report error if data are not UTF-8

    if(nrow(checked_state_files) != 0){

      print(checked_state_files)
      warning("Directory contains file(s) with non-UTF-8 encoding.")

    }

    # Pull the data

    else{

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
                  # Add the download state as a column
                  dl_state =
                    str_extract(
                      pull(state_files[i, ]), "[A-Z]{2}(?=[0-9]{8}\\.txt)"),
                  # Add the download date as a column
                  dl_date =
                    str_extract(
                      pull(state_files[i, ]), "(?<=[A-Z]{2})[0-9]{8}(?=\\.txt)"),
                  # Add the source file as a column
                  source_file =
                    str_remove(pull(state_files[i, ]), path),
                  source_file =
                    str_remove(source_file, "^\\/"),
                  # Add the download cycle as a column
                  dl_cycle =
                    str_extract(pull(state_files[i, ]), "(?<=DL).+(?=\\/)")
                )
            })
        ) %>%
        # Remove duplicates
        distinct()

      return(pulled_data)
    }
  }
