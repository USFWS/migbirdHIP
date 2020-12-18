#' Check if data are UTF-8
#'
#' Check the encoding of all files in download directory
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

encodingCheck <-
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

    # Fix encodings to UTF-8 if they're anything other than UTF-8

    if(nrow(checked_state_files) != 0){

      print(checked_state_files)

    }

    else(

      message("All files are UTF-8!")

    )
  }
