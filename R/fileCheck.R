#' Check for repeat files
#'
#' Check if any files in the input folder have already been written to processed
#' folder.
#'
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#'
#' @param raw_path Directory of the download folder containing HIP .txt files
#' @param processed_path Directory of the folder containing processed files
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

fileCheck <-
  function() {
    if(TRUE %in%
       (str_replace(list.files(raw_path, recursive = F), "TXT", "txt") %in%
        str_replace(list.files(processed_path, recursive = F), "csv", "txt"))) {
      message(
        "The following files have already been written to the processed dir.")

      repeated <-
        tibble(
          filename =
            str_replace(list.files(raw_path, recursive = F), "TXT", "txt"),
          origin = "input") %>%
        bind_rows(
          tibble(
            filename =
              str_replace(
                list.files(processed_path, recursive = F), "csv", "txt"),
            origin = "processed")
        ) %>%
        group_by(filename) %>%
        filter(n() > 1) %>%
        ungroup() %>%
        select(filename) %>%
        distinct()

      print(repeated)
    } else {
      message("All files are new.")
    }
  }

