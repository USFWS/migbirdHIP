#' Check for repeat files
#'
#' Check if any files in the input folder have already been written to processed
#' folder.
#'
#' @importFrom stringr str_replace
#' @importFrom dplyr tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @param raw_path Directory of the download folder containing HIP .txt files
#' @param processed_path Directory of the folder containing processed files
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

fileCheck <-
  function(raw_path, processed_path) {
    # Add a final "/" to raw_path if not included already
    if(!str_detect(raw_path, "\\/$")) {
      raw_path <- paste0(raw_path, "/")
    }
    # Add a final "/" to processed_path if not included already
    if(!str_detect(processed_path, "\\/$")) {
      processed_path <- paste0(processed_path, "/")
    }
    if(TRUE %in%
       (str_replace(list.files(raw_path, recursive = F), "TXT", "txt") %in%
        str_replace(list.files(processed_path, recursive = F), "csv", "txt"))) {
      message(
        "The following files have already been written to the processed dir.")

      repeated <-
        tibble(
          filename =
            str_replace(list.files(raw_path, recursive = F), "TXT", "txt"),
          origin = "input") |>
        bind_rows(
          tibble(
            filename =
              str_replace(
                list.files(processed_path, recursive = F), "csv", "txt"),
            origin = "processed")
        ) |>
        group_by(.data$filename) |>
        filter(n() > 1) |>
        ungroup() |>
        distinct(.data$filename)

      print(repeated)
    } else {
      message("All files are new.")
    }
  }

#' Raw HIP file name correction
#'
#' This function overwrites HIP filenames. Files in the supplied directory are
#' renamed by converting the Julian date to YYYYMMDD format. State abbreviations
#' that are in lowercase format are capitalized.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr bind_cols
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_replace
#' @importFrom stringr str_extract
#' @importFrom dplyr as_tibble
#' @importFrom tidyr separate_wider_position
#' @importFrom tidyr unite
#' @importFrom rlang .data
#'
#' @param path Directory to download folder containing new HIP files
#' @param year The year in which the Harvest Information Program data were
#'   collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

fileRename <-
  function(path, year){
    failyear(year)

    # Add a final "/" if not included already
    if(!str_detect(path, "\\/$")) {
      path <- paste0(path, "/")
    }

    # Run if there are files in the dir with 5 digits (old Julian data format)
    if(TRUE %in% str_detect(list.files(path), "^[A-Z]{2}[0-9]{3}\\.(txt|TXT)$")){

      # File name(s) with 5-digit format
      names5 <- list.files(path, pattern = "^[A-Z]{2}[0-9]{3}\\.(txt|TXT)$")

      # New 10-digit file name(s)
      names10 <-
        names5 |>
        as_tibble() |>
        # Pull the file name apart so we can convert the date
        separate_wider_position(
          .data$value, c("state" = 2, "jdate" = 3, "suffix" = 4)) |>
        # Convert Julian date to YYYYMMDD
        # The as.Date function calculates to the jdate + 1, so subtract a day
        # using - 1 to get the accurate date; str_remove wraps around the
        # conversion script to strip the automatically added dash separators
        mutate(
          jdate =
            str_remove_all(
              as.character(
                as.Date(
                  as.numeric(.data$jdate),
                  origin = structure(paste0(as.character(year),"-01-01"))) - 1),
              "-")) |>
        unite("value", 1:3, sep = "") |>
        mutate(value = paste0(path, .data$value)) |>
        pull()

      # Add dir to file names
      names5 <- paste0(path, names5)

      # Overwrite the file names in the given directory
      file.rename(from = names5, to = names10)

      message("Success: Julian dates changed to standard format.")
      print(
        bind_cols(
          old = str_extract(names5, "(?<=\\/)[A-Z|a-z]{2}[0-9]{1,5}\\.txt"),
          new = str_extract(names10, "(?<=\\/)[A-Z]{2}[0-9]{8}\\.txt")))
    }

    # Run if there's a lowercase letter in the state abbreviation
    if(TRUE %in%
       str_detect(
         list.files(path),
         "^([a-z]{2}|[A-Z]{1}[a-z]{1}|[a-z]{1}[A-Z]{1})(?=[0-9])")){

      names_lower <-
        list.files(path) |>
        as_tibble() |>
        filter(
          str_detect(
            .data$value,
            "^([a-z]{2}|[A-Z]{1}[a-z]{1}|[a-z]{1}[A-Z]{1})(?=[0-9])")) |>
        pull()

      names_upper <-
        names_lower |>
        str_to_upper() |>
        str_replace("TXT", "txt")

      # Add dir to file names
      names_lower2 <- paste0(path, names_lower)
      names_upper2 <- paste0(path, names_upper)

      # Overwrite the file names in the given directory
      file.rename(from = names_lower2, to = names_upper2)

      message("Success: Lowercase state abbreviations changed to upper.")
      print(
        bind_cols(old = names_lower, new = names_upper))
    }
    if(FALSE %in% str_detect(list.files(path), "^[A-Z]{2}[0-9]{8}\\.")){
      message("Error: Unresolved issue(s) with file name(s) in directory.")
      print(
        list.files(path) |>
          as_tibble() |>
          filter(!str_detect(.data$value, "^[A-Z]{2}[0-9]{8}\\.")))
    }
  }
