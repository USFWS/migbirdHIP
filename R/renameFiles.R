#' Raw HIP file name correction
#'
#' This function overwrites HIP filenames. Files in the supplied directory are renamed by converting the Julian date to YYYYMMDD format. State abbreviations that are in lowercase format are capitalized.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr bind_cols
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_replace
#' @importFrom tibble as_tibble
#' @importFrom tidyr separate
#' @importFrom tidyr unite
#'
#' @param path Directory to download folder containing new HIP files
#' @param year The year in which the Harvest Information Program data were collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

renameFiles <-
  function(path, year){

    # Add a final "/" if not included already
    if(!str_detect(path, "\\/$")){
      x <- paste0(path, "/")
      }else{
        x <- path
      }

    # Run if there are files in the dir with 5 digits (old Julian data format)
    if(TRUE %in% str_detect(list.files(x), "^[A-Z]{2}[0-9]{3}(?=\\.)")){

      # File name(s) with 5-digit format
      names5 <- list.files(x, pattern = "^[A-Z]{2}[0-9]{3}\\.")

      # New 10-digit file name(s)
      names10 <-
        names5 %>%
        as_tibble() %>%
        # Pull the file name apart so we can convert the date
        separate(value, into = c("state", "jdate", "suffix"), sep = c(2, 5)) %>%
        # Convert Julian date to YYYYMMDD
        # The as.Date function calculates to the jdate + 1, so subtract a day
        # using - 1 to get the accurate date; str_remove wraps around the
        # conversion script to strip the automatically added dash separators
        mutate(
          jdate =
            str_remove_all(
              as.character(
                as.Date(
                  as.numeric(jdate),
                  origin = structure(paste0(as.character(year),"-01-01"))) - 1),
              "-")) %>%
        unite(value, 1:3, sep = "") %>%
        mutate(value = paste0(x, value)) %>%
        pull()

      # Add dir to file names
      names5 <- paste0(x, names5)

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
         list.files(x),
         "^([a-z]{2}|[A-Z]{1}[a-z]{1}|[a-z]{1}[A-Z]{1})(?=[0-9])")){

      names_lower <-
        list.files(x) %>%
        as_tibble() %>%
        filter(
          str_detect(
            value,
            "^([a-z]{2}|[A-Z]{1}[a-z]{1}|[a-z]{1}[A-Z]{1})(?=[0-9])")) %>%
        pull()

      names_upper <-
        names_lower %>%
        str_to_upper() %>%
        str_replace("TXT", "txt")

      # Add dir to file names
      names_lower2 <- paste0(x, names_lower)
      names_upper2 <- paste0(x, names_upper)

      # Overwrite the file names in the given directory
      file.rename(from = names_lower2, to = names_upper2)

      message("Success: Lowercase state abbreviations changed to upper.")
      print(
        bind_cols(old = names_lower, new = names_upper))
      }
    if(FALSE %in% str_detect(list.files(x), "^[A-Z]{2}[0-9]{8}\\.")){
      message("Error: Unresolved issue(s) with file name(s) in directory.")
      print(
        list.files(x) %>%
          as_tibble() %>%
          filter(!str_detect(value, "^[A-Z]{2}[0-9]{8}\\.")))
    }
  }
