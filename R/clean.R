#' Clean data
#'
#' After reading the data with \code{\link{read_hip}}, do basic data cleaning.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate_all
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom dplyr if_any
#' @importFrom dplyr select
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom dplyr left_join
#' @importFrom dplyr reframe
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#'
#' @param x The object created after reading in data with \code{\link{read_hip}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

clean <-
  function(x){

    cleaned_x <-
      x |>
      # Filter out records if firstname, lastname, city of residence, state of
      # residence, or date of birth are missing -- records discarded because
      # these are needed to identify individuals
      filter(!if_any(c("firstname", "lastname", "state", "birth_date"), ~is.na(.x))) |>
      # Discard additional records if they are missing a value for email AND
      # elements of a physical address that are required to determine where
      filter(!if_all(c("address", "email"), ~is.na(.x))) |>
      filter(!if_all(c("city", "zip", "email"), ~is.na(.x))) |>
      mutate(
        # Names to uppercase for easier stringr
        firstname = str_to_upper(firstname),
        lastname = str_to_upper(lastname),
        # Extract suffixes from lastname and firstname cols to suffix col
        # Catches values from 1-20 in Roman numerals and numeric, excluding
        # XVIII since the db limit is 4 characters
        suffix =
          case_when(
            # Lastname
            str_detect(lastname, "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$") ~
              str_extract(lastname, "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$"),
            # Firstname
            str_detect(firstname, "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$") ~
              str_extract(firstname, "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$"),
            TRUE ~ suffix),
        # The original suffixes aren't uppercase, convert them to upper
        suffix = str_to_upper(suffix),
        # Delete periods and commas from suffixes
        suffix = str_remove_all(suffix, "\\.|\\,"),
        # Delete suffixes from lastname col (includes 1-20 in Roman numerals and
        # numeric, excluding XVIII since the db limit is 4 characters)
        lastname =
          ifelse(
            str_detect(lastname, "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$"),
            str_remove(lastname, "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$"),
            lastname),
        # Delete suffixes from firstname col (includes 1-20 in Roman numerals
        # and numeric, excluding XVIII since the db limit is 4 characters)
        firstname =
          ifelse(
            str_detect(firstname, "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$"),
            str_remove(firstname, "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$"),
            firstname),
        # Remove anything that's not a letter from middle initial col
        middle = ifelse(str_detect(middle, "[^A-Z]"), NA, middle),
        # Zip code correction
        zip =
          # Remove ending hyphen from zip codes with 5 digits
          ifelse(
            str_detect(zip, "^[0-9]{5}\\-$"),
            str_remove(zip, "\\-$"),
            zip),
        zip =
          # Remove final 0 from zip codes with length of 10 digits
          ifelse(
            str_detect(zip, "^[0-9]{10}$") &
              str_extract(zip, "[0-9]{1}(?=$)") == "0",
            str_remove(zip, "[0-9]{1}(?=$)"),
            zip),
        zip =
          # Insert a hyphen in continuous 9 digit zip codes
          ifelse(
            str_detect(zip, "^[0-9]{9}$"),
            paste0(
              str_extract(zip, "^[0-9]{5}"),
              "-",
              str_extract(zip,"[0-9]{4}$")),
            zip),
        zip =
          # Insert a hyphen in 9 digit zip codes with a middle space
          ifelse(
            str_detect(zip, "^[0-9]{5}\\s[0-9]{4}$"),
            str_replace(zip, "\\s", "\\-"),
            zip),
        zip =
          # Remove trailing -0000
          ifelse(
            str_detect(zip, "\\-0000"),
            str_remove(zip, "\\-0000"),
            zip),
        zip =
          # Remove trailing -___
          ifelse(
            str_detect(zip, "\\-\\_+"),
            str_remove(zip, "\\-\\_+"),
            zip)) |>
      # Delete white space around strings again
      mutate_all(str_trim)

    # Proof the zip codes -- are they associated with the correct states?
    zipcheck <-
      cleaned_x |>
      left_join(
        zip_code_ref |>
          distinct(zip = zipcode, zipState = state),
        by = "zip") |>
      select(source_file, state, zip, zipState) |>
      group_by(source_file) |>
      mutate(total_records = n()) |>
      ungroup() |>
      filter(state != zipState) |>
      group_by(source_file) |>
      reframe(
        n = n(),
        prop = round(n/total_records, 2)) |>
      distinct() |>
      arrange(desc(n)) |>
      filter(n >= 100 | prop >= 0.1)

    # Error check: are any zip codes wrong?
    if(nrow(zipcheck) > 0){
      message(
        paste0("Warning: Zip codes detected that do not correspond to ",
               "provided state of residence for >10% of a file ",
               "and/or >100 records."))

      print(zipcheck)
    }

    # If any permit file states submitted a 2 for crane and/or
    # band_tailed_pigeon, change the 2 to a 0
    cleaned_x <- strataFix(cleaned_x)

    return(cleaned_x)
  }
