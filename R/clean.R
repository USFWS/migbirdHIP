#' Clean data
#'
#' After reading the data with \code{\link{read_hip}}, rename the column names and do preliminary cleaning of firstname, lastname, middle initial, and suffixes.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr cur_group_id
#' @importFrom dplyr ungroup
#' @importFrom dplyr case_when
#' @importFrom dplyr row_number
#' @importFrom dplyr mutate_all
#' @importFrom dplyr na_if
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom rlang .data
#'
#' @param x The object created after reading in data with \code{\link{read_hip}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

clean <-
  function(x){

    cleaned_x <-
      x %>%
      rename(
        title = X1,
        firstname = X2,
        middle = X3,
        lastname = X4,
        suffix = X5,
        address = X6,
        city = X7,
        state = X8,
        zip = X9,
        birth_date = X10,
        # Edited X11 to specific .data$X11 to avoid error:
        # "Found an obsolete/platform-specific call in: 'tidy'"
        # "Found the platform-specific device: 'X11'"
        issue_date = .data$X11,
        hunt_mig_birds = X12,
        ducks_bag = X13,
        geese_bag = X14,
        dove_bag = X15,
        woodcock_bag = X16,
        coots_snipe = X17,
        rails_gallinules = X18,
        cranes = X19,
        band_tailed_pigeon = X20,
        brant = X21,
        seaducks = X22,
        registration_yr = X23,
        email = X24) %>%
      # Delete white space around strings
      mutate_all(str_trim) %>%
      # Convert N/A strings to NA
      na_if("N/A") %>%
      # Add a download key
      group_by(dl_date, dl_state) %>%
      mutate(dl_key = paste0("dl_", cur_group_id())) %>%
      ungroup() %>%
      # Filter out records if firstname, lastname, city of residence, state of
      # residence, or date of birth are missing -- records discarded because
      # these are needed to identify individuals
      filter(!is.na(firstname)) %>%
      filter(!is.na(lastname)) %>%
      filter(!is.na(state)) %>%
      filter(!is.na(birth_date)) %>%
      # Discard additional records if they are missing elements of an address
      # and we have no way to resolve it from remaining information (i.e. zip
      # from city)
      filter(!is.na(address)) %>%
      filter(!is.na(city) & !is.na(zip)) %>%
      mutate(
        # Add a record key
        record_key = paste0("record_", row_number()),
        # Names to uppercase for easier stringr
        firstname = str_to_upper(firstname),
        lastname = str_to_upper(lastname),
        # Extract suffixes from lastname and firstname cols to suffix col
        # We cannot go above VII in Roman numerals or 9TH in ordinal numbering
        # because of the 3 char column limit
        suffix =
          case_when(
            # Lastname
            str_detect(lastname, "(?<=\\s)JR\\.?$") ~ "JR",
            str_detect(lastname, "(?<=\\s)SR\\.?$") ~ "SR",
            str_detect(lastname, "(?<=\\s)I\\.?$") ~ "I",
            str_detect(lastname, "(?<=\\s)II\\.?$") ~ "II",
            str_detect(lastname, "(?<=\\s)III\\.?$") ~ "III",
            str_detect(lastname, "(?<=\\s)IV\\.?$") ~ "IV",
            str_detect(lastname, "(?<=\\s)V\\.?$") ~ "V",
            str_detect(lastname, "(?<=\\s)VI\\.?$") ~ "VI",
            str_detect(lastname, "(?<=\\s)VII\\.?$") ~ "VII",
            str_detect(lastname, "(?<=\\s)1ST\\.?$") ~ "1ST",
            str_detect(lastname, "(?<=\\s)2ND\\.?$") ~ "2ND",
            str_detect(lastname, "(?<=\\s)3RD\\.?$") ~ "3RD",
            str_detect(lastname, "(?<=\\s)4TH\\.?$") ~ "4TH",
            str_detect(lastname, "(?<=\\s)5TH\\.?$") ~ "5TH",
            str_detect(lastname, "(?<=\\s)6TH\\.?$") ~ "6TH",
            str_detect(lastname, "(?<=\\s)7TH\\.?$") ~ "7TH",
            str_detect(lastname, "(?<=\\s)8TH\\.?$") ~ "8TH",
            str_detect(lastname, "(?<=\\s)9TH\\.?$") ~ "9TH",
            # Firstname
            str_detect(firstname, "(?<=\\s)JR\\.?$") ~ "JR",
            str_detect(firstname, "(?<=\\s)SR\\.?$") ~ "SR",
            str_detect(firstname, "(?<=\\s)I\\.?$") ~ "I",
            str_detect(firstname, "(?<=\\s)II\\.?$") ~ "II",
            str_detect(firstname, "(?<=\\s)III\\.?$") ~ "III",
            str_detect(firstname, "(?<=\\s)IV\\.?$") ~ "IV",
            str_detect(firstname, "(?<=\\s)V\\.?$") ~ "V",
            str_detect(firstname, "(?<=\\s)VI\\.?$") ~ "VI",
            str_detect(firstname, "(?<=\\s)VII\\.?$") ~ "VII",
            str_detect(firstname, "(?<=\\s)1ST\\.?$") ~ "1ST",
            str_detect(firstname, "(?<=\\s)2ND\\.?$") ~ "2ND",
            str_detect(firstname, "(?<=\\s)3RD\\.?$") ~ "3RD",
            str_detect(firstname, "(?<=\\s)4TH\\.?$") ~ "4TH",
            str_detect(firstname, "(?<=\\s)5TH\\.?$") ~ "5TH",
            str_detect(firstname, "(?<=\\s)6TH\\.?$") ~ "6TH",
            str_detect(firstname, "(?<=\\s)7TH\\.?$") ~ "7TH",
            str_detect(firstname, "(?<=\\s)8TH\\.?$") ~ "8TH",
            str_detect(firstname, "(?<=\\s)9TH\\.?$") ~ "9TH",
            TRUE ~ suffix),
        # The original suffixes aren't uppercase, convert them to upper
        suffix = str_to_upper(suffix),
        # Delete periods and commas from suffixes
        suffix = str_remove_all(suffix, "\\.|\\,"),
        # Delete suffixes from lastname col
        lastname =
          case_when(
            str_detect(lastname, "(?<=\\s)JR\\.?$") ~
              str_remove(lastname, "(?<=\\s)JR\\.?$"),
            str_detect(lastname, "(?<=\\s)SR\\.?$") ~
              str_remove(lastname, "(?<=\\s)SR\\.?$"),
            str_detect(lastname, "(?<=\\s)XX$") ~
              str_remove(lastname, "(?<=\\s)XX$"),
            str_detect(lastname, "(?<=\\s)XIX$") ~
              str_remove(lastname, "(?<=\\s)XIX$"),
            str_detect(lastname, "(?<=\\s)XVIII$") ~
              str_remove(lastname, "(?<=\\s)XVIII$"),
            str_detect(lastname, "(?<=\\s)XVII$") ~
              str_remove(lastname, "(?<=\\s)XVII$"),
            str_detect(lastname, "(?<=\\s)XVI$") ~
              str_remove(lastname, "(?<=\\s)XVI$"),
            str_detect(lastname, "(?<=\\s)XV$") ~
              str_remove(lastname, "(?<=\\s)XV$"),
            str_detect(lastname, "(?<=\\s)XIV$") ~
              str_remove(lastname, "(?<=\\s)XIV$"),
            str_detect(lastname, "(?<=\\s)XIII$") ~
              str_remove(lastname, "(?<=\\s)XIII$"),
            str_detect(lastname, "(?<=\\s)XII$") ~
              str_remove(lastname, "(?<=\\s)XII$"),
            str_detect(lastname, "(?<=\\s)XI$") ~
              str_remove(lastname, "(?<=\\s)XI$"),
            str_detect(lastname, "(?<=\\s)X$") ~
              str_remove(lastname, "(?<=\\s)X$"),
            str_detect(lastname, "(?<=\\s)IX$") ~
              str_remove(lastname, "(?<=\\s)IX$"),
            str_detect(lastname, "(?<=\\s)VIII$") ~
              str_remove(lastname, "(?<=\\s)VIII$"),
            str_detect(lastname, "(?<=\\s)VII$") ~
              str_remove(lastname, "(?<=\\s)VII$"),
            str_detect(lastname, "(?<=\\s)VI$") ~
              str_remove(lastname, "(?<=\\s)VI$"),
            str_detect(lastname, "(?<=\\s)V$") ~
              str_remove(lastname, "(?<=\\s)V$"),
            str_detect(lastname, "(?<=\\s)IV$") ~
              str_remove(lastname, "(?<=\\s)IV$"),
            str_detect(lastname, "(?<=\\s)III$") ~
              str_remove(lastname, "(?<=\\s)III$"),
            str_detect(lastname, "(?<=\\s)II$") ~
              str_remove(lastname, "(?<=\\s)II$"),
            str_detect(lastname, "(?<=\\s)I$") ~
              str_remove(lastname, "(?<=\\s)I$"),
            str_detect(lastname, "(?<=\\s)1ST$") ~
              str_remove(lastname, "(?<=\\s)1ST$"),
            str_detect(lastname, "(?<=\\s)2ND$") ~
              str_remove(lastname, "(?<=\\s)2ND$"),
            str_detect(lastname, "(?<=\\s)3RD$") ~
              str_remove(lastname, "(?<=\\s)3RD$"),
            str_detect(lastname, "(?<=\\s)4TH$") ~
              str_remove(lastname, "(?<=\\s)4TH$"),
            str_detect(lastname, "(?<=\\s)5TH$") ~
              str_remove(lastname, "(?<=\\s)5TH$"),
            str_detect(lastname, "(?<=\\s)6TH$") ~
              str_remove(lastname, "(?<=\\s)6TH$"),
            str_detect(lastname, "(?<=\\s)7TH$") ~
              str_remove(lastname, "(?<=\\s)7TH$"),
            str_detect(lastname, "(?<=\\s)8TH$") ~
              str_remove(lastname, "(?<=\\s)8TH$"),
            str_detect(lastname, "(?<=\\s)9TH$") ~
              str_remove(lastname, "(?<=\\s)9TH$"),
            str_detect(lastname, "(?<=\\s)ESQ$") ~
              str_remove(lastname, "(?<=\\s)ESQ$"),
            TRUE ~ lastname),
        # Delete suffixes from firstname col
        firstname =
          case_when(
            str_detect(firstname, "(?<=\\s)JR\\.?$") ~
              str_remove(firstname, "(?<=\\s)JR\\.?$"),
            str_detect(firstname, "(?<=\\s)SR\\.?$") ~
              str_remove(firstname, "(?<=\\s)SR\\.?$"),
            str_detect(firstname, "(?<=\\s)XX$") ~
              str_remove(firstname, "(?<=\\s)XX$"),
            str_detect(firstname, "(?<=\\s)XIX$") ~
              str_remove(firstname, "(?<=\\s)XIX$"),
            str_detect(firstname, "(?<=\\s)XVIII$") ~
              str_remove(firstname, "(?<=\\s)XVIII$"),
            str_detect(firstname, "(?<=\\s)XVII$") ~
              str_remove(firstname, "(?<=\\s)XVII$"),
            str_detect(firstname, "(?<=\\s)XVI$") ~
              str_remove(firstname, "(?<=\\s)XVI$"),
            str_detect(firstname, "(?<=\\s)XV$") ~
              str_remove(firstname, "(?<=\\s)XV$"),
            str_detect(firstname, "(?<=\\s)XIV$") ~
              str_remove(firstname, "(?<=\\s)XIV$"),
            str_detect(firstname, "(?<=\\s)XIII$") ~
              str_remove(firstname, "(?<=\\s)XIII$"),
            str_detect(firstname, "(?<=\\s)XII$") ~
              str_remove(firstname, "(?<=\\s)XII$"),
            str_detect(firstname, "(?<=\\s)XI$") ~
              str_remove(firstname, "(?<=\\s)XI$"),
            str_detect(firstname, "(?<=\\s)X$") ~
              str_remove(firstname, "(?<=\\s)X$"),
            str_detect(firstname, "(?<=\\s)IX$") ~
              str_remove(firstname, "(?<=\\s)IX$"),
            str_detect(firstname, "(?<=\\s)VIII$") ~
              str_remove(firstname, "(?<=\\s)VIII$"),
            str_detect(firstname, "(?<=\\s)VII$") ~
              str_remove(firstname, "(?<=\\s)VII$"),
            str_detect(firstname, "(?<=\\s)VI$") ~
              str_remove(firstname, "(?<=\\s)VI$"),
            str_detect(firstname, "(?<=\\s)V$") ~
              str_remove(firstname, "(?<=\\s)V$"),
            str_detect(firstname, "(?<=\\s)IV$") ~
              str_remove(firstname, "(?<=\\s)IV$"),
            str_detect(firstname, "(?<=\\s)III$") ~
              str_remove(firstname, "(?<=\\s)III$"),
            str_detect(firstname, "(?<=\\s)II$") ~
              str_remove(firstname, "(?<=\\s)II$"),
            str_detect(firstname, "(?<=\\s)I$") ~
              str_remove(firstname, "(?<=\\s)I$"),
            str_detect(firstname, "(?<=\\s)1ST$") ~
              str_remove(firstname, "(?<=\\s)1ST$"),
            str_detect(firstname, "(?<=\\s)2ND$") ~
              str_remove(firstname, "(?<=\\s)2ND$"),
            str_detect(firstname, "(?<=\\s)3RD$") ~
              str_remove(firstname, "(?<=\\s)3RD$"),
            str_detect(firstname, "(?<=\\s)4TH$") ~
              str_remove(firstname, "(?<=\\s)4TH$"),
            str_detect(firstname, "(?<=\\s)5TH$") ~
              str_remove(firstname, "(?<=\\s)5TH$"),
            str_detect(firstname, "(?<=\\s)6TH$") ~
              str_remove(firstname, "(?<=\\s)6TH$"),
            str_detect(firstname, "(?<=\\s)7TH$") ~
              str_remove(firstname, "(?<=\\s)7TH$"),
            str_detect(firstname, "(?<=\\s)8TH$") ~
              str_remove(firstname, "(?<=\\s)8TH$"),
            str_detect(firstname, "(?<=\\s)9TH$") ~
              str_remove(firstname, "(?<=\\s)9TH$"),
            str_detect(firstname, "(?<=\\s)ESQ$") ~
              str_remove(firstname, "(?<=\\s)ESQ$"),
            TRUE ~ firstname),
        # Remove anything that's not a letter or number from middle initial col
        # Preserve numbers in case of a frame shift
        middle = ifelse(str_detect(middle, "[^A-Z|0-9]"), NA, middle),
        # Do a little P.O. Box string cleaning
        address =
          case_when(
            str_detect(address, "P.O.BOX ") ~
              str_replace(address, "P.O.BOX ", "P.O. BOX "),
            str_detect(address, "PO BOX ") ~
              str_replace(address, "PO BOX ", "P.O. BOX "),
            TRUE ~ address),
        # Zip code correction
        zip =
          # Remove ending hyphen from zip codes with 5 digits
          ifelse(str_detect(zip, "\\-$"), str_remove(zip, "\\-"), zip),
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
            zip)) %>%
      # Delete white space around strings again
      mutate_all(str_trim)

    return(cleaned_x)
  }
