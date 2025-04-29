#' Clean data
#'
#' After reading the data with \code{\link{read_hip}}, reformat and clean the HIP registrations.
#'
#' @param raw_data The object created after reading in data with \code{\link{read_hip}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

clean <-
  function(raw_data) {

    cleaned_data <-
      raw_data |>
      # Filter out any record if any bag value is not a 1-digit number
      nonDigitBagsFilter() |>
      # Filter out any record with all-NA or all-0 bag values
      naAndZeroBagsFilter() |>
      # Filter out records if firstname, lastname, city of residence, state of
      # residence, or date of birth are missing -- records discarded because
      # these are needed to identify individuals. Filter out any other
      # additional records if they are missing a value for email AND elements of
      # a physical address that are required to determine where to mail a
      # letter.
      missingPIIFilter() |>
      # Convert firstname, lastname, and suffix to upper case
      namesToUppercase() |>
      # Filter out any test records
      testRecordFilter() |>
      # Delete suffixes from the lastname field and/or firstname field and move
      # them to the suffix field. Catches values from 1-20 in Roman numerals and
      # numeric, excluding XVIII since the db limit is 4 characters. Delete
      # periods and commas from suffixes.
      moveSuffixes() |>
      # Remove ending hyphen from zip codes with 5 digits
      # Remove final 0 from zip codes with length of 10 digits
      # Insert a hyphen in continuous 9 digit zip codes
      # Insert a hyphen in 9 digit zip codes with a middle space
      # Remove trailing -0000
      # Remove trailing -___
      formatZip() |>
      # If any OR or WA hunt_mig_birds != 2 for presumed solo permit, change
      # hunt_mig_birds to 2
      inLinePermitDNHFix() |>
      # If any permit file states submitted a 2 for crane and/or
      # band_tailed_pigeon, change the 2 to a 0
      permitBagFix()

    # Check that the zip code for each address is associated with the correct
    # state
    zipCheck(cleaned_data)

    return(cleaned_data)
  }

#' Names to uppercase
#'
#' The internal \code{namesToUppercase} function converts name elements to uppercase for easier string cleaning.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_upper
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

namesToUppercase <-
  function(raw_data) {

    # Convert name elements to uppercase for easier string cleaning
    raw_data |>
      mutate(
        firstname = str_to_upper(firstname),
        lastname = str_to_upper(lastname),
        suffix = str_to_upper(suffix))
  }

#' Filter out non-digit bags
#'
#' The internal \code{nonDigitBagsFilter} function filters out any record if any bag value is not a 1-digit number.
#'
#' @importFrom dplyr filter
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

nonDigitBagsFilter <-
  function(raw_data) {

    # Filter out any record if any bag value is not a 1-digit number
    raw_data |> filter(!(!!LOGIC_NONDIGIT_BAGS))
  }

#' Filter out all-NA and all-zero bag records
#'
#' The internal \code{naAndZeroBagsFilter} function filters out records if they contain all-NA or all-zero bag values.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom dplyr all_of
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

naAndZeroBagsFilter <-
  function(raw_data) {

    # Filter out any record if any bag value is NA or "0"
    raw_data |> filter(!(!!LOGIC_ZERO_BAGS))
  }

#' Filter out test records
#'
#' The internal \code{testRecordFilter} function filters out records if they contain all-NA or all-zero bag values.
#'
#' @importFrom dplyr filter
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

testRecordFilter <-
  function(raw_data) {

    # Filter out test records
    raw_data |> filter(!(!!LOGIC_TEST_RECORD))
  }

#' Missing PII filter
#'
#' The internal \code{missingPIIFilter} function filters out HIP registrations that are missing critical pieces of contact information.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_all
#' @importFrom dplyr if_any
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

missingPIIFilter <-
  function(raw_data) {

    raw_data |>
      # Filter out records if firstname, lastname, city of residence, state of
      # residence, or date of birth are missing -- records discarded because
      # these are needed to identify individuals
      filter(
        !if_any(
          c("firstname", "lastname", "state", "birth_date"), \(x) is.na(x))) |>
      # Filter out any additional records if they are missing a value for email
      # AND elements of a physical address that are required to determine where
      # to mail a letter
      filter(!if_all(c("address", "email"), \(x) is.na(x))) |>
      filter(!if_all(c("city", "zip", "email"), \(x) is.na(x)))

  }

#' Move suffixes
#'
#' The internal \code{moveSuffixes} function moves suffixes from first name or last name columns into the suffix column and performs other cleaning steps. This function catches values from 1 to 20 in Roman numerals and numeric, excluding
# XVIII since the database limit is 4 characters.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_remove
#' @importFrom dplyr across
#' @importFrom dplyr contains
#' @importFrom stringr str_trim
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

moveSuffixes <-
  function(raw_data) {

    suffixes_moved <-
      raw_data |>
      mutate(
        # Extract suffixes from lastname and firstname cols to suffix col
        # Catches values from 1-20 in Roman numerals and numeric, excluding
        # XVIII since the db limit is 4 characters
        suffix =
          case_when(
            # Lastname
            str_detect(lastname, REF_SUFFIXES) ~
              str_extract(lastname, REF_SUFFIXES),
            # Firstname
            str_detect(firstname, REF_SUFFIXES) ~
              str_extract(firstname, REF_SUFFIXES),
            TRUE ~ suffix),
        # Delete periods and commas from suffixes
        suffix = str_remove_all(suffix, "\\.|\\,"),
        # Delete suffixes from lastname col (includes 1-20 in Roman numerals and
        # numeric, excluding XVIII since the db limit is 4 characters)
        lastname =
          ifelse(
            str_detect(lastname, REF_SUFFIXES),
            str_remove(lastname, REF_SUFFIXES),
            lastname),
        # Delete suffixes from firstname col (includes 1-20 in Roman numerals
        # and numeric, excluding XVIII since the db limit is 4 characters)
        firstname =
          ifelse(
            str_detect(firstname, REF_SUFFIXES),
            str_remove(firstname, REF_SUFFIXES),
            firstname)) |>
      # Delete white space around names due to moving the suffixes
      mutate(across(contains("name"), \(x) str_trim(x)))

    return(suffixes_moved)
  }

#' Format zip codes
#'
#' The internal \code{formatZip} function formats zip codes.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

formatZip <-
  function(raw_data) {

    zips_formatted <-
      raw_data |>
      # Zip code format corrections
      mutate(
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
            zip)
      )

    return(zips_formatted)
  }

#' In-line permit did-not-hunt fix
#'
#' The internal \code{inLinePermitDNHFix} function changes any presumed solo permit from OR or WA indicating "did not hunt" in the hunt_mig_birds field if one or more of the band_tailed_pigeon, brant, or seaducks fields indicate hunting.
#'
#' @importFrom dplyr mutate
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

inLinePermitDNHFix <-
  function(raw_data) {

    # If any OR or WA hunt_mig_birds != "2" for presumed solo permit, change
    # hunt_mig_birds to 2
    raw_data |>
      mutate(
        hunt_mig_birds =
          ifelse(
            !!LOGIC_INLINE_PMT_DNH,
            "2",
            hunt_mig_birds)
      )
  }

#' Check if zip codes are associated with the correct state
#'
#' The internal \code{zipCheck} function checks to see if zip codes in hunter addresses match the address state.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr reframe
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr distinct
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

zipCheck <-
  function(raw_data) {

    # Proof the zip codes -- are they associated with the correct states?
    zipcheck <-
      raw_data |>
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
        paste0("Warning: Zip codes detected that do not correspond to provided",
               " state of residence for >10% of a file and/or >100 records."))

      print(zipcheck)
    }
  }

#' Fix permit bag values
#'
#' The internal \code{permitBagFix} function is used inside of \code{\link{clean}} to edit bag values for states that submit permit files separately from HIP. If records from these states submit a "2" for the band_tailed_pigeon or crane field, they will be mistakenly identified as permit records. The \code{permitBagFix} function changes band_tailed_pigeon and/or crane "2" values to "0" so that they are classified as HIP records until permit files are received later in the hunting season.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

permitBagFix <-
  function(raw_data) {

    bad_bt_2s <-
      raw_data |>
      filter(
        dl_state %in%
          REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == "band_tailed_pigeon"] &
          band_tailed_pigeon == "2") |>
      count(dl_state)

    bad_cr_2s <-
      raw_data |>
      filter(
        dl_state %in%
          REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == "cranes"] &
          cranes == "2") |>
      count(dl_state)

    if(nrow(bad_bt_2s) > 0 | nrow(bad_cr_2s) > 0) {

      corrected_pmt_bags <-
        raw_data |>
        mutate(
          band_tailed_pigeon =
            ifelse(
              dl_state %in%
                REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == "band_tailed_pigeon"] &
                band_tailed_pigeon == "2",
              "0",
              band_tailed_pigeon
            ),
          cranes =
            ifelse(
              dl_state %in%
                REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == "cranes"] &
                cranes == "2",
              "0",
              cranes
            )
        )

      message("2s converted to 0s for permit file states:")
      print(
        bind_rows(
          bad_bt_2s |> mutate(spp = "band_tailed_pigeon"),
          bad_cr_2s |> mutate(spp = "cranes")
        )
      )

      return(corrected_pmt_bags)

    } else {
      message(
        paste0(
          "No 2s received for band_tailed_pigeon or crane from permit file",
          " states."))
      return(raw_data)
    }
  }

