#' Clean data
#'
#' After reading the data with \code{\link{read_hip}}, reformat and clean the
#' HIP registrations.
#'
#' @importFrom dplyr filter
#'
#' @param raw_data The object created after reading in data with
#'   \code{\link{read_hip}}
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
      filter(!(!!LOGIC_NONDIGIT_BAGS)) |>
      # Filter out any record with all-NA or all-0 bag values
      filter(!(!!LOGIC_ZERO_BAGS)) |>
      # Filter out records if firstname, lastname, state, or date of birth are
      # missing; records discarded because these are needed to identify
      # individuals and cannot be otherwise determined. Also, filter out records
      # if they are missing a value for email AND combinations of elements of a
      # physical address that are required to determine where to mail a letter.
      missingPIIFilter() |>
      # Convert firstname, lastname, and suffix to upper case
      namesToUppercase() |>
      # Filter out any test records
      filter(!(!!LOGIC_TEST_RECORD)) |>
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
      # If any permit file state submitted 2 for crane, change to 0
      cranePermitBagFix() |>
      # If any permit file state submitted 2 for band_tailed_pigeon, change to 0
      btpiPermitBagFix()

    # Check that the zip code for each address is associated with the correct
    # state
    zipCheck(cleaned_data)

    return(cleaned_data)
  }

#' Names to uppercase
#'
#' The internal \code{namesToUppercase} function converts name elements to
#' uppercase for easier string cleaning.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_upper
#' @importFrom rlang .data
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
        firstname = str_to_upper(.data$firstname),
        lastname = str_to_upper(.data$lastname),
        suffix = str_to_upper(.data$suffix))
  }

#' Missing PII filter
#'
#' The internal \code{missingPIIFilter} function filters out HIP registrations
#' that are missing critical pieces of contact information.
#'
#' @importFrom dplyr filter
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

missingPIIFilter <-
  function(raw_data) {

    # Filter out records if firstname, lastname, state, or date of birth are
    # missing; records discarded because these are needed to identify
    # individuals and cannot be otherwise determined. Also, filter out records
    # if they are missing a value for email AND combinations of elements of a
    # physical address that are required to determine where to mail a letter.
    raw_data |>
      filter(
        !(!!LOGIC_MISSING_PII |
            !!LOGIC_MISSING_ADDRESSES |
            !!LOGIC_MISSING_CITY_ZIP_EMAIL)
        )
  }

#' Move suffixes
#'
#' The internal \code{moveSuffixes} function moves suffixes from first name or
#' last name columns into the suffix column and performs other cleaning steps.
#' This function catches values from 1 to 20 in Roman numerals and numeric,
#' excluding XVIII since the database limit is 4 characters.
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
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

moveSuffixes <-
  function(raw_data) {

    raw_data |>
      mutate(
        # Extract suffixes from lastname and firstname cols to suffix col
        # Catches values from 1-20 in Roman numerals and numeric, excluding
        # XVIII since the db limit is 4 characters
        suffix =
          case_when(
            # Lastname
            str_detect(.data$lastname, REGEX_SUFFIX_SEARCH) ~
              str_extract(.data$lastname, REGEX_SUFFIX_SEARCH),
            # Firstname
            str_detect(.data$firstname, REGEX_SUFFIX_SEARCH) ~
              str_extract(.data$firstname, REGEX_SUFFIX_SEARCH),
            TRUE ~ .data$suffix),
        # Delete periods and commas from suffixes
        suffix = str_remove_all(.data$suffix, "\\.|\\,"),
        # Delete suffixes from lastname col (includes 1-20 in Roman numerals and
        # numeric, excluding XVIII since the db limit is 4 characters)
        lastname =
          ifelse(
            str_detect(.data$lastname, REGEX_SUFFIX_SEARCH),
            str_remove(.data$lastname, REGEX_SUFFIX_SEARCH),
            .data$lastname),
        # Delete suffixes from firstname col (includes 1-20 in Roman numerals
        # and numeric, excluding XVIII since the db limit is 4 characters)
        firstname =
          ifelse(
            str_detect(.data$firstname, REGEX_SUFFIX_SEARCH),
            str_remove(.data$firstname, REGEX_SUFFIX_SEARCH),
            .data$firstname)) |>
      # Delete white space around names due to moving the suffixes
      mutate(across(contains("name"), \(x) str_trim(x)))
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
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

formatZip <-
  function(raw_data) {

    raw_data |>
      # Zip code format corrections
      mutate(
        zip =
          # Remove ending hyphen from zip codes with 5 digits
          ifelse(
            str_detect(.data$zip, "^[0-9]{5}\\-$"),
            str_remove(.data$zip, "\\-$"),
            .data$zip),
        zip =
          # Remove final 0 from zip codes with length of 10 digits
          ifelse(
            str_detect(.data$zip, "^[0-9]{10}$") &
              str_extract(.data$zip, "[0-9]{1}(?=$)") == "0",
            str_remove(.data$zip, "[0-9]{1}(?=$)"),
            .data$zip),
        zip =
          # Insert a hyphen in continuous 9 digit zip codes
          ifelse(
            str_detect(.data$zip, "^[0-9]{9}$"),
            paste0(
              str_extract(.data$zip, "^[0-9]{5}"),
              "-",
              str_extract(.data$zip, "[0-9]{4}$")),
            .data$zip),
        zip =
          # Insert a hyphen in 9 digit zip codes with a middle space
          ifelse(
            str_detect(.data$zip, "^[0-9]{5}\\s[0-9]{4}$"),
            str_replace(.data$zip, "\\s", "\\-"),
            .data$zip),
        zip =
          # Remove trailing -0000
          ifelse(
            str_detect(.data$zip, "\\-0000"),
            str_remove(.data$zip, "\\-0000"),
            .data$zip),
        zip =
          # Remove trailing -___
          ifelse(
            str_detect(.data$zip, "\\-\\_+"),
            str_remove(.data$zip, "\\-\\_+"),
            .data$zip)
      )
  }

#' In-line permit did-not-hunt fix
#'
#' The internal \code{inLinePermitDNHFix} function changes any presumed solo
#' permit from OR or WA indicating "did not hunt" in the hunt_mig_birds field if
#' one or more of the band_tailed_pigeon, brant, or seaducks fields indicate
#' hunting.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
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
            .data$hunt_mig_birds)
      )
  }

#' Check if zip codes are associated with the correct state
#'
#' The internal \code{zipCheck} function checks to see if zip codes in hunter
#' addresses match the address state.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr reframe
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr distinct
#' @importFrom rlang .data
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
        REF_ZIP_CODE |>
          distinct(zip = .data$zipcode, zipState = .data$state),
        by = "zip") |>
      select(c("source_file", "state", "zip", "zipState")) |>
      mutate(total_records = n(), .by = "source_file") |>
      filter(.data$state != .data$zipState) |>
      group_by(.data$source_file) |>
      reframe(
        n = n(),
        proportion = round(.data$n / .data$total_records, 2)) |>
      distinct() |>
      arrange(desc(.data$n)) |>
      filter(.data$n >= 100 | .data$proportion >= 0.1)

    # Error check: are any zip codes wrong?
    if (nrow(zipcheck) > 0) {
      message(
        paste(
          "Warning: Zip codes detected that do not correspond to provided",
          "state of residence for >10% of a file and/or >100 records.")
      )

      print(zipcheck)
    }
  }

#' Fix crane permit bag values
#'
#' The internal \code{cranePermitBagFix} function is used inside of
#' \code{\link{clean}} to edit bag values for states that submit permit files
#' separately from HIP. If records from these states submit a "2" for the crane
#' field, they will be mistakenly identified as permit records. This function
#' changes crane "2" values to "0" so that they are classified as HIP records
#' until permit files are received later in the hunting season.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

cranePermitBagFix <-
  function(raw_data) {

    bad_cr_2s <-
      raw_data |>
      filter(
        .data$dl_state %in%
          REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == "cranes"] &
          .data$cranes == "2") |>
      count(.data$dl_state)

    if (nrow(bad_cr_2s) > 0) {

      corrected_pmt_bags <-
        raw_data |>
        mutate(
          cranes =
            ifelse(
              .data$dl_state %in%
                REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == "cranes"] &
                .data$cranes == "2",
              "0",
              .data$cranes)
        )

      message(
        paste(
          "A total of", format.default(sum(bad_cr_2s$n), big.mark = ","),
          "2s converted to 0s for permit file states:")
      )
      print(bad_cr_2s |> mutate(spp = "cranes"))
      return(corrected_pmt_bags)

    } else {
      message("No 2s received for crane from permit file states.")
      return(raw_data)
    }
  }

#' Fix band-tailed pigeon permit bag values
#'
#' The internal \code{btpiPermitBagFix} function is used inside of
#' \code{\link{clean}} to edit bag values for states that submit permit files
#' separately from HIP. If records from these states submit a "2" for the
#' band_tailed_pigeon field, they will be mistakenly identified as permit
#' records. This function changes band_tailed_pigeon "2" values to "0" so that
#' they are classified as HIP records until permit files are received later in
#' the hunting season.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

btpiPermitBagFix <-
  function(raw_data) {

    short_btpi <- "band_tailed_pigeon"

    bad_bt_2s <-
      raw_data |>
      filter(
        .data$dl_state %in%
          REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == short_btpi] &
          .data$band_tailed_pigeon == "2") |>
      count(.data$dl_state)

    if (nrow(bad_bt_2s) > 0) {

      corrected_pmt_bags <-
        raw_data |>
        mutate(
          band_tailed_pigeon =
            ifelse(
              .data$dl_state %in%
                REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == short_btpi] &
                .data$band_tailed_pigeon == "2",
              "0",
              .data$band_tailed_pigeon)
        )

      message(
        paste(
          "A total of", format.default(sum(bad_bt_2s$n), big.mark = ","),
          "2s converted to 0s for permit file states:")
      )
      print(bad_bt_2s |> mutate(spp = short_btpi))
      return(corrected_pmt_bags)

    } else {
      message("No 2s received for band_tailed_pigeon from permit file states.")
      return(raw_data)
    }
  }
