#' Modify corrected data table format and write as csv
#'
#' After correcting errors in the data with \code{\link{correct}}, this final step will shape up the dataframe into a format ready for the database, and write the data to csv.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom lubridate ymd
#' @importFrom utils write.csv
#'
#' @param x The object created after correcting data with \code{\link{correct}}
#' @param path The file path and file name to write the final table
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

write_hip <-
  function(x, path){

    final_table <-
      x %>%
      # Exclude unwanted columns
      select(-c("dl_cycle", "dl_key", "record_key", "errors")) %>%
      # Rename columns to desired output
      rename(
        postal_code = zip,
        Q_ducks = ducks_bag,
        Q_geese = geese_bag,
        Q_doves = dove_bag,
        Q_woodcock = woodcock_bag,
        Q_coot_snipe = coots_snipe,
        Q_rail_gallinule = rails_gallinules,
        Q_cranes = cranes,
        Q_bt_pigeons = band_tailed_pigeon,
        Q_brant = brant,
        Q_seaducks = seaducks) %>%
      # Set standard codes for record_type field
      mutate(
        record_type =
          case_when(
            record_type == "hip" ~ "HIP",
            record_type == "hip-permit" ~ "HIP-PMT",
            record_type == "permit" ~ "PMT",
            record_type == "unsolved" ~ "unsolved",
            TRUE ~ NA_character_
          )
      ) %>%
      mutate(
        # Only include file names in the "source_file" field, not folder names.
        # The field theoretically shouldn't include DL folder names if the
        # package functions are run on a download-by-download basis, but this
        # will tidy the field values just in case it's needed
        source_file =
          ifelse(
            str_detect(source_file, "\\/"),
            str_remove(source_file, "^.+(?=\\/)"),
            source_file),
        # Convert dl_date to something that interprets as a date and not an
        # integer
        dl_date = ymd(dl_date)
      ) %>%
      mutate(
        # Do one last pass through source_file to remove the last "/", couldn't
        # pipe a dot above
        source_file = str_remove(source_file, "\\/")
      )


    # Check to see if there are bags or strata in the table
    if("strata" %in% names(x)){

      # Check to see if the data contains strata values or bag values
      # If strata values, return a message

      message(
        paste0("Strata values in table. Re-run correct function with option",
               " set to preserve bag values."))
    }
    else{

      # Write data to csv
      write.csv(
        final_table,
        file = path,
        row.names = FALSE,
        na = "",
        fileEncoding = "UTF-8")
    }
  }
