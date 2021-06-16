#' Modify corrected data table format and write as csv
#'
#' After correcting errors in the data with \code{\link{correct}}, this final step will shape up the dataframe into a format ready for the database, and write the data to csv.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
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
      select(-c("dl_cycle", "dl_key", "record_key", "errors"))

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
        na = "")
    }
  }
