#' Convert data to utf-8
#'
#' Convert text files from their original encoding to utf-8. The new file is added to the same directory as the original file.
#'
#' @param original The path to the original file that needs to be converted to utf-8
#' @param encoding The existing encoding of the original file
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

to_utf8 <-
  function(original, encoding){
    writeLines(
      iconv(
        readLines(original),
        from = encoding,
        to = "UTF8"),
      file(paste0(str_remove(original, "\\.txt"), "-utf8.txt"), encoding="UTF-8"))

    message("Reminder: Don't forget to move or delete the original file.")
  }
