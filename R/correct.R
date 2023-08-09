#' Correct data
#'
#' After flagging errors in the data with \code{\link{proof}}, attempt corrections in all fields. Errors that cannot be programmatically corrected will be reported for manual correction.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove_all
#'
#' @param x The object created after error flagging data with \code{\link{proof}}
#' @param year The year in which the Harvest Information Program data were collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

correct <-
  function(x, year){

    corrected_x <-
      x |>
      mutate(
        # Change NAs in errors col to "none" so that str_detect functions work
        errors =
          ifelse(is.na(errors), "none", errors),
        # Title correction
        title =
          # Set to NA if title is flagged
          ifelse(str_detect(errors, "title"), NA, title),
        # Suffix correction
        suffix =
          # Set to NA if suffix is flagged
          ifelse(str_detect(errors, "suffix"), NA, suffix),
        # Change "none"s back to NAs in errors col
        errors =
          ifelse(errors == "none", NA, errors),
        # Email
        email =
          # Delete spaces
          str_remove_all(email, " "),
        email =
          # Delete slashes /
          str_remove_all(email, "\\/"),
        email =
          # Delete commas
          str_remove_all(email, "\\,"),
        email =
          # To lowercase
          tolower(email),
        email =
          # Fix multiple @
          ifelse(
            str_detect(email, "\\@\\@+"),
            str_replace(email, "\\@\\@+", "\\@"),
            email),
        email =
          # Add endings to common domains
          case_when(
            str_detect(email, "\\@gmail$") ~
              str_replace(email, "\\@gmail$", "\\@gmail\\.com"),
            str_detect(email, "\\@yahoo$") ~
              str_replace(email, "\\@yahoo$", "\\@yahoo\\.com"),
            str_detect(email, "\\@aol$") ~
              str_replace(email, "\\@aol$", "\\@aol\\.com"),
            str_detect(email, "\\@comcast$") ~
              str_replace(email, "\\@comcast$", "\\@comcast\\.net"),
            str_detect(email, "\\@verizon$") ~
              str_replace(email, "\\@verizon$", "\\@verizon\\.net"),
            str_detect(email, "\\@cox$") ~
              str_replace(email, "\\@cox$", "\\@cox\\.net"),
            str_detect(email, "\\@outlook$") ~
              str_replace(email, "\\@outlook$", "\\@outlook\\.com"),
            str_detect(email, "\\@hotmail$") ~
              str_replace(email, "\\@hotmail$", "\\@hotmail\\.com"),
            TRUE ~ email
          ),
        email =
          # Fix punctuation
          case_when(
            # Correct .ccom typos
            str_detect(email, "(?<=\\.)ccom$") ~
              str_replace(email, "ccom$", "com"),
            # Add period if missing
            str_detect(email, "(?<=[^\\.])com$") ~
              str_replace(email, "com$", "\\.com"),
            str_detect(email, "(?<=[^\\.])net$") ~
              str_replace(email, "net$", "\\.net"),
            str_detect(email, "(?<=[^\\.])edu$") ~
              str_replace(email, "edu$", "\\.edu"),
            str_detect(email, "(?<=[^\\.])gov$") ~
              str_replace(email, "gov$", "\\.gov"),
            str_detect(email, "(?<=[^\\.])org$") ~
              str_replace(email, "org$", "\\.org"),
            TRUE ~ email
          ),
        email =
          # Replace with NA
          case_when(
            # If there is no @
            !str_detect(email, "\\@") ~ NA_character_,
            # If the email is invalid
            str_detect(email, "noemail\\@") ~ NA_character_,
            str_detect(email, "^none@none$") ~ NA_character_,
            # If there are any remaining "NA" type strings
            str_detect(email, "n\\/a") ~ NA_character_,
            # If there is only an @
            str_detect(email, "^@$") ~ NA_character_,
            TRUE ~ email
          )
      )

    # Re-run the proof script to get an updated errors column
    corrproof_bag_x <- suppressMessages(proof(corrected_x, year = year))

    return(corrproof_bag_x)

  }
