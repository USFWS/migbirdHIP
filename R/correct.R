#' Correct data
#'
#' After flagging errors in the data with \code{\link{proof}}, attempt corrections in all fields. Errors that cannot be programmatically corrected will be reported for manual correction.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove
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

    # Fail if incorrect year supplied
    stopifnot("Error: `year` parameter must be numeric." = is.numeric(year))
    stopifnot("Error: Incorrect value supplied for `year` parameter. Please use a 4-digit year in the 2020s, e.g. 2024." = str_detect(year, "^202[0-9]{1}$"))

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
        # Email correction
        email =
          # Delete spaces, slashes, and commas; change to lowercase
          tolower(str_remove_all(email, "( |\\/|\\,)")),
        email =
          # Delete ! from domain part
          ifelse(
            str_detect(email, "\\!+(?!.*\\@.*)"),
            str_remove_all(email, "\\!+(?!.*\\@.*)"),
            email),
        email =
          # Fix multiple @
          ifelse(
            str_detect(email, "\\@\\@+"),
            str_replace(email, "\\@\\@+", "\\@"),
            email),
        email =
          # Fix multiple .
          ifelse(
            str_detect(email, "\\.\\.+"),
            str_replace(email, "\\.\\.+", "\\."),
            email),
        email =
          # Delete dot when it is first character in local part
          ifelse(
            str_detect(email, "^\\."),
            str_remove(email, "^\\."),
            email),
        email =
          # Delete dot when it is last character in local part
          ifelse(
            str_detect(email, "\\.(?=\\@)"),
            str_remove(email, "\\.(?=\\@)"),
            email),
        email =
          # Delete dot when it is last character
          ifelse(
            str_detect(email, "\\.$"),
            str_remove(email, "\\.$"),
            email),
        email =
          # Delete hyphen when it is first character in domain part
          ifelse(
            str_detect(email, "(?<=\\@)\\-"),
            str_remove(email, "(?<=\\@)\\-"),
            email),
        email =
          # Correct top level domain
          case_when(
            # gmail
            str_detect(email, "(?<=\\@)gmail\\.(co|net|edu|org)$") ~
              str_replace(email, "gmail\\.(co|net|edu|org)$", "gmail.com"),
            # ATT
            str_detect(email, "(?<=\\@)att\\.(com|org)$") ~
              str_replace(email, "att\\.(com|org)$", "att.net"),
            # Comcast
            str_detect(email, "(?<=\\@)comcast\\.(com|org)$") ~
              str_replace(email, "comcast\\.(com|org)$", "comcast.net"),
            # iCloud
            str_detect(email, "(?<=\\@)icloud\\.(net|org)$") ~
              str_replace(email, "icloud\\.(net|org)$", "icloud.com"),
            TRUE ~ email
            ),
        email =
          # Correct top level domain typos
          case_when(
            # .com.com (2 or more)
            str_detect(email, "(?<=\\.)com(\\.com)+$") ~
              str_replace(email, "com(\\.com)+$", "com"),
            # .comcom (2 or more)
            str_detect(email, "(?<=\\.)com(com)+$") ~
              str_replace(email, "com(com)+$", "com"),
            # .con
            str_detect(email, "(?<=\\.)con$") ~
              str_replace(email, "con$", "com"),
            # .ccom
            str_detect(email, "(?<=\\.)ccom$") ~
              str_replace(email, "ccom$", "com"),
            # .coom
            str_detect(email, "(?<=\\.)coom$") ~
              str_replace(email, "coom$", "com"),
            # .comm
            str_detect(email, "(?<=\\.)comm$") ~
              str_replace(email, "comm$", "com"),
            # .c0m
            str_detect(email, "(?<=\\.)c0m$") ~
              str_replace(email, "c0m$", "com"),
            # .ocm
            str_detect(email, "(?<=\\.)ocm$") ~
              str_replace(email, "ocm$", "com"),
            # .cm
            str_detect(email, "(?<=\\.)cm$") ~
              str_replace(email, "cm$", "com"),
            # .om
            str_detect(email, "(?<=\\.)om$") ~
              str_replace(email, "om$", "com"),
            # .cim
            str_detect(email, "(?<=\\.)cim$") ~
              str_replace(email, "cim$", "com"),
            # .common
            str_detect(email, "(?<=\\.)common$") ~
              str_replace(email, "common$", "com"),
            TRUE ~ email
          ),
        email =
          # Add in missing top level domain endings
          case_when(
            str_detect(email, "\\@gmail$") ~
              str_replace(email, "\\@gmail$", "\\@gmail\\.com"),
            str_detect(email, "\\@yahoo$") ~
              str_replace(email, "\\@yahoo$", "\\@yahoo\\.com"),
            str_detect(email, "\\@hotmail$") ~
              str_replace(email, "\\@hotmail$", "\\@hotmail\\.com"),
            str_detect(email, "\\@aol$") ~
              str_replace(email, "\\@aol$", "\\@aol\\.com"),
            str_detect(email, "\\@icloud$") ~
              str_replace(email, "\\@icloud$", "\\@icloud\\.com"),
            str_detect(email, "\\@comcast$") ~
              str_replace(email, "\\@comcast$", "\\@comcast\\.net"),
            str_detect(email, "\\@outlook$") ~
              str_replace(email, "\\@outlook$", "\\@outlook\\.com"),
            str_detect(email, "\\@sbcglobal$") ~
              str_replace(email, "\\@sbcglobal$", "\\@sbcglobal\\.net"),
            str_detect(email, "\\@att$") ~
              str_replace(email, "\\@att$", "\\@att\\.net"),
            str_detect(email, "\\@msn$") ~
              str_replace(email, "\\@msn$", "\\@msn\\.com"),
            str_detect(email, "\\@live$") ~
              str_replace(email, "\\@live$", "\\@live\\.com"),
            str_detect(email, "\\@bellsouth$") ~
              str_replace(email, "\\@bellsouth$", "\\@bellsouth\\.net"),
            str_detect(email, "\\@charter$") ~
              str_replace(email, "\\@charter$", "\\@charter\\.net"),
            str_detect(email, "\\@ymail$") ~
              str_replace(email, "\\@ymail$", "\\@ymail\\.com"),
            str_detect(email, "\\@me$") ~
              str_replace(email, "\\@me$", "\\@me\\.com"),
            str_detect(email, "\\@verizon$") ~
              str_replace(email, "\\@verizon$", "\\@verizon\\.net"),
            str_detect(email, "\\@cox$") ~
              str_replace(email, "\\@cox$", "\\@cox\\.net"),
            str_detect(email, "\\@earthlink$") ~
              str_replace(email, "\\@earthlink$", "\\@earthlink\\.net"),
            str_detect(email, "\\@protonmail$") ~
              str_replace(email, "\\@protonmail$", "\\@protonmail\\.com"),
            str_detect(email, "\\@pm$") ~
              str_replace(email, "\\@pm$", "\\@pm\\.me"),
            str_detect(email, "\\@mail$") ~
              str_replace(email, "\\@mail$", "\\@mail\\.com"),
            str_detect(email, "\\@duck$") ~
              str_replace(email, "\\@duck$", "\\@duck\\.com"),
            str_detect(email, "\\@ducks$") ~
              str_replace(email, "\\@ducks$", "\\@ducks\\.org"),
            TRUE ~ email
          ),
        email =
          # Add period(s) to top level domains if missing
          case_when(
            str_detect(email, "(?<!\\.)com$") ~
              str_replace(email, "com$", "\\.com"),
            str_detect(email, "(?<!\\.)net$") ~
              str_replace(email, "net$", "\\.net"),
            str_detect(email, "(?<!\\.)edu$") ~
              str_replace(email, "edu$", "\\.edu"),
            str_detect(email, "(?<!\\.)gov$") ~
              str_replace(email, "gov$", "\\.gov"),
            str_detect(email, "(?<!\\.)org$") ~
              str_replace(email, "org$", "\\.org"),
            str_detect(email, "\\@navymil$") ~
              str_replace(email, "navymil$", "navy\\.mil"),
            str_detect(email, "\\@usnavymil$") ~
              str_replace(email, "usnavymil$", "us\\.navy\\.mil"),
            str_detect(email, "\\@usafmil$") ~
              str_replace(email, "usafmil$", "us\\.af\\.mil"),
            str_detect(email, "\\@mailmil$") ~
              str_replace(email, "mailmil$", "mail\\.mil"),
            str_detect(email, "\\@armymil$") ~
              str_replace(email, "armymil$", "army\\.mil"),
            str_detect(email, "\\@usarmymil$") ~
              str_replace(email, "usarmymil$", "us\\.army\\.mil"),
            str_detect(email, "\\@usacearmymil$") ~
              str_replace(email, "usacearmymil$", "us\\.ace\\.army\\.mil"),
            TRUE ~ email
          )
      )

    # Re-run the proof script to get an updated errors column
    corrproof_bag_x <- suppressMessages(proof(corrected_x, year = year))

    return(corrproof_bag_x)

  }
