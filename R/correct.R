#' Correct data
#'
#' After flagging errors in the data with \code{\link{proof}}, attempt corrections in all fields. Errors that cannot be programmatically corrected will be reported for manual correction.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#'
#' @param proofed_data The object created after error flagging data with \code{\link{proof}}
#' @param year The year in which the Harvest Information Program data were collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

correct <-
  function(proofed_data, year){

    # Fail if incorrect year supplied
    stopifnot("Error: `year` parameter must be numeric." = is.numeric(year))
    stopifnot(
      "Error: For `year`, use a 4-digit year in the 2020s, e.g. 2024." =
        str_detect(year, "^202[0-9]{1}$"))

    corrected_data <-
      proofed_data |>
      # Change NAs in errors col to "none" so that str_detect functions work
      mutate(errors = ifelse(is.na(errors), "none", errors)) |>
      # Title correction (change to NA if error detected)
      correctTitle() |>
      # Suffix correction (change to NA if error detected)
      correctSuffix() |>
      # Correct middle initial (change to NA if error detected)
      correctMiddleInitial() |>
      # Change "none"s back to NAs in errors col
      mutate(errors = ifelse(errors == "none", NA, errors)) |>
      # Email correction
      correctEmail()

    # Re-run the proof script to get an updated errors column
    corrected_proofed <- suppressMessages(proof(corrected_data, year = year))

    return(corrected_proofed)

  }

#' Correct title
#'
#' The internal \code{correctTitle} function is used inside of \code{\link{correct}} to change the value(s) in the title field to NA if an error is detected.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#'
#' @param proofed_data The object created after error flagging data with \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

correctTitle <-
  function(proofed_data) {

    # Change title to NA if error detected
    proofed_data |>
      mutate(title = ifelse(str_detect(errors, "title"), NA, title))

  }

#' Correct suffix
#'
#' The internal \code{correctSuffix} function is used inside of \code{\link{correct}} to change the value(s) in the suffix field to NA if an error is detected.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#'
#' @param proofed_data The object created after error flagging data with \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

correctSuffix <-
  function(proofed_data) {

    # Change suffix to NA if error detected
    proofed_data |>
      mutate(suffix = ifelse(str_detect(errors, "suffix"), NA, suffix))

  }

#' Correct middle initials
#'
#' The internal \code{correctMiddleInitial} function changes non-alphabetic characters in the middle initial column to NA.
#'
#' @importFrom stringr str_detect
#'
#' @param proofed_data The object created after error flagging data with \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

correctMiddleInitial <-
  function(proofed_data) {

    # Change any character that's not a letter to NA in the from middle
    # initial field
    proofed_data |>
      mutate(middle = ifelse(str_detect(errors, "middle"), NA, middle))

  }

#' Correct email
#'
#' The internal \code{correctEmail} function is used inside of \code{\link{correct}} to ...
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#'
#' @param proofed_data The object created after error flagging data with \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

correctEmail <-
  function(proofed_data) {

    # Email correction
    email_corrected <-
      proofed_data |>
        mutate(
          # Delete spaces, slashes, and commas; change to lowercase
          email =
            tolower(str_remove_all(email, "( |\\/|\\,)")),
          # Delete ! from domain part
          email =
            ifelse(
              str_detect(email, "\\!+(?!.*\\@.*)"),
              str_remove_all(email, "\\!+(?!.*\\@.*)"),
              email),
          # Fix multiple @
          email =
            ifelse(
              str_detect(email, "\\@\\@+"),
              str_replace(email, "\\@\\@+", "\\@"),
              email),
          # Fix multiple .
          email =
            ifelse(
              str_detect(email, "\\.\\.+"),
              str_replace(email, "\\.\\.+", "\\."),
              email),
          # Delete dot when it is first character in local part
          email =
            ifelse(
              str_detect(email, "^\\."),
              str_remove(email, "^\\."),
              email),
          # Delete dot when it is last character in local part
          email =
            ifelse(
              str_detect(email, "\\.(?=\\@)"),
              str_remove(email, "\\.(?=\\@)"),
              email),
          # Delete dot when it is last character
          email =
            ifelse(
              str_detect(email, "\\.$"),
              str_remove(email, "\\.$"),
              email),
          # Delete hyphen when it is first character in domain part
          email =
            ifelse(
              str_detect(email, "(?<=\\@)\\-"),
              str_remove(email, "(?<=\\@)\\-"),
              email),
          # Correct top level domain
          email =
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
          # Correct top level domain typos
          email =
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
          # Add in missing top level domain endings
          email =
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
          # Add period(s) to top level domains if missing
          email =
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
    return(email_corrected)
  }
