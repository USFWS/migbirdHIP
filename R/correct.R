#' Correct data
#'
#' After flagging errors in the data with \code{\link{proof}}, attempt
#' corrections in all fields. Errors that cannot be programmatically corrected
#' will be reported for manual correction.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}}
#' @param year The year in which the Harvest Information Program data were
#'   collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

correct <-
  function(proofed_data, year){
    failyear(year)

    corrected_data <-
      proofed_data |>
      # Change NAs in errors col to "none" so that str_detect functions work
      mutate(errors = ifelse(is.na(.data$errors), "none", .data$errors)) |>
      # Title correction (change to NA if error detected)
      correctTitle() |>
      # Suffix correction (change to NA if error detected)
      correctSuffix() |>
      # Correct middle initial (change to NA if error detected)
      correctMiddleInitial() |>
      mutate(
        # Change "none"s back to NAs in errors col
        errors = ifelse(.data$errors == "none", NA, .data$errors),
        # Change email to lower case
        email = tolower(.data$email)) |>
      # Email correction
      correctEmail()

    # Re-run proof() to get an updated errors column
    corrected_proofed <- suppressMessages(proof(corrected_data, year = year))

    return(corrected_proofed)

  }

#' Correct title
#'
#' The internal \code{correctTitle} function is used inside of
#' \code{\link{correct}} to change the value(s) in the title field to NA if an
#' error is detected.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

correctTitle <-
  function(proofed_data) {

    # Change title to NA if error detected
    proofed_data |>
      mutate(title = ifelse(str_detect(.data$errors, "title"), NA, .data$title))

  }

#' Correct suffix
#'
#' The internal \code{correctSuffix} function is used inside of
#' \code{\link{correct}} to change the value(s) in the suffix field to NA if an
#' error is detected.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

correctSuffix <-
  function(proofed_data) {

    # Change suffix to NA if error detected
    proofed_data |>
      mutate(
        suffix = ifelse(str_detect(.data$errors, "suffix"), NA, .data$suffix))

  }

#' Correct middle initials
#'
#' The internal \code{correctMiddleInitial} function changes non-alphabetic
#' characters in the middle initial column to NA.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

correctMiddleInitial <-
  function(proofed_data) {

    # Change any character that's not a letter to NA in the from middle
    # initial field
    proofed_data |>
      mutate(
        middle = ifelse(str_detect(.data$errors, "middle"), NA, .data$middle))

  }

#' Correct email
#'
#' The internal \code{correctEmail} function is used inside of
#' \code{\link{correct}} to ...
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

correctEmail <-
  function(proofed_data) {

    proofed_data |>
      mutate(
        # Add in missing top level domain endings
        email =
          case_when(
            str_detect(.data$email, "\\@gmail$") ~
              str_replace(.data$email, "\\@gmail$", "\\@gmail\\.com"),
            str_detect(.data$email, "\\@yahoo$") ~
              str_replace(.data$email, "\\@yahoo$", "\\@yahoo\\.com"),
            str_detect(.data$email, "\\@hotmail$") ~
              str_replace(.data$email, "\\@hotmail$", "\\@hotmail\\.com"),
            str_detect(.data$email, "\\@aol$") ~
              str_replace(.data$email, "\\@aol$", "\\@aol\\.com"),
            str_detect(.data$email, "\\@icloud$") ~
              str_replace(.data$email, "\\@icloud$", "\\@icloud\\.com"),
            str_detect(.data$email, "\\@comcast$") ~
              str_replace(.data$email, "\\@comcast$", "\\@comcast\\.net"),
            str_detect(.data$email, "\\@outlook$") ~
              str_replace(.data$email, "\\@outlook$", "\\@outlook\\.com"),
            str_detect(.data$email, "\\@sbcglobal$") ~
              str_replace(.data$email, "\\@sbcglobal$", "\\@sbcglobal\\.net"),
            str_detect(.data$email, "\\@att$") ~
              str_replace(.data$email, "\\@att$", "\\@att\\.net"),
            str_detect(.data$email, "\\@msn$") ~
              str_replace(.data$email, "\\@msn$", "\\@msn\\.com"),
            str_detect(.data$email, "\\@live$") ~
              str_replace(.data$email, "\\@live$", "\\@live\\.com"),
            str_detect(.data$email, "\\@bellsouth$") ~
              str_replace(.data$email, "\\@bellsouth$", "\\@bellsouth\\.net"),
            str_detect(.data$email, "\\@charter$") ~
              str_replace(.data$email, "\\@charter$", "\\@charter\\.net"),
            str_detect(.data$email, "\\@ymail$") ~
              str_replace(.data$email, "\\@ymail$", "\\@ymail\\.com"),
            str_detect(.data$email, "\\@me$") ~
              str_replace(.data$email, "\\@me$", "\\@me\\.com"),
            str_detect(.data$email, "\\@verizon$") ~
              str_replace(.data$email, "\\@verizon$", "\\@verizon\\.net"),
            str_detect(.data$email, "\\@cox$") ~
              str_replace(.data$email, "\\@cox$", "\\@cox\\.net"),
            str_detect(.data$email, "\\@earthlink$") ~
              str_replace(.data$email, "\\@earthlink$", "\\@earthlink\\.net"),
            str_detect(.data$email, "\\@protonmail$") ~
              str_replace(.data$email, "\\@protonmail$", "\\@protonmail\\.com"),
            str_detect(.data$email, "\\@pm$") ~
              str_replace(.data$email, "\\@pm$", "\\@pm\\.me"),
            str_detect(.data$email, "\\@mail$") ~
              str_replace(.data$email, "\\@mail$", "\\@mail\\.com"),
            str_detect(.data$email, "\\@duck$") ~
              str_replace(.data$email, "\\@duck$", "\\@duck\\.com"),
            str_detect(.data$email, "\\@ducks$") ~
              str_replace(.data$email, "\\@ducks$", "\\@ducks\\.org"),
            TRUE ~ .data$email
          ),
        # Add period(s) to top level domains if missing
        email =
          case_when(
            str_detect(.data$email, "(?<!\\.)com$") ~
              str_replace(.data$email, "com$", "\\.com"),
            str_detect(.data$email, "(?<!\\.)net$") ~
              str_replace(.data$email, "net$", "\\.net"),
            str_detect(.data$email, "(?<!\\.)edu$") ~
              str_replace(.data$email, "edu$", "\\.edu"),
            str_detect(.data$email, "(?<!\\.)gov$") ~
              str_replace(.data$email, "gov$", "\\.gov"),
            str_detect(.data$email, "(?<!\\.)org$") ~
              str_replace(.data$email, "org$", "\\.org"),
            str_detect(.data$email, "\\@navymil$") ~
              str_replace(.data$email, "navymil$", "navy\\.mil"),
            str_detect(.data$email, "\\@usnavymil$") ~
              str_replace(.data$email, "usnavymil$", "us\\.navy\\.mil"),
            str_detect(.data$email, "\\@usafmil$") ~
              str_replace(.data$email, "usafmil$", "us\\.af\\.mil"),
            str_detect(.data$email, "\\@mailmil$") ~
              str_replace(.data$email, "mailmil$", "mail\\.mil"),
            str_detect(.data$email, "\\@armymil$") ~
              str_replace(.data$email, "armymil$", "army\\.mil"),
            str_detect(.data$email, "\\@usarmymil$") ~
              str_replace(.data$email, "usarmymil$", "us\\.army\\.mil"),
            str_detect(.data$email, "\\@usacearmymil$") ~
              str_replace(
                .data$email, "usacearmymil$", "us\\.ace\\.army\\.mil"),
            TRUE ~ .data$email
          )
      )
  }
