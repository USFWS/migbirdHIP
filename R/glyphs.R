#' Find non-UTF-8 glyphs/characters in a field
#'
#' The internal \code{glyphFinder} function pulls non-UTF-8 characters in a field.
#'
#' @importFrom dplyr select
#' @importFrom rlang sym
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom stringr str_detect
#'
#' @param raw_data The tibble created after reading in data with \code{\link{read_hip}}
#' @param field Field that should be checked for non-UTF-8 characters. One of the fields from the following list may be supplied:
#' \itemize{
#' \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks, registration_yr, email}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

glyphFinder <-
  function(raw_data, field){

    raw_data |>
      select(source_file, record_key, !!sym(field)) |>
      mutate(check = is.na(iconv(!!sym(field), "UTF-8", "UTF-8"))) |>
      filter(check == TRUE) |>
      arrange(!!sym(field)) |>
      select(-check)
  }

#' Find non-UTF-8 glyphs/characters in any field
#'
#' Pull and view any non-UTF-8 characters in the raw data. This function iterates \code{\link{glyphFinder}} over the entire tibble.
#'
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#'
#' @inheritParams clean
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

glyphCheck <-
  function(raw_data) {

    checked <-
      map(
        names(raw_data),
        \(x) glyphFinder(raw_data, x) |>
          rename(value = !!sym(x)) |>
          mutate(
            field = x,
            value = as.character(value)) |>
          relocate(field, .before = "value")
        ) |>
      list_rbind() |>
      filter(!is.na(value)) |>
      arrange(source_file)

    if(nrow(checked) > 0) {
      message("Non-UTF8 characters detected.")
      return(checked)
    } else {
      message("All characters are UTF-8")
    }

  }

