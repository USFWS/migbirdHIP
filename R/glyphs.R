#' Find non-UTF-8 glyphs/characters in a field
#'
#' Pull and view non-UTF-8 characters in a field when R throws error 'invalid UTF-8 byte sequence detected'.
#'
#' @importFrom dplyr select
#' @importFrom rlang sym
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#'
#' @param data The tibble created after reading in data with \code{\link{read_hip}}
#' @param field Field that should be checked for non-UTF-8 characters. One of the fields from the following list may be supplied:
#' \itemize{
#' \item title, firstname, middle, lastname, suffix, address, city, state, zip, birth_date, issue_date, hunt_mig_birds, ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks, registration_year, email}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

glyphFinder <-
  function(data, field){
    data |>
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
#' @importFrom purrr map_dfr
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#'
#' @param data The tibble created after reading in data with \code{\link{read_hip}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

glyphCheck <-
  function(data) {

    checked <-
      map_dfr(
        names(data),
        ~glyphFinder(data, .x) |>
          rename(value = !!sym(.x)) |>
          mutate(
            field = .x,
            value = as.character(value)) |>
          relocate(field, .before = "value")
      ) |>
      filter(!is.na(value)) |>
      arrange(source_file)

    if(nrow(checked) > 0) {
      message("Non-UTF8 characters detected.")
      return(checked)
    } else {
      message("All characters are UTF-8")
    }

  }

