#' Assign flyway as new field
#'
#' Internal helper function that assigns a new field for flyway name (e.g.,
#' "Atlantic Flyway") to a tibble containing an existing field for state
#' abbreviations (e.g., "MA").
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang sym
#' @importFrom rlang :=
#'
#' @param x Tibble containing a field with two-letter state abbreviation
#' @param state_field_name Name of field with two-letter state abbreviations
#' @param flyway_field_name Name to use for flyway column assignment
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#'
#' @family helper functions

assignFlyway <-
  function(x, state_field_name, flyway_field_name) {
    x |>
      mutate(
        !!sym(flyway_field_name) :=
          case_when(
            !!sym(state_field_name) %in% REF_STATES_AF ~ "Atlantic Flyway",
            !!sym(state_field_name) %in% REF_STATES_MF ~ "Mississippi Flyway",
            !!sym(state_field_name) %in% REF_STATES_CF ~ "Central Flyway",
            !!sym(state_field_name) %in% REF_STATES_PF ~ "Pacific Flyway",
            .unmatched = "error")
      )
  }
