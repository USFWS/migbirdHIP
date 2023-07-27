#' Check state strata
#'
#' After fixing the data with \code{\link{fixDuplicates}}, ensure there are no new strata that have been introduced by a state to the species groups.
#'
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom dplyr n
#' @importFrom purrr list_rbind
#'
#' @param x The object created after fixing data with \code{\link{fixDuplicates}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

strataCheck <-
  function(x){

    # Create a tibble that contains all of the possible strata for a spp in a
    # state based on the current hip_bags_ref
    strata_by_state <-
      hip_bags_ref |>
      distinct(state, spp, stateBagValue) |>
      filter(!is.na(stateBagValue)) |>
      group_by(state, spp) |>
      mutate(normal_strata = paste(stateBagValue, collapse = ", ")) |>
      ungroup() |>
      select(-stateBagValue) |>
      distinct()

    # Do any species strata in the HIP data fall outside what is expected in the
    # hip_bags_ref?
    strata_x <-
      x |>
      select(dl_state, ducks_bag:seaducks) |>
      group_by(dl_state) |>
      pivot_longer(
        cols = !contains("dl"),
        names_to = "spp",
        values_to = "state_strata") |>
      ungroup() |>
      distinct() |>
      arrange(dl_state, spp) |>
      left_join(
        hip_bags_ref |>
          select(-FWSstratum) |>
          rename(dl_state = state) |>
          mutate(
            state_strata = as.character(stateBagValue),
            stateBagValue  = as.character(stateBagValue)),
        by = c("dl_state", "spp", "state_strata")
      ) |>
      filter(is.na(stateBagValue)) |>
      select(-stateBagValue) |>
      left_join(
        strata_by_state |>
          rename(dl_state = state),
        by = c("dl_state", "spp")) |>
      arrange(desc(normal_strata))

    if(nrow(strata_x) > 0) {
      return(
        strata_x |>
          left_join(
            map(
              1:nrow(strata_x),
              ~x |>
                select(dl_state, sym(strata_x[[.x,2]])) |>
                filter(dl_state == strata_x[[.x,1]]) |>
                mutate(n_state = n()) |>
                filter(!!sym(strata_x[[.x,2]]) == strata_x[[.x,3]]) |>
                mutate(
                  n_bad_strata = n(),
                  spp = strata_x[[.x,2]]) |>
                select(-sym(strata_x[[.x,2]])) |>
                mutate(prop = paste0(round(n_bad_strata/n_state, 2)*100,"%")) |>
                distinct(dl_state, spp, n = n_bad_strata, prop)) |>
              list_rbind(),
            by = c("dl_state", "spp")
          )
      )
    } else {
      message("No strata abnormalities detected.")
    }

  }
