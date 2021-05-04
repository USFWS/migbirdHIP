#' Check state strata
#'
#' After cleaning the data with \code{\link{clean}}, ensure there are no new strata that have been introduced by a state to the species groups.
#'
#' @importFrom dplyr %>%
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
#'
#' @param x The object created after cleaning data with \code{\link{clean}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

strataCheck <-
  function(x){

    # Create a tibble that contains all of the possible strata for a spp in a
    # state based on the current hip_bags_ref
    strata_by_state <-
      hip_bags_ref %>%
      select(state, spp, stateBagValue) %>%
      distinct() %>%
      filter(!is.na(stateBagValue)) %>%
      group_by(state, spp) %>%
      mutate(normal_strata = paste(stateBagValue, collapse = ", ")) %>%
      ungroup() %>%
      select(-stateBagValue) %>%
      distinct()

    # Do any species strata in the HIP data fall outside what is expected in the
    # hip_bags_ref?
    strata_x <-
      x %>%
      select(dl_state, ducks_bag:seaducks) %>%
      group_by(dl_state) %>%
      pivot_longer(
        cols = !contains("dl"),
        names_to = "spp",
        values_to = "state_strata") %>%
      ungroup() %>%
      distinct() %>%
      arrange(dl_state, spp) %>%
      left_join(
        hip_bags_ref %>%
          select(-FWSstratum) %>%
          rename(dl_state = state) %>%
          mutate(
            state_strata = as.character(stateBagValue),
            stateBagValue  = as.character(stateBagValue)),
        by = c("dl_state", "spp", "state_strata")
      ) %>%
      filter(is.na(stateBagValue)) %>%
      select(-stateBagValue) %>%
      left_join(
        strata_by_state %>%
          rename(dl_state = state),
        by = c("dl_state", "spp")) %>%
      arrange(desc(normal_strata))

    if(nrow(strata_x) > 0){
      return(strata_x)}
    else{
      message("No strata abnormalities detected.")
    }

  }
