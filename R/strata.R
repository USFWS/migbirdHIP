#' Check state strata
#'
#' After fixing the data with \code{\link{duplicateFix}}, ensure there are no new strata that have been introduced by a state to the species groups.
#'
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr anti_join
#' @importFrom dplyr inner_join
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom dplyr n
#' @importFrom purrr list_rbind
#'
#' @param deduplicated_data The object created after deduplicating data with \code{\link{duplicateFix}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

strataCheck <-
  function(deduplicated_data){

    # Filter out solo inline permits
    deduplicated_data <-
      deduplicated_data |>
      filter(
        !(dl_state == "OR" &
            ducks_bag == "0" &
            geese_bag == "0" &
            dove_bag == "0" &
            woodcock_bag == "0" &
            coots_snipe == "0" &
            rails_gallinules == "0" &
            (band_tailed_pigeon == "2" |
               brant == "2" |
               seaducks == "2")))

    # The following nested joins remove all values from hip_bags_ref that are
    # not 0 or 1 for states/species combinations in pmt_files (e.g. 2s are not
    # acceptable in regular HIP pre-processing)
    non_pmt_file_bags_ref <-
      anti_join(
        hip_bags_ref |>
          select(-FWSstratum) |>
          rename(dl_state = state) |>
          mutate(stateBagValue  = as.character(stateBagValue)),
        hip_bags_ref |>
          select(-FWSstratum) |>
          rename(dl_state = state) |>
          mutate(stateBagValue  = as.character(stateBagValue)) |>
          inner_join(pmt_files |> select(-value)) |>
          filter(!stateBagValue %in% c("0", "1"))
      )

    # Create a tibble that contains all of the possible strata for a spp in a
    # state based on the current hip_bags_ref
    strata_by_state <-
      non_pmt_file_bags_ref |>
      group_by(dl_state, spp) |>
      mutate(normal_strata = paste(stateBagValue, collapse = ", ")) |>
      ungroup() |>
      select(-stateBagValue) |>
      distinct()

    # Do any species strata in the HIP data fall outside what is expected in the
    # hip_bags_ref?
    strata_x <-
      deduplicated_data |>
      select(dl_state, all_of(ref_bagfields)) |>
      group_by(dl_state) |>
      pivot_longer(
        cols = !contains("dl"),
        names_to = "spp",
        values_to = "state_strata") |>
      ungroup() |>
      distinct() |>
      left_join(
        non_pmt_file_bags_ref |>
          mutate(state_strata = as.character(stateBagValue)),
        by = c("dl_state", "spp", "state_strata")
      ) |>
      filter(is.na(stateBagValue)) |>
      # Filter out permit file states with unexpected 0s (they were created by
      # strataFix) for btpi and cranes
      filter(
        !(dl_state %in%
            pmt_files$dl_state[pmt_files$spp == "band_tailed_pigeon"] &
          spp == "band_tailed_pigeon" &
          state_strata == "0")) |>
      filter(
        !(dl_state %in% pmt_files$dl_state[pmt_files$spp == "cranes"] &
            spp == "cranes" &
            state_strata == "0"))

    if(nrow(strata_x) > 0) {
      return(
        strata_x |>
          select(-stateBagValue) |>
          left_join(
            strata_by_state,
            by = c("dl_state", "spp")) |>
          arrange(desc(normal_strata)) |>
          left_join(
            map(
              1:nrow(strata_x),
              \(x) deduplicated_data |>
                select(dl_state, sym(strata_x[[x,2]])) |>
                filter(dl_state == strata_x[[x,1]]) |>
                mutate(n_state = n()) |>
                filter(!!sym(strata_x[[x,2]]) == strata_x[[x,3]]) |>
                mutate(
                  n_bad_strata = n(),
                  spp = strata_x[[x,2]]) |>
                select(-sym(strata_x[[x,2]])) |>
                mutate(
                  prop = paste0(round(n_bad_strata/n_state, 2)*100,"%"),
                  state_strata = strata_x[[x,3]]) |>
                distinct(dl_state, spp, state_strata, n = n_bad_strata, prop)
            ) |>
              list_rbind(),
            by = c("dl_state", "spp", "state_strata")
          )
      )
    } else {
      message("No strata abnormalities detected.")
    }

  }


