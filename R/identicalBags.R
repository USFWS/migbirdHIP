#' Check for identical bag columns
#'
#' Check to make sure the data from each file don't have any columns that are exactly the same.
#'
#' @importFrom dplyr select
#' @importFrom dplyr matches
#' @importFrom tidyr expand_grid
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr contains
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr distinct
#' @importFrom dplyr group_split
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @importFrom stringr str_extract
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#'
#' @param x A HIP data tibble
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

identicalBags <-
  function(x) {

    # Get a list of species cols
    spp <-
      x |>
      select(
        matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) |>
      names()

    # Get species column combinations
    spp_combos <-
      expand_grid(
        spp1 = spp,
        spp2 = spp) |>
      filter(spp1 != spp2) |>
      mutate(id = row_number()) |>
      pivot_longer(cols = contains("spp")) |>
      arrange(value) |>
      group_by(id) |>
      mutate(name = c("s1", "s2")) |>
      ungroup() |>
      pivot_wider() |>
      distinct(s1, s2)

    # Split dataframe into a list by source_file
    split_df <-
      x |>
      group_by(source_file) |>
      group_split()

    # Check to see if any 2 cols from any 1 source_file are identical
    checked_cols <-
      map(
        1:length(split_df),
        function(x){
          map(
            1:nrow(spp_combos),
            function(y) {
              v1 <- pull(split_df[[x]], spp_combos$s1[[y]])
              v2 <- pull(split_df[[x]], spp_combos$s2[[y]])
              if(identical(v1, v2) == TRUE) {
                tibble(
                  source_file = unique(split_df[[x]]$source_file),
                  spp1 = spp_combos$s1[[y]],
                  spp2 = spp_combos$s2[[y]],
                  n = length(v1),
                  value = paste0(unique(v1), collapse = ", "))
              }
            }) |>
            list_rbind()
        }) |>
        list_rbind() |>
        mutate(dl_state = str_extract(source_file, "^[A-Z]{2}")) |>
        # Keep only the spp groups with more than 1 possible bag value
        # i.e. filter out "no season" species/states
        left_join(
          hip_bags_ref |>
            select(-FWSstratum) |>
            group_by(state, spp) |>
            filter(n() == 1) |>
            ungroup() |>
            rename(
              dl_state = state,
              value = stateBagValue,
              spp1 = spp) |>
            mutate(
              value = as.character(value),
              flag1 = "no season"),
          by = c("dl_state", "spp1", "value")
        ) |>
        left_join(
          hip_bags_ref |>
            select(-FWSstratum) |>
            group_by(state, spp) |>
            filter(n() == 1) |>
            ungroup() |>
            rename(
              dl_state = state,
              value = stateBagValue,
              spp2 = spp) |>
            mutate(
              value = as.character(value),
              flag2 = "no season"),
          by = c("dl_state", "spp2", "value")
        ) |>
        select(-dl_state) |>
        # Filter out any state/species that do not have a season
        filter(is.na(flag1) & is.na(flag2)) |>
        select(-c("flag1", "flag2"))

    if(nrow(checked_cols) > 0) {
      return(checked_cols)
    } else {
      message("Data are good to go! No identical columns detected.")}
  }
