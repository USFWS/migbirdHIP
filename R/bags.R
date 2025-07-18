#' Check bag values
#'
#' After fixing the data with \code{\link{duplicateFix}}, see if any unexpected
#' bag values were submitted.
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
#' @importFrom dplyr contains
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom rlang .data
#'
#' @param deduplicated_data The object created after deduplicating data with
#'   \code{\link{duplicateFix}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

bagCheck <-
  function(deduplicated_data) {

    # Filter out in-line permits
    deduplicated_data <-
      deduplicated_data |>
      filter(!(!!LOGIC_INLINE_PMT))

    # Reformat REF_BAGS
    mini_bags_ref <-
      REF_BAGS |>
      select(-"FWSstratum") |>
      rename(dl_state = "state") |>
      mutate(stateBagValue  = as.character(.data$stateBagValue))

    # The following nested joins remove all values that are not 0 or 1 from
    # REF_BAGS for state/species combinations in REF_PMT_FILES (e.g. 2s are
    # not acceptable in regular HIP pre-processing for CO cranes, so this
    # resulting tibble will only contain a line for CO cranes = 1). This rule is
    # also applied in internal function `permitBagFix()`, please refer to that
    # function if this comment is still unclear.
    non_pmt_file_bags_ref <-
      anti_join(
        mini_bags_ref,
        mini_bags_ref |>
          inner_join(
            REF_PMT_FILES |> select(-"value"),
            by = c("dl_state", "spp")) |>
          filter(!.data$stateBagValue %in% c("0", "1")),
        by = c("dl_state", "spp", "stateBagValue")
      )

    # Create a tibble that contains a column of vectors for all of the possible
    # bags and species in a state (based on non_pmt_file_bags_ref)
    bags_by_state <-
      non_pmt_file_bags_ref |>
      summarize(
        expected_bag_value = paste(.data$stateBagValue, collapse = ", "),
        .by = c("dl_state", "spp")
      )

    # Do any species bag values in the HIP data fall outside what is expected in
    # the REF_BAGS?
    bad_bag_values <-
      deduplicated_data |>
      select(c("dl_state", all_of(REF_FIELDS_BAG))) |>
      group_by(.data$dl_state) |>
      pivot_longer(
        cols = !contains("dl"),
        names_to = "spp",
        values_to = "bad_bag_value") |>
      ungroup() |>
      distinct() |>
      left_join(
        non_pmt_file_bags_ref |>
          mutate(bad_bag_value = .data$stateBagValue),
        by = c("dl_state", "spp", "bad_bag_value")
      ) |>
      filter(is.na(.data$stateBagValue)) |>
      # Filter out permit file states with unexpected 0s (they were created by
      # permitBagFix) for btpi and cranes
      filter(
        !(.data$dl_state %in%
            REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == "band_tailed_pigeon"] &
            .data$spp == "band_tailed_pigeon" &
            .data$bad_bag_value == "0")) |>
      filter(
        !(.data$dl_state %in%
            REF_PMT_FILES$dl_state[REF_PMT_FILES$spp == "cranes"] &
            .data$spp == "cranes" &
            .data$bad_bag_value == "0"))

    if (nrow(bad_bag_values) > 0) {

      bad_bag_values |>
        select(-"stateBagValue") |>
        left_join(
          bags_by_state,
          by = c("dl_state", "spp")) |>
        arrange(desc(.data$expected_bag_value)) |>
        left_join(
          map(
            seq_len(nrow(bad_bag_values)),
            \(x) summarizeBadBags(deduplicated_data, bad_bag_values, x)
            ) |>
            list_rbind(),
          by = c("dl_state", "spp", "bad_bag_value"))
    } else {
      message("No bag abnormalities detected.")
    }

  }

#' Summarize count and proportion of bad bag values
#'
#' The internal \code{filterOutOregonPermits} function calculates the count and
#' proportion of a bad bag value inside of \code{\link{bagCheck}}.
#'
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom dplyr n
#' @importFrom rlang .data
#'
#' @param deduplicated_data The object created after deduplicating data with
#'   \code{\link{duplicateFix}}
#' @param bad_bag_values Bad bag values df
#' @param x Row number
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

summarizeBadBags <-
  function(deduplicated_data, bad_bag_values, x) {
    deduplicated_data |>
      select(c("dl_state", bad_bag_values[[x, 2]])) |>
      filter(.data$dl_state == bad_bag_values[[x, 1]]) |>
      mutate(n_state = n()) |>
      filter(!!sym(bad_bag_values[[x, 2]]) == bad_bag_values[[x, 3]]) |>
      mutate(
        n_bad_bags = n(),
        spp = bad_bag_values[[x, 2]]) |>
      select(-bad_bag_values[[x, 2]]) |>
      mutate(
        proportion =
          paste0(round(.data$n_bad_bags / .data$n_state, 2) * 100, "%"),
        bad_bag_value = bad_bag_values[[x, 3]]) |>
      distinct(
        .data$dl_state, .data$spp, .data$bad_bag_value, n = .data$n_bad_bags,
        .data$proportion)
  }
