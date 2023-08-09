#' Validate data
#'
#' After cleaning the data with \code{\link{clean}}, check to make sure the data don't have any erroneously repeated values.
#'
#' @importFrom dplyr select
#' @importFrom dplyr matches
#' @importFrom dplyr all_of
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr relocate
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr sym
#' @importFrom dplyr contains
#' @importFrom dplyr row_number
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom tidyr unite
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_sub
#' @importFrom stringr str_split
#' @importFrom dplyr left_join
#' @importFrom dplyr bind_rows
#'
#' @param x The object created after cleaning data with \code{\link{clean}}
#' @param type Type of validation to perform. Acceptable values include:
#'  \itemize{
#'  \item vertical - Checks for repetition vertically in species and/or bag fields, grouped by dl_state and dl_date
#'  \item horizontal - Checks for repetition horizontally, across each record
#'  }
#' @param all Should all species groups be checked (TRUE)? If set to FALSE (default), then only ducks will be vertically checked and only ducks, geese, and coots_snipe will be horizontally checked.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

validate <-
  function(x, type, all = FALSE){

    if (type == "vertical") {
      if (all == FALSE) {

        # Quick check (ducks only)
        validated_v <-
          x |>
          # Subset the data
          select(dl_state, dl_date, ducks_bag) |>
          group_by(dl_state, dl_date) |>
          # Add number of rows per group
          mutate(dl_rows = n()) |>
          # Move dl_rows to 3rd col position
          relocate(dl_rows, .after = dl_date) |>
          ungroup() |>
          group_by(dl_state, dl_date, dl_rows) |>
          # Count the number of unique values in each species column
          summarize(ducks_bag = length(unique(ducks_bag)), .groups = "keep") |>
          pivot_longer(
            cols = !contains("dl"),
            names_to = "spp",
            values_to = "v_rep") |>
          ungroup() |>
          # Keep only the uniform spp groups
          filter(v_rep == 1) |>
          # Report count of times uniform values are repeated
          mutate(v_repeated = dl_rows) |>
          select(-c("dl_rows", "v_rep"))

      } else if (all == TRUE) {

        # All species columns
        vr <-
          x |>
          # Subset the data
          select(
            dl_state,
            source_file,
            all_of(ref_bagfields)) |>
          group_by(source_file) |>
          # Add number of rows per source_file
          mutate(dl_rows = n()) |>
          ungroup() |>
          # Move dl_rows to 3rd col position
          relocate(dl_rows, .after = source_file) |>
          group_by(dl_state, source_file, dl_rows) |>
          # Count the number of unique values in each species column
          summarize(
            across(
              all_of(ref_bagfields),
              ~length(unique(.x))),
            .groups = "keep") |>
          pivot_longer(
            cols = !matches("dl|source"),
            names_to = "spp",
            values_to = "n_unique_values") |>
          ungroup() |>
          # Keep only the spp groups with uniform values
          filter(n_unique_values == 1) |>
          mutate(v_repeated = dl_rows) |>
          select(dl_state, source_file, spp, v_repeated)

        validated_v <-
          vr |>
          # Join in what value was repeated
          mutate(
            repeated_value =
              map(
                1:nrow(vr),
                ~investigate(
                  x,
                  loc = vr$dl_state[.x],
                  period_type = "dl_date",
                  period =
                    str_extract(
                      vr$source_file[.x], "[0-9].+(?=\\.txt)"),
                  species = vr$spp[.x]) |>
                  pull(1)) |>
              unlist()
          ) |>
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
                repeated_value = stateBagValue) |>
              mutate(repeated_value = as.character(repeated_value)) |>
              # Join in state/species combinations that are supposed to be
              # all 0s because we receive those bag values in another format
              bind_rows(
                pmt_files |> rename(repeated_value = value)) |>
              distinct() |>
              mutate(flag = "no season"),
            by = c("dl_state", "spp", "repeated_value")
          ) |>
          filter(is.na(flag)) |>
          select(-flag)
        }
      if (nrow(validated_v) != 0) {
        return(validated_v)
      } else {
        message("Data are good to go! No vertical repetition detected.")
      }

    } else if (type == "horizontal") {
        if(all == FALSE){

          # Quick check (duck, goose, & coots_snipe only)
          h_test <-
            x |>
            # Subset the data
            select(source_file, ducks_bag, geese_bag, coots_snipe) |>
            group_by(source_file) |>
            # Paste all of the species group values together
            unite(h_string, !contains("source"), sep = "-") |>
            ungroup() |>
            mutate(
              # Subset the first value from each horizontal bag string, useful
              # later if the values are repeated
              h_first = str_sub(h_string, 0, 1),
              # Convert string to vector
              h_string = str_split(h_string, "-"),
              # Create unique row key
              rowkey = row_number())

        } else if (all == TRUE) {

          # All species columns
          h_test <-
            x |>
            # Subset the data
            select(
              source_file,
              all_of(ref_bagfields)) |>
            group_by(source_file) |>
            # Paste all of the species group values together
            unite(h_string, !contains("source"), sep = "-") |>
            ungroup() |>
            mutate(
              # Subset the first value from each horizontal bag string, useful
              # later if the values are repeated
              h_first = str_sub(h_string, 0, 1),
              # Convert string to vector
              h_string = str_split(h_string, "-"),
              # Create unique row key
              rowkey = row_number())
        }

      validated_h <-
        h_test |>
        group_by(rowkey) |>
        # Calculate the number of unique values used by each row
        mutate(h_validate = length(unique(h_test$h_string[[rowkey]]))) |>
        ungroup() |>
        select(-c("h_string", "rowkey")) |>
        # If only one value was used across the row, mark it as containing
        # repeats
        mutate(h_validate = ifelse(h_validate == "1", "rep", "not-rep")) |>
        # Calculate the total number of rows per file
        group_by(source_file) |>
        mutate(h_total = n()) |>
        ungroup() |>
        # Keep only the rows that have repeats indicated
        filter(h_validate == "rep") |>
        # Count the number of repeated rows per file
        group_by(source_file, h_first, h_total) |>
        summarize(h_rep = n(), .groups = "keep") |>
        ungroup() |>
        # Calculate the proportion of repeats per file
        group_by(source_file) |>
        mutate(prop_repeat = h_rep/h_total) |>
        ungroup() |>
        relocate(h_rep, .before = h_total) |>
        # Rename h_first so it is more clear that the field indicates the
        # repeated horizontal value
        rename(h_value = h_first) |>
        # Sort with descending proportion of repeats
        arrange(desc(prop_repeat))

      if (nrow(validated_h) > 0) {
        return(validated_h)
      } else {
        message("Data are good to go! No horizontal repetition detected.")
      }
    } else {
      # Return for incorrect type given
      message(
        paste0(
          "Error: Incorrect value supplied for `type` parameter. Please",
          " choose: 'vertical' or 'horizontal'.")
      )
    }
  }

#' Investigate data
#'
#' The internal \code{investigate} function looks into vertical repetition in \code{\link{validate}}.
#'
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr quo_name
#'
#' @param x The object created after cleaning data with \code{\link{clean}}
#' @param loc The download state in question
#' @param period_type The type of time period that will be given. You may choose one of two types:
#' \itemize{
#' \item dl_date
#' \item dl_cycle}
#' @param period Value of the time period in question
#' @param species The bird group in question. One of the bird groups from the following list may be supplied:
#' \itemize{
#' \item ducks_bag, geese_bag, dove_bag, woodcock_bag, coots_snipe, rails_gallinules, cranes, band_tailed_pigeon, brant, seaducks}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

investigate <-
  function(x, loc, period_type, period, species){

    # Pull requested value

    investigated_x <-
      x |>
      select(dl_state, {{period_type}}, all_of(ref_bagfields), source_file) |>
      filter(dl_state == loc & !!sym(period_type) == period) |>
      select(quo_name(species), source_file) |>
      distinct()

    if(nrow(investigated_x) == 0) {
      message("Are you sure you entered all of the parameters correctly?")
    }

    else{
      return(investigated_x)
    }
  }
