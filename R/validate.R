#' Validate data
#'
#' After cleaning the data with \code{\link{clean}}, check to make sure the data don't have any erroneously repeated values.
#'
#' @importFrom dplyr select
#' @importFrom dplyr matches
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
#'
#' @param x The object created after cleaning data with \code{\link{clean}}
#' @param type Type of validation to perform. Acceptable values include:
#'  \itemize{
#'  \item vertical - Checks for repetition vertically in species and/or bag fields, grouped by dl_state and dl_date
#'  \item horizontal - Checks for repetition horizontally, across each record
#'  }
#' @param all Should all species groups be checked (TRUE)? If set to FALSE (default), then only ducks will be vertically checked and only ducks, geese, and coots_snipe will be horizontally checked.
#' @param period Time period in which to group the data. The function uses dl_date automatically, but either of the following may be supplied:
#'  \itemize{
#'  \item dl_date - Date the HIP data were downloaded
#'  \item dl_cycle - Download cycle that the HIP data belong to
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

validate <-
  function(x, type, all = FALSE, period = NA){
    if (!period %in% c(NA, "dl_date", "dl_cycle")) {
      message(
        paste0(
          "Error: Incorrect value supplied for `period` parameter. Please",
          " choose: NA, 'dl_cycle', or 'dl_date'.")
      )
    } else {
      if (type == "vertical") {
        # Vertical validation

        # Quick check (ducks only)
        if (all == FALSE) {
          if (is.na(period)) {


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
                  names_to = "species_grp",
                  values_to = "v_rep") |>
                ungroup() |>
                # Keep only the uniform spp groups
                filter(v_rep == 1) |>
                # Report count of times uniform values are repeated
                mutate(v_repeated = dl_rows) |>
                select(-c("dl_rows", "v_rep"))
              } else if (period == "dl_date" | period == "dl_cycle") {

                # With time period specification: dl_date or dl_cycle
                validated_v <-
                  x |>
                  # Subset the data
                  select(dl_state, {{period}}, ducks_bag) |>
                  group_by(dl_state, !!sym(period)) |>
                  # Add number of rows per group
                  mutate(dl_rows = n()) |>
                  # Move dl_rows to 3rd col position
                  relocate(dl_rows, .after = {{period}}) |>
                  ungroup() |>
                  group_by(dl_state, !!sym(period), dl_rows) |>
                  # Count the number of unique values in each species column
                  summarize(ducks_bag = length(unique(ducks_bag)), .groups = "keep") |>
                  pivot_longer(
                    cols = !contains("dl"),
                    names_to = "species_grp",
                    values_to = "v_rep") |>
                  ungroup() |>
                  # Keep only the uniform spp groups
                  filter(v_rep == 1) |>
                  # Report count of times uniform values are repeated
                  mutate(v_repeated = dl_rows) |>
                  select(-c("dl_rows", "v_rep"))
              }
        } else {
          if(is.na(period)) {

            # Without time period specification

            vr <-
              x |>
              # Subset the data
              select(
                dl_state,
                source_file,
                matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) |>
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
                  ducks_bag:seaducks,
                  ~length(unique(.x))),
                .groups = "keep") |>
              pivot_longer(
                cols = !matches("dl|source"),
                names_to = "species_grp",
                values_to = "n_unique_values") |>
              ungroup() |>
              # Keep only the spp groups with uniform values
              filter(n_unique_values == 1) |>
              mutate(v_repeated = dl_rows) |>
              select(dl_state, source_file, species_grp, v_repeated)

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
                      species = vr$species_grp[.x]) |>
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
                    species_grp = spp,
                    repeated_value = stateBagValue) |>
                  mutate(
                    repeated_value = as.character(repeated_value),
                    flag = "no season"),
                by = c("dl_state", "species_grp", "repeated_value")
              ) |>
              filter(is.na(flag)) |>
              select(-flag)

          } else if (period %in% c("dl_date", "dl_cycle")) {

            # With time period specification: dl_date or dl_cycle

            validated_v <-
              x |>
              # Subset the data
              select(
                  dl_state,
                  {{period}},
                  matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) |>
                group_by(dl_state, !!sym(period)) |>
                # Add number of rows per group
                mutate(dl_rows = n()) |>
                # Move dl_rows to 3rd col position
                relocate(dl_rows, .after = {{period}}) |>
                ungroup() |>
                group_by(dl_state, !!sym(period), dl_rows) |>
                # Count the number of unique values in each species column
                summarize(across(ducks_bag:seaducks, ~length(unique(.x))), .groups = "keep") |>
                pivot_longer(
                  cols = !contains("dl"),
                  names_to = "species_grp",
                  values_to = "v_rep") |>
                ungroup() |>
                # Keep only the uniform spp groups
                filter(v_rep == 1) |>
                # Report count of times uniform values are repeated
                mutate(v_repeated = dl_rows) |>
                select(-c("dl_rows", "v_rep"))
          }
        }


        if (nrow(validated_v) != 0) {
          # Return for uniformity detected
          return(validated_v)
          } else {
            # Return for no uniformities
            message("Data are good to go! No vertical repetition detected.")
          }
      } else if (type == "horizontal") {

        # Horizontal validation

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
              # Subset the first value from each horizontal bag string, useful later if
              # the values are repeated
              h_first = str_sub(h_string, 0, 1),
              # Convert string to vector
              h_string = str_split(h_string, "-"),
              # Create unique row key
              rowkey = row_number())
          } else {

            # Thorough check (all spp)

            h_test <-
              x |>
              # Subset the data
              select(
                source_file,
                matches("bag|coots|rails|cranes|pigeon|brant|seaducks")) |>
              group_by(source_file) |>
              # Paste all of the species group values together
              unite(h_string, !contains("source"), sep = "-") |>
              ungroup() |>
              mutate(
                # Subset the first value from each horizontal bag string, useful later if
                # the values are repeated
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
          # If only one value was used across the row, mark it as containing repeats
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
          # Rename h_first so it is more clear that the field indicates the repeated
          # horizontal value
          rename(h_value = h_first) |>
          # Sort with descending proportion of repeats
          arrange(desc(prop_repeat))

        if (nrow(validated_h) > 0) {
          # Return for uniformity detected
          return(validated_h)
          } else {
            # Return for no uniformities
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
  }
