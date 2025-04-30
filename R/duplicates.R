#' Fix duplicates
#'
#' Resolve duplicate HIP records.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr across
#' @importFrom dplyr matches
#' @importFrom dplyr select
#' @importFrom dplyr anti_join
#' @importFrom dplyr slice_sample
#' @importFrom dplyr n_distinct
#' @importFrom stringr str_replace
#' @importFrom rlang syms
#'
#' @param current_data The object created after filtering to current data with \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

duplicateFix <-
  function(current_data) {

    # Create a tibble of duplicate records with a duplicate id field
    duplicates <- duplicateID(current_data)

    # --------------------------------------------------------------------------
    # PART 2: Sea duck AND brant states duplicate resolution

    # For sea duck AND brant states, we handle duplicates by:
    # 1. Keep record(s) with the most recent issue date.
    # 2. Exclude records with all 1s or all 0s in bag columns from consideration.
    # 3. Keep any records that have a 2 for either brant or sea duck.
    # 4. If more than one record remains, choose to keep one randomly.

    sdbr_dupes <-
      duplicates |>
      # Filter to sea duck AND brant states
      filter(dl_state %in% states_sdbr) |>
      duplicateNewest() |>
      duplicateAllOnes() |>
      mutate(
        # Check records for 2 in brant or seaduck field
        x_sdbrs = ifelse(brant == "2"|seaducks == "2", "keeper", NA)) |>
      # Convert x_bags if "keeper" to the number of records in the group with
      # the "keeper" value; otherwise, indicate 0s or 1s
      group_by(duplicate_id, x_bags) |>
      mutate(x_bags = ifelse(x_bags == "keeper", as.character(n()), x_bags)) |>
      ungroup() |>
      # Convert x_sdbrs that passed the check above to equal the number of
      # records in the group that count as "keeper"
      group_by(duplicate_id, x_sdbrs) |>
      mutate(
        x_sdbrs =
          ifelse(!is.na(x_sdbrs), as.character(n()), x_sdbrs)) |>
      ungroup() |>
      # Make decisions on which record to keep for each group
      group_by(duplicate_id) |>
      mutate(
        decision =
          case_when(
            # When there's only 1 record per group, keep it
            n() == 1 ~ "keeper",
            # When there's a record in a group and it's the only one that passed
            # the bag and sdbr checks above, keep it
            x_bags == 1 & x_sdbrs == 1 ~ "keeper",
            # When there isn't a 1 value in any of the checking columns, it's a
            # duplicate still and we will need to randomly choose which record
            # in the group to keep later
            !(1 %in% x_bags)&!(1 %in% x_sdbrs) ~ "dupl",
            # If there is a 1 in the sdbr checks col, then that record is the
            # one we want to keep
            x_sdbrs == 1 ~ "keeper",
            # For rare cases that have two records: keep the record with the not
            # all 1s bag values (unsure if needed)
            x_bags == 1 ~ "keeper",
            TRUE ~ NA_character_)) |>
      # If NA records have another qualifying record in their group, drop them
      mutate(
        decision =
          ifelse(
            n() > 1 & length(unique(decision)) > 1 & is.na(decision),
            "drop",
            decision)) |>
      ungroup() |>
      filter(decision != "drop")

    # Get the final frame with 1 record per hunter
    if(nrow(filter(sdbr_dupes, decision == "dupl")) > 0){
      sdbr_dupes <-
        bind_rows(
          # Handle "dupl"s; randomly keep one per group using slice_sample()
          sdbr_dupes |>
            filter(decision == "dupl") |>
            group_by(duplicate_id) |>
            slice_sample(n = 1) |>
            ungroup(),
          # Row bind in the "keepers" (should already be 1 per hunter)
          sdbr_dupes |>
            filter(decision == "keeper")) |>
        # Drop unneeded columns
        select(-c(x_bags:decision)) |>
        # Designate record type
        mutate(record_type = "HIP")
    }else{
      sdbr_dupes <-
        sdbr_dupes |>
        filter(decision == "keeper") |>
        # Drop unneeded columns
        select(-c(x_bags:decision)) |>
        # Designate record type
        mutate(record_type = "HIP")}

    # --------------------------------------------------------------------------
    # PART 3: Sea duck states duplicate resolution (Maine)

    # For sea duck states, we handle duplicates by:
    # 1. Keep record(s) with the most recent issue date.
    # 2. Exclude records with all 1s or all 0s in bag columns from consideration.
    # 3. Keep any records that have a 2 for sea duck.
    # 4. If more than one record remains, choose to keep one randomly.

    seaduck_dupes <-
      duplicates |>
      # Filter to seaduck states
      filter(dl_state %in% states_seaducks) |>
      duplicateNewest() |>
      duplicateAllOnes() |>
      mutate(
        # Check records for 2 in seaduck field
        x_seaducks = ifelse(seaducks == "2", "keeper", NA)) |>
      # Convert x_bags if "keeper" to the number of records in the group with
      # the "keeper" value; otherwise, indicate 0s or 1s
      group_by(duplicate_id, x_bags) |>
      mutate(x_bags = ifelse(x_bags == "keeper", as.character(n()), x_bags)) |>
      ungroup() |>
      # Convert x_seaducks that passed the check above to equal the number of
      # records in the group that count as "keeper"
      group_by(duplicate_id, x_seaducks) |>
      mutate(
        x_seaducks =
          ifelse(!is.na(x_seaducks), as.character(n()), x_seaducks)) |>
      ungroup() |>
      # Make decisions on which record to keep for each group
      group_by(duplicate_id) |>
      mutate(
        decision =
          case_when(
            # When there's only 1 record per group, keep it
            n() == 1 ~ "keeper",
            # When there's a record in a group and it's the only one that passed
            # the bag and seaduck checks above, keep it
            x_bags == 1 & x_seaducks == 1 ~ "keeper",
            # When there isn't a 1 value in any of the checking columns, it's a
            # duplicate still and we will need to randomly choose which record
            # in the group to keep later
            !(1 %in% x_bags)&!(1 %in% x_seaducks) ~ "dupl",
            # If there is a 1 in the seaduck checks col, then that record is the
            # one we want to keep
            x_seaducks == 1 ~ "keeper",
            # For rare cases that have two records: keep the record with the not
            # all 1s bag values (unsure if needed)
            x_bags == 1 ~ "keeper",
            TRUE ~ NA_character_)) |>
      # If NA records have another qualifying record in their group, drop them
      mutate(
        decision =
          ifelse(
            n() > 1 & length(unique(decision)) > 1 & is.na(decision),
            "drop",
            decision)) |>
      ungroup() |>
      filter(decision != "drop")

    # Get the final frame with 1 record per hunter
    if(nrow(filter(seaduck_dupes, decision == "dupl")) > 0){
      seaduck_dupes <-
        bind_rows(
          # Handle "dupl"s; randomly keep one per group using slice_sample()
          seaduck_dupes |>
            filter(decision == "dupl") |>
            group_by(duplicate_id) |>
            slice_sample(n = 1) |>
            ungroup(),
          # Row bind in the "keepers" (should already be 1 per hunter)
          seaduck_dupes |>
            filter(decision == "keeper")) |>
        # Drop unneeded columns
        select(-c(x_bags:decision)) |>
        # Designate record type
        mutate(record_type = "HIP")
    }else{
      seaduck_dupes <-
        seaduck_dupes |>
        filter(decision == "keeper") |>
        # Drop unneeded columns
        select(-c(x_bags:decision)) |>
        # Designate record type
        mutate(record_type = "HIP")}

    # --------------------------------------------------------------------------
    # PART 4: Non-permit, non-seaduck, non-brant record duplicate resolution

    other_dupes <-
      duplicates |>
      # Record not from seaduck, brant, or permit state
      filter(
        !(dl_state %in% states_sdbr) &
        !(dl_state %in% states_seaducks) &
        !(dl_state %in% unique(REF_PMT_INLINE$dl_state))) |>
      # Repeat duplicate checking
      duplicateNewest() |>
      duplicateAllOnes() |>
      # Convert x_bags if "keeper" to the number of records in the group with
      # the "keeper" value; otherwise, indicate 0s or 1s
      group_by(duplicate_id, x_bags) |>
      mutate(x_bags = ifelse(x_bags == "keeper", as.character(n()), x_bags)) |>
      ungroup() |>
      # Make decisions on which record to keep for each group
      group_by(duplicate_id) |>
      mutate(
        decision =
          case_when(
            # When there's only 1 record per group, keep it
            n() == 1 ~ "keeper",
            # When there's a record in a group and it's the only one that passed
            # the bag check above, keep it
            x_bags == 1 ~ "keeper",
            # When there isn't a 1 value in the checking column, it's a
            # duplicate still and we will need to randomly choose which record
            # in the group to keep later
            !(1 %in% x_bags) ~ "dupl",
            # For rare cases that have two records: keep the record with not all
            # 1s (unsure if needed)
            x_bags == 1 ~ "keeper",
            TRUE ~ NA_character_)) |>
      # If NA records have another qualifying record in their group, drop them
      mutate(
        decision =
          ifelse(
            n() > 1 & length(unique(decision)) > 1 & is.na(decision),
            "drop",
            decision)) |>
      ungroup() |>
      filter(decision != "drop")

    # Get the final frame with 1 record per hunter
    if(nrow(filter(other_dupes, decision == "dupl")) > 0){
      other_dupes <-
        bind_rows(
          # Handle "dupl"s; randomly keep one per group using slice_sample()
          other_dupes |>
            filter(decision == "dupl") |>
            group_by(duplicate_id) |>
            slice_sample(n = 1) |>
            ungroup(),
          # Row bind in the "keepers" (should already be 1 per hunter)
          other_dupes |>
            filter(decision == "keeper")) |>
        # Drop unneeded columns
        select(-c(x_bags:decision)) |>
        # Designate record type
        mutate(record_type = "HIP")
    }else{
      other_dupes <-
        other_dupes |>
        filter(decision == "keeper") |>
        # Drop unneeded columns
        select(-c(x_bags:decision)) |>
        # Designate record type
        mutate(record_type = "HIP")}

    # --------------------------------------------------------------------------
    # PART 5: Permit state duplicate resolution

    # Some permit states (WA, OR) submit permit records separately from
    # HIP records. The status of each record will be determined and they will be
    # labeled as either HIP or permit. Multiple HIP records must be resolved
    # (keep only 1 per hunter), but multiple permits are allowed.
    # - HIP records contain nonzero values in "regular spp" columns
    # - Permit records always have 0s in "regular spp" columns

    permit_dupes <-
      duplicates |>
      # Filter the duplicates to those that occur in permit states
      filter(dl_state %in% unique(REF_PMT_INLINE$dl_state)) |>
      # Set "." to NA in take fields & make those fields numeric
      mutate(
        across(
          matches("bag|coots|rails"),
          ~str_replace(.x, "\\.", NA_character_) |>
          as.numeric(.))) |>
      # Calculate sum of values in the non-permit species columns to use as a
      # proxy.
      # Note: We can no longer use "special_sum" == 0 because the value for
      # cranes is often sent as a 1. The special_sum reference col was
      # previously calculated by adding the values for brant, seaducks,
      # band_tailed_pigeon, and cranes. Using only the "regular" HIP species sum
      # is relatively reliable instead.
      mutate(
        other_sum = rowSums(across(matches("bag|coots|rails")), na.rm = T)) |>
      # Reset bags to character for row bind later
      mutate(
        across(matches("bag|coots|rails"), ~as.character(.x)),
        record_type =
          case_when(
            # If the sum of values in non-permit species columns is > 0, the
            # record is a HIP record
            other_sum > 0 ~ "HIP",
            # If the sum of values in permit species columns is > 0 the record
            # is a permit
            other_sum == 0 ~ "PMT",
            TRUE ~ NA_character_))

    # If there is more than one HIP record per person, decide which one to keep
    hip_dupes <-
      permit_dupes |>
      filter(record_type == "HIP") |>
      duplicateNewest() |>
      duplicateAllOnes() |>
      # Convert x_bags if "keeper" to the number of records in the group with
      # the "keeper" value; otherwise, indicate 0s or 1s
      group_by(duplicate_id, x_bags) |>
      mutate(x_bags = ifelse(x_bags == "keeper", as.character(n()), x_bags)) |>
      ungroup() |>
      # Make decisions on which record to keep for each group
      group_by(duplicate_id) |>
      mutate(
        decision =
          case_when(
            # When there's only 1 record per group, keep it
            n() == 1 ~ "keeper",
            # When there's a record in a group and it's the only one that passed
            # the bag check above, keep it
            x_bags == 1 ~ "keeper",
            # When there isn't a 1 value in the checking column, it's a
            # duplicate still and we will need to randomly choose which record
            # in the group to keep later
            !(1 %in% x_bags) ~ "dupl",
            # For rare cases that have two records: keep the record with the not
            # all 1s bag values (unsure if needed)
            x_bags == 1 ~ "keeper",
            TRUE ~ NA_character_)) |>
      # If NA records have another qualifying record in their group, drop them
      mutate(
        decision =
          ifelse(
            n() > 1 & length(unique(decision)) > 1 & is.na(decision),
            "drop",
            decision)) |>
      ungroup() |>
      filter(decision != "drop") |>
      # Remove unneeded rows
      select(-c("other_sum", "x_bags"))

    # Get the final frame with 1 record per hunter
    if(nrow(filter(hip_dupes, decision == "dupl")) > 0){
      hip_dupes <-
        bind_rows(
          # Handle "dupl"s; randomly keep one per group using slice_sample()
          hip_dupes |>
            filter(decision == "dupl") |>
            group_by(duplicate_id) |>
            slice_sample(n = 1) |>
            ungroup(),
          # Row bind in the "keepers" (should already be 1 per hunter)
          hip_dupes |>
            filter(decision == "keeper"))
    }else{
      hip_dupes <-
        # Keep the "keepers" (should already be 1 per hunter)
        hip_dupes |>
        filter(decision == "keeper")}

    # Get table of just permits
    permit_dupes <-
      permit_dupes |>
      filter(record_type == "PMT")

    # --------------------------------------------------------------------------
    # PART 6: Combine all resolved records into one tibble

    resolved_duplicates <-
      # Remove duplicates from the input frame
      current_data |>
      group_by(!!!syms(REF_DUPL_FIELDS)) |>
      filter(n() == 1) |>
      ungroup() |>
      # These records should all be HIP
      mutate(record_type = "HIP") |>
      # Classify solo permit records as PMT
      mutate(
        across(
          matches("bag|coots|rails|band|brant|seaducks"),
          ~as.numeric(.x)),
        other_sum =
          rowSums(across(matches("bag|coots|rails")), na.rm = T),
        special_sum =
          rowSums(across(matches("band|brant|seaducks")), na.rm = T),
        across(
          matches("bag|coots|rails|band|brant|seaducks"),
          ~as.character(.x)),
        record_type =
          ifelse(
            other_sum == 0 & special_sum > 0 & dl_state %in% unique(REF_PMT_INLINE$dl_state),
            "PMT",
            record_type)) |>
      select(-c(other_sum, special_sum)) |>
      # Add back in the resolved duplicates
      bind_rows(
        sdbr_dupes |> select(-duplicate_id),
        seaduck_dupes |> select(-duplicate_id),
        other_dupes |> select(-duplicate_id),
        permit_dupes |> select(-c("duplicate_id", "other_sum")),
        hip_dupes |> select(-c("duplicate_id", "decision"))) |>
      distinct()

    return(resolved_duplicates)
  }

#' Create a tibble of duplicates with an ID column
#'
#' Return a tibble of duplicates with a duplicate ID column identifying each group of records.
#'
#' @importFrom dplyr group_by
#' @importFrom rlang syms
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom dplyr mutate
#' @importFrom dplyr cur_group_id
#' @importFrom dplyr ungroup
#'
#' @inheritParams duplicateFix
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

duplicateID <-
  function(current_data) {
    current_data |>
      # Group by REF_DUPL_FIELDS to determine each unique hunter
      group_by(!!!syms(REF_DUPL_FIELDS)) |>
      # Identify duplicates, aka records in groups of n() > 1
      filter(n() > 1) |>
      mutate(duplicate_id = paste0("duplicate_", cur_group_id())) |>
      ungroup()
  }

#' Find the most recent records out of a group of duplicates
#'
#' The internal \code{duplicateNewest} function is used inside of \code{\link{duplicateFix}} to filter groups of duplicates to the most recent records out of each group.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom lubridate mdy
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#'
#' @param duplicates The tibble created by \code{\link{duplicateID}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

duplicateNewest <-
  function(duplicates) {
    duplicates |>
      group_by(duplicate_id) |>
      mutate(
        # Check for most recent issue date
        x_issue_date =
          # Skip frame shift issues and only evaluate for dates in correct
          # mm/dd/yyyy format
          ifelse(
            str_detect(issue_date, "^[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}$"),
            ifelse(
              issue_date ==
                strftime(
                  max(mdy(issue_date), na.rm = TRUE), format = "%m/%d/%Y"),
              "keeper",
              NA),
            "bad_issue_date_format")) |>
      ungroup() |>
      # If the issue date was in the right format, keep the record(s) from each
      # group that were the most recent; if the issue date was in the wrong
      # format, keep those too for future evaluation
      filter(!is.na(x_issue_date)) |>
      select(-x_issue_date)
  }

#' Flag all-one records in a group of duplicates
#'
#' The internal \code{duplicateAllOnes} function is used inside of \code{\link{duplicateFix}} to evaluate groups of duplicates for records containing "1" for every bag value.
#'
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_chr
#' @importFrom dplyr select
#' @importFrom dplyr all_of
#'
#' @param duplicates The tibble created by \code{\link{duplicateID}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

duplicateAllOnes <-
  function(duplicates) {
    duplicates |>
      # Flag records with 1 in every bag field
      mutate(
        x_bags =
          pmap_chr(
            select(duplicates, all_of(REF_BAG_FIELDS)),
            ~ifelse(all(c(...) == "1"), "ones", "keeper"))
      )
  }

#' Find duplicates
#'
#' Determine how many duplicate records are in the data and return a table.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr all_of
#' @importFrom dplyr matches
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr n_distinct
#' @importFrom dplyr cur_group_id
#' @importFrom dplyr row_number
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom dplyr summarize
#' @importFrom dplyr distinct
#' @importFrom rlang syms
#'
#' @inheritParams duplicateFix
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

duplicateFinder <-
  function(current_data) {

    # List of permit records
    pmts <-
      current_data |>
      # Classify solo permit records as PMT
      mutate(
        across(
          all_of(REF_BAG_FIELDS),
          ~as.numeric(.x)),
        other_sum =
          rowSums(across(matches("bag|coots|rails")), na.rm = T),
        special_sum =
          rowSums(across(matches("cranes|band|brant|seaducks")), na.rm = T),
        record_type =
          ifelse(
            other_sum == 0 & special_sum > 0 & dl_state %in% unique(REF_PMT_INLINE$dl_state),
            "PMT",
            NA)) |>
      filter(record_type == "PMT") |>
      pull(record_key)

    duplicates <-
      current_data |>
      # Filter out permits
      filter(!record_key %in% pmts) |>
      # Create a row key
      mutate(hunter_key = paste0("hunter_", row_number())) |>
      group_by(!!!syms(REF_DUPL_FIELDS)) |>
      # Identify duplicates
      filter(n() > 1) |>
      ungroup() |>
      # Filter out non-duplicate records
      mutate(duplicate = "duplicate") |>
      # Sort tibble
      arrange(!!!syms(REF_DUPL_FIELDS))

    # Number of registrations that are duplicated
    duplicate_individuals <-
      duplicates |>
      select(!!!syms(REF_DUPL_FIELDS)) |>
      n_distinct()

    # Determine which fields are different between the duplicates so we can
    # try to figure out why hunters are in the data more than once
    dupl_tibble <-
      duplicates |>
      select(-c("hunter_key", "duplicate")) |>
      group_by(!!!syms(REF_DUPL_FIELDS)) |>
      mutate(
        # Hunter key per individual (not per row)
        hunter_key = cur_group_id(),
        # Find the reason for the duplicates
        # We start with a blank string so the following code can paste in
        dupl = "",
        # Iterate over each field in order to paste the field names together
        # (can't be done with case_when)
        dupl =
          ifelse(
            length(unique(title)) > 1,
            paste(dupl, "title", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(middle)) > 1,
            paste(dupl, "middle", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(suffix)) > 1,
            paste(dupl, "suffix", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(address)) > 1,
            paste(dupl, "address", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(city)) > 1,
            paste(dupl, "city", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(zip)) > 1,
            paste(dupl, "zip", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(birth_date)) > 1,
            paste(dupl, "birth_date", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(issue_date)) > 1,
            paste(dupl, "issue_date", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(hunt_mig_birds)) > 1,
            paste(dupl, "hunt_mig_birds", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(registration_yr)) > 1,
            paste(dupl, "registration_yr", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(email)) > 1,
            paste(dupl, "email", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(dl_date)) > 1,
            paste(dupl, "dl_date", sep = "-"),
            dupl),
        dupl =
          ifelse(
            length(unique(dl_cycle)) > 1,
            paste(dupl, "dl_cycle", sep = "-"),
            dupl),
        dupl = ifelse(str_detect(dupl, "^$"), "bag", dupl),
        dupl = str_remove(dupl, "^\\-")
      ) |>
      ungroup() |>
      select(hunter_key, dupl, dl_state) |>
      distinct()

    if(nrow(dupl_tibble) == 0) {
      message(
        paste(
          "There are", duplicate_individuals,
          "registrations with duplicates;", nrow(duplicates),
          "total duplicated records."))
    } else {
      message(
        paste(
          "There are", duplicate_individuals,
          "registrations with duplicates;", nrow(duplicates),
          "total duplicated records."))

      dupl_table <-
        dupl_tibble |>
        group_by(dupl) |>
        summarize(count = n()) |>
        ungroup()

      return(dupl_table)
    }
  }

#' Plot duplicates
#'
#' Plot which fields are duplicates of individual hunters.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom stats reorder
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 after_stat
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#'
#' @param current_data The object created after filtering to current data with \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

duplicatePlot <-
  function(current_data) {

    dupl_tibble <- duplicateFinder(current_data)

    dupl_plot <-
      dupl_tibble |>
      # Bin into generic "2+ fields" if more than one field contributes to a
      # duplicate
      mutate(
        dupl =
          case_when(
            # 5+ fields
            str_detect(dupl, "[a-z|a-z\\_a-z|a-z|a-z\\_a-z\\_a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "2+ fields",
            # 4 fields
            str_detect(dupl, "[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "2+ fields",
            # 3 fields
            str_detect(dupl, "[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "2+ fields",
            str_detect(dupl, "[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "2+ fields",
            TRUE ~ dupl)
      ) |>
      # Make a new col to reorder the bars
      group_by(dupl) |>
      mutate(total_count = n()) |>
      ungroup() |>
      ggplot(aes(x = reorder(dupl, -total_count))) +
      geom_bar(stat = "count") +
      geom_text(
        aes(
          x = dupl,
          label = after_stat(count),
          angle = 90),
        stat = "count",
        vjust = 0.2,
        hjust = -0.2) +
      labs(
        x = "Inconsistent field(s) for duplicated hunters",
        y = "Count",
        title = "Types of duplicates") +
      scale_y_continuous(expand = expansion(mult = c(-0, 0.2))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

    return(dupl_plot)
  }
