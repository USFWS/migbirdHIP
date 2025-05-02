#' Fix duplicates
#'
#' Resolve duplicate HIP records.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom rlang syms
#' @importFrom dplyr n
#' @importFrom dplyr distinct
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

    # Sea duck and brant states duplicate resolution
    # 1. Keep record(s) with the most recent issue date.
    # 2. Evaluate if records contain 1s in all bag columns and if records have a
    #    2 for either brant or sea duck (just seaduck = Maine).
    # 3. Keep record if it is the only one in its group that has a 2 in
    #    seaduck or brant, or if it's the only one that is not-all-1s.
    # 4. If more than one record remains per group, keep one randomly.
    seaduck_and_brant_duplicates <-
      duplicates |>
      # Filter to sea duck and brant states
      filter(dl_state %in% c(states_sdbr, states_seaducks)) |>
      # Keep records with the most recent issue_date
      duplicateNewest() |>
      # Check records for "1" in every bag field
      duplicateAllOnes() |>
      # Check records for "2" in brant or seaduck field
      mutate(
        sd_or_br_has_2 =
          case_when(
            dl_state %in% states_sdbr & brant == "2" ~ "has_2",
            dl_state %in% states_sdbr & seaducks == "2" ~ "has_2",
            dl_state %in% states_seaducks & seaducks == "2" ~ "has_2",
            TRUE ~ "no_2")) |>
      duplicateAllOnesGroupSize() |>
      # If record has 2 in brant, seaduck, or both, put the group size (number
      # of records in the set of duplicates that have hunted brant and/or
      # seaducks); if record DOES NOT have a 2 in brant or seaduck, put "no_2"
      group_by(duplicate_id, sd_or_br_has_2) |>
      mutate(
        sd_or_br_has_2_group_size =
          ifelse(sd_or_br_has_2 == "has_2", as.character(n()), "no_2")) |>
      ungroup() |>
      # Make decision on which record to keep for each group
      group_by(duplicate_id) |>
      mutate(
        decision =
          case_when(
            # When there's only 1 record per group, keep it
            n() == 1 ~ "keeper_single",
            # Keep a record if it's the only one in its group that has a 2 in
            # seaduck or brant columns
            sd_or_br_has_2_group_size == 1 ~ "keeper_sd_or_br_has_2",
            # For rare cases that still have two or more records: keep a record
            # if it's the only one in its group with the not all 1s bag values
            all_ones_group_size == 1 ~ "keeper_not_all_1s",
            # When there isn't a 1 value in any of the checking columns, it's a
            # duplicate still and we will need to randomly choose which record
            # in the group to keep later
            !(1 %in% all_ones_group_size) &
              !(1 %in% sd_or_br_has_2_group_size) ~
              "duplicate",
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

    # Get the final sea duck and brant tibble with 1 record per hunter
    sdbr_deduplicated <- duplicateSample(seaduck_and_brant_duplicates)

    # Non-permit, non-seaduck, non-brant record duplicate resolution
    other_duplicates <-
      duplicates |>
      # Record not from seaduck, brant, or permit state
      filter(
        !(dl_state %in% states_sdbr) &
        !(dl_state %in% states_seaducks) &
        !(dl_state %in% unique(REF_PMT_INLINE$dl_state))) |>
      # Keep records with the most recent issue_date
      duplicateNewest() |>
      # Check records for "1" in every bag field
      duplicateAllOnes() |>
      duplicateAllOnesGroupSize() |>
      # Make decisions on which record to keep for each group
      duplicateDecide()

    # Get the final non-special record tibble with 1 record per hunter
    other_deduplicated <- duplicateSample(other_duplicates)

    # In-line permit state duplicate resolution
    # WA and OR submit permit records separately from HIP records. These partial
    # duplicates will be labeled as either HIP or PMT. Multiple HIP records must
    # be resolved (keep only 1 per hunter), but multiple permits are allowed.
    permit_state_duplicates <-
      duplicates |>
      # Filter the duplicates to those that occur in permit states
      filter(dl_state %in% unique(REF_PMT_INLINE$dl_state)) |>
      # Set record type for HIP registrations and in-line permits
      duplicateRecordType()

    # If there is more than one HIP record per person from an in-line permit
    # state, decide which one to keep
    if(nrow(permit_state_duplicates) > 0) {
      hip_permit_state_duplicates <-
        permit_state_duplicates |>
        filter(record_type == "HIP") |>
        # Keep records with the most recent issue_date
        duplicateNewest() |>
        # Check records for "1" in every bag field
        duplicateAllOnes() |>
        duplicateAllOnesGroupSize() |>
        # Make decisions on which record to keep for each group
        duplicateDecide()

      # Get the final permit state tibble with 1 HIP record per hunter
      hip_deduplicated <-
        duplicateSample(hip_permit_state_duplicates) |>
        select(-c("duplicate_id", "all_ones", "all_ones_group_size", "decision"))
    } else {
      permit_state_duplicates <-
        permit_state_duplicates |>
        mutate(record_type = as.character(record_type))

      hip_deduplicated <- permit_state_duplicates
    }

    # Get the final permit state tibble with 1 HIP record per hunter
    hip_deduplicated <- duplicateSample(hip_permit_state_duplicates)

    # Combine all resolved records into one tibble
    resolved_duplicates <-
      # Remove duplicates from the input frame
      current_data |>
      group_by(!!!syms(REF_HUNTER_ID_FIELDS)) |>
      filter(n() == 1) |>
      ungroup() |>
      # Set record type for single HIP registrations and solo in-line permits
      duplicateRecordType() |>
      # Add the resolved duplicates back in
      bind_rows(
        # Sea duck and brant states
        sdbr_deduplicated |>
          select(-(duplicate_id:decision)) |>
          mutate(record_type = "HIP"),
        # Other states
        other_deduplicated |>
          select(-(duplicate_id:decision)) |>
          mutate(record_type = "HIP"),
        # In-line permit states PMT records
        permit_state_duplicates |>
          select(-duplicate_id) |>
          filter(record_type == "PMT"),
        # In-line permit states HIP records
        hip_deduplicated
        ) |>
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
      # Group by REF_HUNTER_ID_FIELDS to determine each unique hunter
      group_by(!!!syms(REF_HUNTER_ID_FIELDS)) |>
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
#' @importFrom lubridate mdy
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
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
      # Identify records with most recent issue date
      mutate(
        x_issue_date =
          ifelse(
            issue_date ==
              strftime(max(mdy(issue_date), na.rm = TRUE), format = "%m/%d/%Y"),
              "newest",
              NA)
        ) |>
      ungroup() |>
      # Keep the record(s) from each group that were the most recent
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
        all_ones =
          pmap_chr(
            select(duplicates, all_of(REF_BAG_FIELDS)),
            ~ifelse(all(c(...) == "1"), "all_1s", "not_all_1s"))
      )
  }

#' Evaluate group sizes of all-one/not-all-one records
#'
#' The internal \code{duplicateAllOnesGroupSize} function is used inside of \code{\link{duplicateFix}}.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#'
#' @param duplicates The tibble created by \code{\link{duplicateID}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

duplicateAllOnesGroupSize <-
  function(duplicates) {
    # If record doesn't have 1s in every bag field, put the group size (number
    # of records in the set of duplicates that are not all-1s); if record DOES
    # have 1s in every bag field, put "all_1s"
    duplicates |>
      group_by(duplicate_id, all_ones) |>
      mutate(
        all_ones_group_size =
          ifelse(all_ones == "not_all_1s", as.character(n()), all_ones)) |>
      ungroup()
  }

#' Decide which duplicate records should be kept or dropped
#'
#' The internal \code{duplicateDecide} function is used inside of \code{\link{duplicateFix}} to deduplicate intermediate tibbles.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#'
#' @param dupes Intermediate tibble created in \code{\link{duplicateFix}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

duplicateDecide <-
  function(dupes) {

    dupes |>
      # Make decisions on which record to keep for each group
      group_by(duplicate_id) |>
      mutate(
        decision =
          case_when(
            # When there's only 1 record per group, keep it
            n() == 1 ~ "keeper_single",
            # When there's a record in a group and it's the only one that passed
            # the bag check above, keep it
            all_ones_group_size == 1 ~ "keeper_not_all_1s",
            # When there isn't a 1 value in the checking column, it's a
            # duplicate still and we will need to randomly choose which record
            # in the group to keep later
            !(1 %in% all_ones_group_size) ~ "duplicate",
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
  }

#' De-duplicate by randomly sampling intermediate tibbles
#'
#' The internal \code{duplicateSample} function is used inside of \code{\link{duplicateFix}} to deduplicate intermediate tibbles that have been evaluated using other criteria already.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr slice_sample
#' @importFrom dplyr ungroup
#' @importFrom stringr str_detect
#'
#' @param dupes Intermediate tibble created in \code{\link{duplicateFix}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

duplicateSample <-
  function(dupes) {
    bind_rows(
      # Handle "duplicate"s; randomly keep one per group using slice_sample()
      dupes |>
        filter(decision == "duplicate") |>
        group_by(duplicate_id) |>
        slice_sample(n = 1) |>
        ungroup(),
      # Row bind in the "keepers" (should already be 1 per hunter)
      dupes |>
        filter(str_detect(decision, "keeper")))
  }

#' Set record type
#'
#' The internal \code{duplicateRecordType} function is used inside of \code{\link{duplicateFix}} to set record type of registrations based on each record's bag values.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr matches
#'
#' @param duplicates The tibble created by \code{\link{duplicateID}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

duplicateRecordType <-
  function(duplicates) {
    # If a record is from an in-line permit state, the sum of values in
    # non-permit species columns is 0, AND the sum of values in permit species
    # columns is > 0, the record is an in-line permit.
    duplicates |>
      mutate(
        record_type =
          ifelse(
            dl_state %in% unique(REF_PMT_INLINE$dl_state) &
              rowSums(across(matches(eval(REGEX_NON_PMT_SPECIES)), as.numeric),
                      na.rm = T) == 0 &
              rowSums(across(matches("band|brant|seaducks"), as.numeric),
                      na.rm = T) > 0,
            "PMT",
            "HIP"))
  }

#' Find causes of duplication
#'
#' The internal \code{duplicateFields} function is used inside of \code{\link{duplicateFinder}} to find which fields have different values among a group of duplicate registrations.
#'
#' @importFrom purrr map
#' @importFrom purrr discard
#' @importFrom stringr str_c
#'
#' @param duplicates The tibble created by \code{\link{duplicateID}}
#' @param fields Name of the column to compare values for
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

duplicateFields <-
  function(duplicates, fields) {
    # Return the field name if there is more than one unique value in
    # that field for a hunter, otherwise return NA
    map(
      fields,
      \(x) if (length(unique(duplicates[[x]])) > 1) {
        x
      } else {
        NA_character_
      }) |>
      # Remove NA values
      discard(is.na) |>
      # Combine the names of fields causing duplication into one string
      str_c(collapse = "-")
  }

#' Find duplicates
#'
#' Determine how many duplicate records are in the data and return a table.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom stringr str_detect
#' @importFrom rlang syms
#' @importFrom dplyr reframe
#' @importFrom dplyr pick
#'
#' @inheritParams duplicateFix
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

duplicateFinder <-
  function(current_data) {

    # Create a tibble of HIP duplicates
    duplicates <-
      current_data |>
      # Assign record type
      duplicateRecordType() |>
      # Filter out permits
      filter(record_type != "PMT") |>
      # Filter out non-duplicate records
      group_by(!!!syms(REF_HUNTER_ID_FIELDS)) |>
      filter(n() > 1) |>
      ungroup() |>
      # Sort
      arrange(!!!syms(REF_HUNTER_ID_FIELDS))

    # Define the fields to check for cause of duplication
    fields_to_check <-
      c("title", "middle", "suffix", "address", "city", "zip", "issue_date",
        "hunt_mig_birds", "registration_yr", "email", "dl_date",
        "dl_cycle")

    # Determine the cause(s) of registration duplication for each hunter
    dupl_tibble <-
      duplicates |>
      group_by(!!!syms(REF_HUNTER_ID_FIELDS)) |>
      # Hunter key per individual
      mutate(hunter_key = cur_group_id()) |>
      ungroup() |>
      group_by(hunter_key, dl_state) |>
      # Determine which fields are different between the duplicates to interpret
      # why hunters are in the data more than once
      reframe(
        duplicate_field =
          duplicateFields(pick(!!!fields_to_check), fields_to_check)) |>
      # Blank strings indicate an unequal bag value among duplicates
      mutate(
        duplicate_field =
          ifelse(str_detect(duplicate_field, "^$"), "bag", duplicate_field))

    # Return a message of how many duplicates are in the data
    message(
      paste(
        "There are", length(unique(dupl_tibble$hunter_key)), "registrations",
        "with duplicates;", nrow(duplicates), "total duplicated records."))

    if(nrow(dupl_tibble) > 0) {
      return(dupl_tibble |> count(duplicate_field))
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

    # Find duplicates
    dupl_tibble <- duplicateFinder(current_data)

    # Define regular expression for one field
    regex_field <- "[a-z|a-z\\_a-z]{1,}"

    # Plot
    dupl_plot <-
      dupl_tibble |>
      # Bin into generic "2+ fields" if more than one field contributes to a
      # duplicate
      mutate(
        duplicate_field =
          case_when(
            # 5+ fields
            str_detect(
              duplicate_field,
              paste0(
                "[a-z|a-z\\_a-z|a-z|a-z\\_a-z\\_a-z|a-z\\_a-z]{1,}",
                paste(rep(regex_field, 4), collapse = "\\-"))) ~
              "2+ fields",
            # 4 fields
            str_detect(
              duplicate_field, paste(rep(regex_field, 4), collapse = "\\-")) ~
              "2+ fields",
            # 3 fields
            str_detect(
              duplicate_field, paste(rep(regex_field, 3), collapse = "\\-")) ~
              "2+ fields",
            str_detect(
              duplicate_field, paste(rep(regex_field, 2), collapse = "\\-")) ~
              "2+ fields",
            TRUE ~ duplicate_field)
      ) |>
      # Make a new col to reorder the bars
      group_by(duplicate_field) |>
      mutate(total_count = n()) |>
      ungroup() |>
      ggplot(aes(x = reorder(duplicate_field, -total_count))) +
      geom_bar(stat = "count") +
      geom_text(
        aes(
          x = duplicate_field,
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
