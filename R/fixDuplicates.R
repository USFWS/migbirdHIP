#' Fix duplicates
#'
#' Resolve duplicate HIP records.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr across
#' @importFrom dplyr vars
#' @importFrom dplyr matches
#' @importFrom dplyr select
#' @importFrom dplyr anti_join
#' @importFrom dplyr slice_sample
#' @importFrom dplyr n_distinct
#' @importFrom magrittr %<>%
#' @importFrom stringr str_replace
#' @importFrom lubridate mdy
#' @importFrom purrr pmap_chr
#'
#' @param x A current data table created by \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

fixDuplicates <-
  function(x){

    # --------------------------------------------------------------------------
    # PART 1: Identify duplicate records

    duplicates <-
      x %>%
      # Group by firstname, lastname, state, birth date, download state, and
      # registration year to determine each unique hunter
      group_by(
        firstname, lastname, state, birth_date, dl_state, registration_yr) %>%
      # Identify duplicates, aka records in groups of n > 1 that belong to the
      # same individual
      mutate(
        duplicate =
          ifelse(
            n() > 1,
            paste0("duplicate_", cur_group_id()),
            "single")) %>%
      ungroup() %>%
      # Filter out non-duplicate records
      filter(str_detect(duplicate, "duplicate"))

    # Define sea duck & brant states
    sdbr_states <-
      c("AK", "CA", "CT", "DE", "MA", "MD", "NH", "NJ", "NY", "RI", "VA")

    # Define sea duck (w/o brant) states
    seaduck_states <- c("ME")

    # Define brant (w/o seaduck) states
    brant_states <- c("NC")

    # Define permit states
    permit_states <- c("WA", "OR", "CO", "SD")

    # --------------------------------------------------------------------------
    # PART 2: Sea duck AND brant states duplicate resolution

    # For sea duck AND brant states, we handle duplicates by:
    # 1. Keep record(s) with the most recent issue date.
    # 2. From the remaining records, keep the ones with a most recent
    #    (aka hopefully current) registration year.
    # 3. Exclude records with all 1s or all 0s in bag columns from consideration.
    # 4. Keep any records that have a 2 for either brant or sea duck.
    # 5. If more than one record remains, choose to keep one randomly.

    sdbr_dupes <-
      duplicates %>%
      # Filter to sea duck AND brant states
      filter(dl_state %in% sdbr_states) %>%
      group_by(duplicate) %>%
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
            "bad_issue_date_format")) %>%
      ungroup() %>%
      # If the issue date was in the right format, keep the record(s) from each
      # group that were the most recent; if the issue date was in the wrong
      # format, keep those too for future evaluation
      filter(!is.na(x_issue_date)) %>%
      select(-x_issue_date)

    # Quick-check variables
    sdbr1 <-
      duplicates %>%
      filter(dl_state %in% sdbr_states) %>% select(duplicate) %>% distinct()
    sdbr2 <- sdbr_dupes %>% select(duplicate) %>% distinct()

    # Error checker #1a
    # Thrown when the filter above for sdbr_dupes removes too many records.
    if(nrow(sdbr2) != nrow(sdbr1)){
      message("Error 1a: Internal data issue. Is there a frame shift?")
      print(
        duplicates %>%
          filter(duplicate %in% pull(anti_join(sdbr1, sdbr2, by = "duplicate"))))}

    # Error checker #1b
    # Thrown when there is a bad issue date detected.
    if(nrow(sdbr2) != nrow(sdbr1)){
      message("Error 1b: Internal data issue. Is there a frame shift?")
      print(
        sdbr_dupes %>%
          mutate(
            x_issue_date =
              ifelse(
                str_detect(issue_date, "^[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}$"),
                "good",
                "bad_issue_date_format")) %>%
          filter(x_issue_date == "bad_issue_date_format") %>%
          select(birth_date:dove_bag, record_key))}

    sdbr_dupes %<>%
      mutate(
        # Check records for all 0s or all 1s
        x_bags =
          pmap_chr(
            select(
              sdbr_dupes,
              matches("bag|coots|rails|cranes|pigeon|brant|seaducks")),
            ~case_when(
              # Look for 0s in every species column
              all(c(...) == "0") ~ "zeros",
              # Look for 1s in every species column
              all(c(...) == "1") ~ "ones",
              # Otherwise, the record passes this test
              TRUE ~ "keeper")),
        # Check records for 2 in brant or seaduck field
        x_sdbrs = ifelse(brant == "2"|seaducks == "2", "keeper", NA)) %>%
      # Convert x_bags if "keeper" to the number of records in the group with
      # the "keeper" value; otherwise, indicate 0s or 1s
      group_by(duplicate, x_bags) %>%
      mutate(x_bags = ifelse(x_bags == "keeper", as.character(n()), x_bags)) %>%
      ungroup() %>%
      # Convert x_sdbrs that passed the check above to equal the number of
      # records in the group that count as "keeper"
      group_by(duplicate, x_sdbrs) %>%
      mutate(
        x_sdbrs =
          ifelse(!is.na(x_sdbrs), as.character(n()), x_sdbrs)) %>%
      ungroup() %>%
      # Make decisions on which record to keep for each group
      group_by(duplicate) %>%
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
            TRUE ~ NA_character_)) %>%
      # If NA records have another qualifying record in their group, drop them
      mutate(
        decision =
          ifelse(
            n() > 1 & length(unique(decision)) > 1 & is.na(decision),
            "drop",
            decision)) %>%
      ungroup() %>%
      filter(decision != "drop")

    # Error checker #2
    # Thrown when there are still NAs in the decision column (which means some
    # cases in the data were not addressed in the case_when above)
    if(nrow(sdbr_dupes %>% filter(is.na(decision))) > 0){
      message("Error 2: Internal data issue. NAs left over after processing.")
      print(
        sdbr_dupes %>%
          filter(is.na(decision)) %>%
          select(duplicate:decision))}

    # Error checker #3
    # Thrown when the case_when in the script above accidentally set more than
    # one record per hunter as a "keeper"
    if(
      nrow(
        sdbr_dupes %>%
        filter(decision == "keeper") %>%
        group_by(duplicate, decision) %>%
        filter(n() > 1)) != 0){
      message("Error 3: More than one record marked to keep per hunter.")
      print(
        sdbr_dupes %>%
          group_by(duplicate, decision) %>%
          filter(n() > 1))}

    # Get the final frame with 1 record per hunter
    if(nrow(filter(sdbr_dupes, decision == "dupl")) > 0){
      sdbr_dupes <-
        bind_rows(
          # Handle "dupl"s; randomly keep one per group using slice_sample()
          sdbr_dupes %>%
            filter(decision == "dupl") %>%
            group_by(duplicate) %>%
            slice_sample(n = 1) %>%
            ungroup(),
          # Row bind in the "keepers" (should already be 1 per hunter)
          sdbr_dupes %>%
            filter(decision == "keeper")) %>%
        # Drop unneeded columns
        select(-c(x_bags:decision)) %>%
        # Designate record type
        mutate(record_type = "HIP")
    }else{
      sdbr_dupes <-
        sdbr_dupes %>%
        filter(decision == "keeper") %>%
        # Drop unneeded columns
        select(-c(x_bags:decision)) %>%
        # Designate record type
        mutate(record_type = "HIP")}

    # Error checker #4
    # Thrown when the number of unique hunters in the final sdbr_dupes table are
    # not equal to the initial number of distinct hunters that fall in the sd
    # category
    if(n_distinct(sdbr_dupes$duplicate) != nrow(sdbr1)){
      message("Error 4: Not all duplicate hunters were handled.")
      print(
        anti_join(
          duplicates %>% filter(dl_state %in% sdbr_states),
          sdbr_dupes,
          by = c("duplicate"))
      )}

    # --------------------------------------------------------------------------
    # PART 3: Sea duck states duplicate resolution (Maine)

    # For sea duck states, we handle duplicates by:
    # 1. Keep record(s) with the most recent issue date.
    # 2. From the remaining records, keep the ones with a most recent
    #    (aka hopefully current) registration year.
    # 3. Exclude records with all 1s or all 0s in bag columns from consideration.
    # 4. Keep any records that have a 2 for sea duck.
    # 5. If more than one record remains, choose to keep one randomly.

    seaduck_dupes <-
      duplicates %>%
      # Filter to seaduck states
      filter(dl_state %in% seaduck_states) %>%
      group_by(duplicate) %>%
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
            "bad_issue_date_format")) %>%
      ungroup() %>%
      # If the issue date was in the right format, keep the record(s) from each
      # group that were the most recent; if the issue date was in the wrong
      # format, keep those too for future evaluation
      filter(!is.na(x_issue_date)) %>%
      select(-x_issue_date)

    # Quick-check variables
    sd1 <-
      duplicates %>%
      filter(dl_state %in% seaduck_states) %>% select(duplicate) %>% distinct()
    sd2 <- seaduck_dupes %>% select(duplicate) %>% distinct()

    # Error checker #1a
    # Thrown when the filter above for seaduck_dupes removes too many records.
    if(nrow(sd2) != nrow(sd1)){
      message("Error 1a: Internal data issue. Is there a frame shift?")
      print(
        duplicates %>%
          filter(duplicate %in% pull(anti_join(sd1, sd2, by = "duplicate"))))}

    # Error checker #1b
    # Thrown when there is a bad issue date detected.
    if(nrow(sd2) != nrow(sd1)){
      message("Error 1b: Internal data issue. Is there a frame shift?")
      print(
        seaduck_dupes %>%
          mutate(
            x_issue_date =
              ifelse(
                str_detect(issue_date, "^[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}$"),
                "good",
                "bad_issue_date_format")) %>%
          filter(x_issue_date == "bad_issue_date_format") %>%
          select(birth_date:dove_bag, record_key))}

    seaduck_dupes %<>%
      mutate(
        # Check records for all 0s or all 1s
        x_bags =
          pmap_chr(
            select(
              seaduck_dupes,
              matches("bag|coots|rails|cranes|pigeon|brant|seaducks")),
            ~case_when(
              # Look for 0s in every species column
              all(c(...) == "0") ~ "zeros",
              # Look for 1s in every species column
              all(c(...) == "1") ~ "ones",
              # Otherwise, the record passes this test
              TRUE ~ "keeper")),
        # Check records for 2 in seaduck field
        x_seaducks = ifelse(seaducks == "2", "keeper", NA)) %>%
      # Convert x_bags if "keeper" to the number of records in the group with
      # the "keeper" value; otherwise, indicate 0s or 1s
      group_by(duplicate, x_bags) %>%
      mutate(x_bags = ifelse(x_bags == "keeper", as.character(n()), x_bags)) %>%
      ungroup() %>%
      # Convert x_seaducks that passed the check above to equal the number of
      # records in the group that count as "keeper"
      group_by(duplicate, x_seaducks) %>%
      mutate(
        x_seaducks =
          ifelse(!is.na(x_seaducks), as.character(n()), x_seaducks)) %>%
      ungroup() %>%
      # Make decisions on which record to keep for each group
      group_by(duplicate) %>%
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
            TRUE ~ NA_character_)) %>%
      # If NA records have another qualifying record in their group, drop them
      mutate(
        decision =
          ifelse(
            n() > 1 & length(unique(decision)) > 1 & is.na(decision),
            "drop",
            decision)) %>%
      ungroup() %>%
      filter(decision != "drop")

    # Error checker #2
    # Thrown when there are still NAs in the decision column (which means some
    # cases in the data were not addressed in the case_when above)
    if(nrow(seaduck_dupes %>% filter(is.na(decision))) > 0){
      message("Error 2: Internal data issue. NAs left over after processing.")
      print(
        seaduck_dupes %>%
          filter(is.na(decision)) %>%
          select(duplicate:decision))}

    # Error checker #3
    # Thrown when the case_when in the script above accidentally set more than
    # one record per hunter as a "keeper"
    if(
      nrow(
        seaduck_dupes %>%
        filter(decision == "keeper") %>%
        group_by(duplicate, decision) %>%
        filter(n() > 1)) != 0){
      message("Error 3: More than one record marked to keep per hunter.")
      print(
        seaduck_dupes %>%
          group_by(duplicate, decision) %>%
          filter(n() > 1))}

    # Get the final frame with 1 record per hunter
    if(nrow(filter(seaduck_dupes, decision == "dupl")) > 0){
      seaduck_dupes <-
        bind_rows(
          # Handle "dupl"s; randomly keep one per group using slice_sample()
          seaduck_dupes %>%
            filter(decision == "dupl") %>%
            group_by(duplicate) %>%
            slice_sample(n = 1) %>%
            ungroup(),
          # Row bind in the "keepers" (should already be 1 per hunter)
          seaduck_dupes %>%
            filter(decision == "keeper")) %>%
        # Drop unneeded columns
        select(-c(x_bags:decision)) %>%
        # Designate record type
        mutate(record_type = "HIP")
    }else{
      seaduck_dupes <-
        seaduck_dupes %>%
        filter(decision == "keeper") %>%
        # Drop unneeded columns
        select(-c(x_bags:decision)) %>%
        # Designate record type
        mutate(record_type = "HIP")}

    # Error checker #4
    # Thrown when the number of unique hunters in the final seaduck_dupes table are
    # not equal to the initial number of distinct hunters that fall in the sd
    # category
    if(n_distinct(seaduck_dupes$duplicate) != nrow(sd1)){
      message("Error 4: Not all duplicate hunters were handled.")
      print(
        anti_join(
          duplicates %>% filter(dl_state %in% seaduck_states),
          seaduck_dupes,
          by = c("duplicate"))
      )}

    # --------------------------------------------------------------------------
    # PART 4: Brant states duplicate resolution (North Carolina)

    # For brant states, we handle duplicates by:
    # 1. Keep record(s) with the most recent issue date.
    # 2. From the remaining records, keep the ones with a most recent
    #    (aka hopefully current) registration year.
    # 3. Exclude records with all 1s or all 0s in bag columns from consideration.
    # 4. Keep any records that have a 2 for brant.
    # 5. If more than one record remains, choose to keep one randomly.

    brant_dupes <-
      duplicates %>%
      # Filter to brant states
      filter(dl_state %in% brant_states) %>%
      group_by(duplicate) %>%
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
            "bad_issue_date_format")) %>%
      ungroup() %>%
      # If the issue date was in the right format, keep the record(s) from each
      # group that were the most recent; if the issue date was in the wrong
      # format, keep those too for future evaluation
      filter(!is.na(x_issue_date)) %>%
      select(-x_issue_date)

    # Quick-check variables
    br1 <-
      duplicates %>%
      filter(dl_state %in% brant_states) %>% select(duplicate) %>% distinct()
    br2 <- brant_dupes %>% select(duplicate) %>% distinct()

    # Error checker #5a
    # Thrown when the filter above for brant_dupes removes too many records.
    if(nrow(br2) != nrow(br1)){
      message("Error 5a: Internal data issue. Is there a frame shift?")
      print(
        duplicates %>%
          filter(duplicate %in% pull(anti_join(br1, br2, by = "duplicate"))))}

    # Error checker #5b
    # Thrown when there is a bad issue date detected.
    if(nrow(br2) != nrow(br1)){
      message("Error 5b: Internal data issue. Is there a frame shift?")
      print(
        brant_dupes %>%
          mutate(
            x_issue_date =
              ifelse(
                str_detect(issue_date, "^[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}$"),
                "good",
                "bad_issue_date_format")) %>%
          filter(x_issue_date == "bad_issue_date_format") %>%
          select(birth_date:dove_bag, record_key))}

    brant_dupes %<>%
      mutate(
        # Check records for all 0s or all 1s
        x_bags =
          pmap_chr(
            select(
              brant_dupes,
              matches("bag|coots|rails|cranes|pigeon|brant|seaducks")),
            ~case_when(
              # Look for 0s in every species column
              all(c(...) == "0") ~ "zeros",
              # Look for 1s in every species column
              all(c(...) == "1") ~ "ones",
              # Otherwise, the record passes this test
              TRUE ~ "keeper")),
        # Check records for 2 in brant field
        x_seaducks = ifelse(brant == "2", "keeper", NA)) %>%
      # Convert x_bags if "keeper" to the number of records in the group with
      # the "keeper" value; otherwise, indicate 0s or 1s
      group_by(duplicate, x_bags) %>%
      mutate(x_bags = ifelse(x_bags == "keeper", as.character(n()), x_bags)) %>%
      ungroup() %>%
      # Convert x_seaducks that passed the check above to equal the number of
      # records in the group that count as "keeper"
      group_by(duplicate, x_seaducks) %>%
      mutate(
        x_seaducks =
          ifelse(!is.na(x_seaducks), as.character(n()), x_seaducks)) %>%
      ungroup() %>%
      # Make decisions on which record to keep for each group
      group_by(duplicate) %>%
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
            TRUE ~ NA_character_)) %>%
      # If NA records have another qualifying record in their group, drop them
      mutate(
        decision =
          ifelse(
            n() > 1 & length(unique(decision)) > 1 & is.na(decision),
            "drop",
            decision)) %>%
      ungroup() %>%
      filter(decision != "drop")

    # Error checker #6
    # Thrown when there are still NAs in the decision column (which means some
    # cases in the data were not addressed in the case_when above)
    if(nrow(brant_dupes %>% filter(is.na(decision))) > 0){
      message("Error 6: Internal data issue. NAs left over after processing.")
      print(
        brant_dupes %>%
          filter(is.na(decision)) %>%
          select(duplicate:decision))}

    # Error checker #7
    # Thrown when the case_when in the script above accidentally set more than
    # one record per hunter as a "keeper"
    if(
      nrow(
        brant_dupes %>%
        filter(decision == "keeper") %>%
        group_by(duplicate, decision) %>%
        filter(n() > 1)) != 0){
      message("Error 7: More than one record marked to keep per hunter.")
      print(
        brant_dupes %>%
          group_by(duplicate, decision) %>%
          filter(n() > 1))}

    # Get the final frame with 1 record per hunter
    if(nrow(filter(brant_dupes, decision == "dupl")) > 0){
      brant_dupes <-
        bind_rows(
          # Handle "dupl"s; randomly keep one per group using slice_sample()
          brant_dupes %>%
            filter(decision == "dupl") %>%
            group_by(duplicate) %>%
            slice_sample(n = 1) %>%
            ungroup(),
          # Row bind in the "keepers" (should already be 1 per hunter)
          brant_dupes %>%
            filter(decision == "keeper")) %>%
        # Drop unneeded columns
        select(-c(x_bags:decision)) %>%
        # Designate record type
        mutate(record_type = "HIP")
    }else{
      brant_dupes <-
        brant_dupes %>%
        filter(decision == "keeper") %>%
        # Drop unneeded columns
        select(-c(x_bags:decision)) %>%
        # Designate record type
        mutate(record_type = "HIP")}

    # Error checker #8
    # Thrown when the number of unique hunters in the final brant_dupes table are
    # not equal to the initial number of distinct hunters that fall in the br
    # category
    if(n_distinct(brant_dupes$duplicate) != nrow(br1)){
      message("Error 8: Not all duplicate hunters were handled.")
      print(
        anti_join(
          duplicates %>% filter(dl_state %in% brant_states),
          brant_dupes,
          by = c("duplicate"))
      )}

    # --------------------------------------------------------------------------
    # PART 5: Non-permit, non-seaduck, non-brant record duplicate resolution

    other_dupes <-
      duplicates %>%
      # Record not from seaduck, brant, or permit state
      filter(
        !(dl_state %in% sdbr_states) &
        !(dl_state %in% seaduck_states) &
        !(dl_state %in% brant_states) &
        !(dl_state %in% permit_states)) %>%
      # Repeat duplicate checking
      group_by(duplicate) %>%
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
            "bad_issue_date_format")) %>%
      ungroup() %>%
      filter(!is.na(x_issue_date)) %>%
      select(-x_issue_date)

    # Quick-check variables
    o1 <-
      duplicates %>%
      filter(
        !(dl_state %in% sdbr_states) &
        !(dl_state %in% seaduck_states) &
          !(dl_state %in% brant_states) &
          !(dl_state %in% permit_states)) %>% select(duplicate) %>% distinct()
    o2 <- other_dupes %>% select(duplicate) %>% distinct()

    # Error checker #9a
    # Thrown when the filter above for other_dupes removes too many records.
    if(nrow(o2) != nrow(o1)){
      message("Error 9a: Internal data issue. Is there a frame shift?")
      print(
        duplicates %>%
          filter(duplicate %in% pull(anti_join(o1, o2, by = "duplicate"))))}

    # Error checker #9b
    # Thrown when there is a bad issue date detected.
    if(nrow(o2) != nrow(o1)){
      message("Error 9b: Internal data issue. Is there a frame shift?")
      print(
        other_dupes %>%
          mutate(
            x_issue_date =
              ifelse(
                str_detect(issue_date, "^[0-9]{2}\\/[0-9]{2}\\/[0-9]{4}$"),
                "good",
                "bad_issue_date_format")) %>%
          filter(x_issue_date == "bad_issue_date_format") %>%
          select(birth_date:dove_bag, record_key))}

    other_dupes %<>%
      mutate(
        # Check records for all 0s or all 1s
        x_bags =
          pmap_chr(
            select(
              other_dupes,
              matches("bag|coots|rails|cranes|pigeon|brant|seaducks")),
            ~case_when(
              # Look for 0s in every species column
              all(c(...) == "0") ~ "zeros",
              # Look for 1s in every species column
              all(c(...) == "1") ~ "ones",
              # Otherwise, the record passes this test
              TRUE ~ "keeper"))) %>%
      # Convert x_bags if "keeper" to the number of records in the group with
      # the "keeper" value; otherwise, indicate 0s or 1s
      group_by(duplicate, x_bags) %>%
      mutate(x_bags = ifelse(x_bags == "keeper", as.character(n()), x_bags)) %>%
      ungroup() %>%
      # Make decisions on which record to keep for each group
      group_by(duplicate) %>%
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
            TRUE ~ NA_character_)) %>%
      # If NA records have another qualifying record in their group, drop them
      mutate(
        decision =
          ifelse(
            n() > 1 & length(unique(decision)) > 1 & is.na(decision),
            "drop",
            decision)) %>%
      ungroup() %>%
      filter(decision != "drop")

    # Error checker #10
    # Thrown when there are still NAs in the decision column (which means some
    # cases in the data were not addressed in the case_when above)
    if(nrow(other_dupes %>% filter(is.na(decision))) > 0){
      message("Error 10: Internal data issue. NAs left over after processing.")
      print(
        other_dupes %>%
          filter(is.na(decision)) %>%
          select(duplicate:decision))}

    # Error checker #11
    # Thrown when the case_when in the script above accidentally set more than
    # one record per hunter as a "keeper"
    if(
      nrow(
        other_dupes %>%
        filter(decision == "keeper") %>%
        group_by(duplicate, decision) %>%
        filter(n() > 1)) != 0){
      message("Error 11: More than one record marked to keep per hunter.")
      print(
        other_dupes %>%
          group_by(duplicate, decision) %>%
          filter(n() > 1))}

    # Get the final frame with 1 record per hunter
    if(nrow(filter(other_dupes, decision == "dupl")) > 0){
      other_dupes <-
        bind_rows(
          # Handle "dupl"s; randomly keep one per group using slice_sample()
          other_dupes %>%
            filter(decision == "dupl") %>%
            group_by(duplicate) %>%
            slice_sample(n = 1) %>%
            ungroup(),
          # Row bind in the "keepers" (should already be 1 per hunter)
          other_dupes %>%
            filter(decision == "keeper")) %>%
        # Drop unneeded columns
        select(-c(x_bags:decision)) %>%
        # Designate record type
        mutate(record_type = "HIP")
    }else{
      other_dupes <-
        other_dupes %>%
        filter(decision == "keeper") %>%
        # Drop unneeded columns
        select(-c(x_bags:decision)) %>%
        # Designate record type
        mutate(record_type = "HIP")}

    # Error checker #12
    # Thrown when the number of unique hunters in the final other_dupes table
    # are not equal to the initial number of distinct hunters initially
    if(n_distinct(other_dupes$duplicate) != nrow(o1)){
      message("Error 12: Not all duplicate hunters were handled.")
      print(
        anti_join(
          duplicates %>%
            filter(!(dl_state %in% seaduck_states) &
                     !(dl_state %in% brant_states) &
                     !(dl_state %in% permit_states)),
          other_dupes,
          by = c("duplicate"))
      )}

    # --------------------------------------------------------------------------
    # PART 6: Permit state duplicate resolution

    # Some permit states (WA, OR, CO, SD) submit permit records separately from
    # HIP records. The status of each record will be determined and they will be
    # labeled as either HIP or permit. Multiple HIP records must be resolved
    # (keep only 1 per hunter), but multiple permits are allowed.
    # - HIP records contain nonzero values in "regular spp" columns
    # - Permit records always have 0s in "regular spp" columns

    permit_dupes <-
      duplicates %>%
      # Filter the duplicates to those that occur in permit states
      filter(dl_state %in% permit_states) %>%
      # Set "." to NA in take fields & make those fields numeric
      mutate_at(
        vars(matches("bag|coots|rails")),
        ~str_replace(., "\\.", NA_character_) %>%
          as.numeric(.)) %>%
      # Calculate sum of values in the non-permit species columns to use as a
      # proxy.
      # Note: We can no longer use "special_sum" == 0 because the value for
      # cranes is often sent as a 1. The special_sum reference col was
      # previously calculated by adding the values for brant, seaducks,
      # band_tailed_pigeon, and cranes. Using only the "regular" HIP species sum
      # is relatively reliable instead.
      mutate(
        other_sum = rowSums(across(matches("bag|coots|rails")), na.rm = T)) %>%
      # Reset bags to character for row bind later
      mutate_at(vars(matches("bag|coots|rails")), ~as.character(.)) %>%
      mutate(
        record_type =
          case_when(
            # If the sum of values in non-permit species columns is > 0, the
            # record is a HIP record
            other_sum > 0 ~ "HIP",
            # If the sum of values in permit species columns is > 0 the record
            # is a permit
            other_sum == 0 ~ "PMT",
            TRUE ~ NA_character_))

    # Error checker #13
    # Thrown when there are NAs in the record_type column after the case_when
    if((TRUE %in% is.na(permit_dupes$record_type)) == TRUE){
      message("Error 13: Some permit duplicates not identified as HIP or PMT.")
      print(
        permit_dupes %>%
          filter(is.na(record_type)) %>%
          select(duplicate) %>%
          distinct()
      )}

    # If there is more than one HIP record per person, decide which one to keep
    hip_dupes <-
      permit_dupes %>%
      filter(record_type == "HIP") %>%
      group_by(duplicate) %>%
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
            "bad_issue_date_format")) %>%
      ungroup() %>%
      # If the issue date was in the right format, keep the record(s) from each
      # group that were the most recent; if the issue date was in the wrong
      # format, keep those too for future evaluation
      filter(!is.na(x_issue_date)) %>%
      select(-x_issue_date)

    hip_dupes %<>%
      mutate(
        # Check records for all 0s or all 1s
        x_bags =
          pmap_chr(
            select(
              hip_dupes,
              matches("bag|coots|rails|cranes|pigeon|brant|seaducks")),
            ~case_when(
              # Look for 0s in every species column
              all(c(...) == "0") ~ "zeros",
              # Look for 1s in every species column
              all(c(...) == "1") ~ "ones",
              # Otherwise, the record passes this test
              TRUE ~ "keeper"))) %>%
      # Convert x_bags if "keeper" to the number of records in the group with
      # the "keeper" value; otherwise, indicate 0s or 1s
      group_by(duplicate, x_bags) %>%
      mutate(x_bags = ifelse(x_bags == "keeper", as.character(n()), x_bags)) %>%
      ungroup() %>%
      # Make decisions on which record to keep for each group
      group_by(duplicate) %>%
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
            TRUE ~ NA_character_)) %>%
      # If NA records have another qualifying record in their group, drop them
      mutate(
        decision =
          ifelse(
            n() > 1 & length(unique(decision)) > 1 & is.na(decision),
            "drop",
            decision)) %>%
      ungroup() %>%
      filter(decision != "drop") %>%
      # Remove unneeded rows
      select(-c("other_sum", "x_bags"))

    # Error checker #14
    # Thrown when there are still NAs in the decision column (which means some
    # cases in the data were not addressed in the case_when above)
    if(nrow(hip_dupes %>% filter(is.na(decision))) > 0){
      message("Error 14: Internal data issue. NAs left over after processing.")
      print(
        hip_dupes %>%
          filter(is.na(decision)))}

    # Error checker #15
    # Thrown when the case_when in the script above accidentally set more than
    # one record per hunter as a "keeper"
    if(
      nrow(
        hip_dupes %>%
        filter(decision == "keeper") %>%
        group_by(duplicate, decision) %>%
        filter(n() > 1)) != 0){
      message("Error 15: More than one record marked to keep per hunter.")
      print(
        hip_dupes %>%
          group_by(duplicate, decision) %>%
          filter(n() > 1))}

    # Error checker #16
    # Thrown when the number of unique hunters in the final permit_dupes table
    # are not equal to the initial number of distinct hunters initially
    if(n_distinct(hip_dupes$duplicate) !=
       n_distinct(
         permit_dupes %>%
         filter(record_type == "HIP") %>%
         select(duplicate))
    ){
      message("Error 16: Not all duplicate hunters were handled.")
      print(
        anti_join(
          duplicates %>%
            filter(dl_state %in% permit_states),
          hip_dupes,
          by = c("duplicate"))
      )}

    # Get the final frame with 1 record per hunter
    if(nrow(filter(hip_dupes, decision == "dupl")) > 0){
      hip_dupes <-
        bind_rows(
          # Handle "dupl"s; randomly keep one per group using slice_sample()
          hip_dupes %>%
            filter(decision == "dupl") %>%
            group_by(duplicate) %>%
            slice_sample(n = 1) %>%
            ungroup(),
          # Row bind in the "keepers" (should already be 1 per hunter)
          hip_dupes %>%
            filter(decision == "keeper"))
    }else{
      hip_dupes <-
        # Keep the "keepers" (should already be 1 per hunter)
        hip_dupes %>%
        filter(decision == "keeper")}

    # Get table of just permits
    permit_dupes %<>%
      filter(record_type == "PMT")

    # --------------------------------------------------------------------------
    # PART 7: Combine all resolved records into one tibble

    resolved_duplicates <-
      # Remove duplicates from the input frame
      x %>%
      group_by(
        firstname, lastname, state, birth_date, dl_state, registration_yr) %>%
      mutate(
        duplicate =
          ifelse(
            n() > 1,
            paste0("duplicate_", cur_group_id()),
            "single")) %>%
      ungroup() %>%
      filter(!str_detect(duplicate, "duplicate")) %>%
      select(-duplicate) %>%
      # These records should all be HIP
      mutate(record_type = "HIP") %>%
      # Classify solo permit records as PMT
      mutate_at(
        vars(matches("bag|coots|rails|band|brant|seaducks")),
        ~as.numeric(.)) %>%
      mutate(
        other_sum =
          rowSums(across(matches("bag|coots|rails")), na.rm = T),
        special_sum =
          rowSums(across(matches("band|brant|seaducks")), na.rm = T)) %>%
      mutate_at(
        vars(matches("bag|coots|rails|band|brant|seaducks")),
        ~as.character(.)) %>%
      mutate(
        record_type =
          ifelse(
            other_sum == 0 & special_sum > 0 & dl_state %in% permit_states,
            "PMT",
            record_type)) %>%
      select(-c(other_sum, special_sum)) %>%
      # Add back in the resolved duplicates
      bind_rows(
        sdbr_dupes %>% select(-duplicate),
        seaduck_dupes %>% select(-duplicate),
        brant_dupes %>% select(-duplicate),
        other_dupes %>% select(-duplicate),
        permit_dupes %>% select(-c("duplicate", "other_sum")),
        hip_dupes %>% select(-c("duplicate", "decision"))) %>%
      distinct()

    # Error checker #17
    if(
      n_distinct(
        x %>%
        select(
          firstname, lastname, state, birth_date, dl_state, registration_yr)) !=
      n_distinct(
        resolved_duplicates %>%
        select(
          firstname, lastname, state, birth_date, dl_state, registration_yr))
    ){
      message(
        paste0(
          "Error 17: One or more hunters lost in the duplicate resolution",
          " process."))
      print(
        anti_join(
          x %>%
            select(
              firstname,
              lastname,
              state,
              birth_date,
              dl_state,
              registration_yr) %>%
            distinct(),
          resolved_duplicates %>%
            select(
              firstname,
              lastname,
              state,
              birth_date,
              dl_state,
              registration_yr),
          by =
            c("firstname",
              "lastname",
              "state",
              "birth_date",
              "dl_state",
              "registration_yr"))
      )}

    return(resolved_duplicates)
  }
