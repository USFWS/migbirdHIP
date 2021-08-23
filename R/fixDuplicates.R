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
#' @param x A cleaned data table created by \code{\link{clean}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
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

    # Define Atlantic Flyway states
    af_states <-
      c("ME", "VT", "NH", "MA", "RI", "CT", "NY", "PA", "NY", "DE", "MD", "VA",
        "WV", "NC", "SC", "GA", "FL")

    # Define permit states
    permit_states <- c("WA", "OR", "CO", "SD")

    # --------------------------------------------------------------------------
    # PART 2: Atlantic Flyway states duplicate resolution

    # Atlantic Flyway states (ME, VT, NH, MA, RI, CT, NY, PA, NY, DE, MD, VA,
    # WV, NC, SC, GA, FL) do not issue permits but there are permit species. For
    # these states, we handle duplicates differently by:
    # 1. Keep record(s) with the most recent issue date.
    # 2. From the remaining records, keep the ones with a most recent
    #    (aka hopefully current) registration year.
    # 3. Exclude records with all 1s or all 0s in bag columns from consideration.
    # 4. Keep any records that have a 2 for either brant or sea duck.
    # 5. If more than one record remains, choose to keep one randomly.

    af_dupes <-
      duplicates %>%
      # Filter to Atlantic Flyway states
      filter(dl_state %in% af_states) %>%
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
      filter(x_issue_date == "keeper") %>%
      select(-x_issue_date)

    # Quick-check variables
    af1 <-
      duplicates %>%
      filter(dl_state %in% af_states) %>% select(duplicate) %>% distinct()
    af2 <- af_dupes %>% select(duplicate) %>% distinct()

    # Error checker #1
    # Thrown when the filter above for af_dupes removes too many records.
    if(nrow(af2) != nrow(af1)){
      message("Error 1: Internal data issue. Is there a frame shift?")
      print(
        duplicates %>%
          filter(duplicate %in% pull(anti_join(af1, af2, by = "duplicate"))))}

    af_dupes %<>%
      group_by(duplicate) %>%
      mutate(
        # Check records for most recent registration year
        x_reg_yr =
          # Skip frame shift issues and only evaluate for years in correct yyyy
          # format
          ifelse(
            str_detect(registration_yr, "^[0-9]{4}$"),
            ifelse(
              registration_yr ==
                ifelse(
                  !is.na(registration_yr),
                  as.character(max(as.numeric(registration_yr), na.rm = TRUE)),
                  NA),
              n(),
              NA),
            999))%>%
      ungroup()

    af_dupes %<>%
      mutate(
        # Check records for all 0s or all 1s
        x_bags =
          pmap_chr(
            select(
              af_dupes,
              matches("bag|coots|rails|cranes|pigeon|brant|seaducks")),
            ~case_when(
              # Look for 0s in every species column
              all(c(...) == "0") ~ "zeros",
              # Look for 1s in every species column
              all(c(...) == "1") ~ "ones",
              # Otherwise, the record passes this test
              TRUE ~ "keeper")),
        # Check records for 2 in brant or seaduck field
        x_seaducks = ifelse(brant == "2"|seaducks == "2", "keeper", NA)) %>%
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
            # the registration year, bag, and seaduck checks above, keep it
            x_reg_yr == 1 & x_bags == 1 & x_seaducks == 1 ~ "keeper",
            # When there isn't a 1 value in any of the checking columns, it's a
            # duplicate still and we will need to randomly choose which record
            # in the group to keep later
            !(1 %in% x_reg_yr)&!(1 %in% x_bags)&!(1 %in% x_seaducks) ~ "dupl",
            # If there is a 1 in the seaduck checks col, then that record is the
            # one we want to keep
            x_seaducks == 1 ~ "keeper",
            # For rare cases that have two records: two registration years, one
            # record all 1s and the other not all 1s, keep the record with not
            # all 1s
            x_reg_yr == 2 & x_bags == 1 ~ "keeper",
            # Another rare case: two records, one with all 1s and the other not
            # all 1s, BUT the record without all 1s is from the "wrong"
            # registration year (although issue dates are the same for both);
            # keep the record with the not all 1s bag values
            is.na(x_reg_yr) & x_bags == 1 ~ "keeper",
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
    if(nrow(af_dupes %>% filter(is.na(decision))) > 0){
      message("Error 2: Internal data issue. NAs left over after processing.")
      print(
        af_dupes %>%
          filter(is.na(decision)) %>%
          select(duplicate:decision))}

    # Error checker #3
    # Thrown when the case_when in the script above accidentally set more than
    # one record per hunter as a "keeper"
    if(
      nrow(
        af_dupes %>%
        filter(decision == "keeper") %>%
        group_by(duplicate, decision) %>%
        filter(n() > 1)) != 0){
      message("Error 3: More than one record marked to keep per hunter.")
      print(
        af_dupes %>%
          group_by(duplicate, decision) %>%
          filter(n() > 1))}

    # Get the final frame with 1 record per hunter
    af_dupes %<>%
      bind_rows(
        # Handle "dupl"s; randomly keep one per group using slice_sample()
        af_dupes %>%
          filter(decision == "dupl") %>%
          group_by(duplicate) %>%
          slice_sample(n = 1) %>%
          ungroup(),
        # Row bind in the "keepers" (should already be 1 per hunter)
        af_dupes %>%
          filter(decision == "keeper")) %>%
      # Drop unneeded columns
      select(-c(x_reg_yr:decision)) %>%
      # Designate record type
      mutate(record_type = "HIP")

    # Error checker #4
    # Thrown when the number of unique hunters in the final af_dupes table are
    # not equal to the initial number of distinct hunters that fall in the AF
    # category
    if(n_distinct(af_dupes$duplicate) != nrow(af1)){
      message("Error 4: Not all duplicate hunters were handled.")
      print(
        anti_join(
          duplicates %>% filter(dl_state %in% af_states),
          af_dupes,
          by = c("duplicate"))
      )}

    # --------------------------------------------------------------------------
    # PART 3: Non-permit state, non-AF record duplicate resolution

    # Iowa duplicates

    ia_dupes <-
      duplicates %>%
      filter(dl_state == "IA") %>%
      # Handle special Iowa dove cases, in which dove records are sent
      # separately from HIP
      mutate(
        record_type =
          case_when(
            # For Iowa records, identify "HIP" records as those with 0s in dove
            dove_bag == "0" ~ "HIP",
            # For Iowa records, identify "IAD" records as those with >0 in dove
            dove_bag != "0" ~ "IAD",
            TRUE ~ NA_character_))

    ia_hips <-
      ia_dupes %>%
      filter(record_type == "HIP")

    ia_doves <-
      ia_dupes %>%
      filter(record_type == "IAD") %>%
      group_by(duplicate) %>%
      slice_sample(n = 1) %>%
      ungroup()

    rm(ia_dupes)

    # Error checker #5
    # Make sure number of rows are equal between 1) duplicates not belonging to
    # AF or permit states and 2) the tables made from explicitly excluding and
    # including Iowa. These tables might not be equal when dl_state == NA, which
    # occurs when file names are not in the standard format
    if(length(unique(ia_hips$duplicate)) > 1){
      message("Error 5a: Extra duplicates (n > 1) detected in Iowa HIPs.")
      print(ia_hips %>% group_by(duplicate) %>% filter(n() > 1))
    }

    # Error checker #6
    # Make sure number of rows are equal between 1) duplicates not belonging to
    # AF or permit states and 2) the tables made from explicitly excluding and
    # including Iowa. These tables might not be equal when dl_state == NA, which
    # occurs when file names are not in the standard format
    if(
      nrow(
        duplicates %>%
        filter(!(dl_state %in% af_states) & !(dl_state %in% permit_states))) !=
      nrow(
        bind_rows(
          duplicates %>%
          filter(!(dl_state %in% af_states) &
                 !(dl_state %in% permit_states) &
                 dl_state != "IA"),
          duplicates %>%
          filter(dl_state == "IA")))
    ){
      message(
        paste0(
          "Error 6: Inconsistent number of rows between tables.\n",
          "Is there an error in the standardization of source file name(s)?"))
      print(
        anti_join(
          duplicates %>%
            filter(!(dl_state %in% af_states) &!(dl_state %in% permit_states)),
          bind_rows(
            duplicates %>%
              filter(!(dl_state %in% af_states) &
                       !(dl_state %in% permit_states) &
                       dl_state != "IA"),
            duplicates %>%
              filter(dl_state == "IA")),
          by = c("duplicate")) %>%
          select(source_file) %>%
          distinct()
      )}

    # Non-permit state, non-AF state duplicates

    other_dupes <-
      duplicates %>%
      # Record not from: Atlantic Flyway state, permit state, or Iowa
      filter(!(dl_state %in% af_states) & !(dl_state %in% permit_states) &
               dl_state != "IA") %>%
      # Repeat duplicate checking, similar to as above (in Part 2)
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
        !(dl_state %in% af_states) &
          !(dl_state %in% permit_states) &
          dl_state != "IA") %>% select(duplicate) %>% distinct()
    o2 <- other_dupes %>% select(duplicate) %>% distinct()

    # Error checker #7
    # Thrown when the filter above for other_dupes removes too many records.
    if(nrow(o2) != nrow(o1)){
      message("Error 7: Internal data issue. Is there a frame shift?")
      print(
        duplicates %>%
          filter(duplicate %in% pull(anti_join(o1, o2, by = "duplicate"))))}

    other_dupes %<>%
      group_by(duplicate) %>%
      mutate(
        # Check records for most recent registration year
        x_reg_yr =
          # Skip frame shift issues and only evaluate for years in correct yyyy
          # format
          ifelse(
            str_detect(registration_yr, "^[0-9]{4}$"),
            ifelse(
              registration_yr ==
                ifelse(
                  !is.na(registration_yr),
                  as.character(max(as.numeric(registration_yr), na.rm = TRUE)),
                  NA),
              n(),
              NA),
            999))%>%
      ungroup()

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
            # the registration year and bag checks above, keep it
            x_reg_yr == 1 & x_bags == 1 ~ "keeper",
            # When there isn't a 1 value in either of the checking columns, it's
            # a duplicate still and we will need to randomly choose which record
            # in the group to keep later
            !(1 %in% x_reg_yr)&!(1 %in% x_bags) ~ "dupl",
            # For rare cases that have two records: two registration years, one
            # record all 1s and the other not all 1s, keep the record with not
            # all 1s
            x_reg_yr == 2 & x_bags == 1 ~ "keeper",
            # Another rare case: two records, one with all 1s and the other not
            # all 1s, BUT the record without all 1s is from the "wrong"
            # registration year (although issue dates are the same for both);
            # keep the record with the not all 1s bag values
            is.na(x_reg_yr) & x_bags == 1 ~ "keeper",
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

    # Error checker #8
    # Thrown when there are still NAs in the decision column (which means some
    # cases in the data were not addressed in the case_when above)
    if(nrow(other_dupes %>% filter(is.na(decision))) > 0){
      message("Error 8: Internal data issue. NAs left over after processing.")
      print(
        other_dupes %>%
          filter(is.na(decision)) %>%
          select(duplicate:decision))}

    # Error checker #9
    # Thrown when the case_when in the script above accidentally set more than
    # one record per hunter as a "keeper"
    if(
      nrow(
        other_dupes %>%
        filter(decision == "keeper") %>%
        group_by(duplicate, decision) %>%
        filter(n() > 1)) != 0){
      message("Error 9: More than one record marked to keep per hunter.")
      print(
        other_dupes %>%
          group_by(duplicate, decision) %>%
          filter(n() > 1))}

    # Get the final frame with 1 record per hunter
    other_dupes %<>%
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
      select(-c(x_reg_yr:decision)) %>%
      # Designate record type
      mutate(record_type = "HIP")

    # Error checker #10
    # Thrown when the number of unique hunters in the final other_dupes table
    # are not equal to the initial number of distinct hunters initially
    if(n_distinct(other_dupes$duplicate) != nrow(o1)){
      message("Error 10: Not all duplicate hunters were handled.")
      print(
        anti_join(
          duplicates %>%
            filter(!(dl_state %in% af_states) &
                     !(dl_state %in% permit_states) &
                     dl_state != "IA"),
          other_dupes,
          by = c("duplicate"))
      )}

    # --------------------------------------------------------------------------
    # PART 4: Permit state duplicate resolution

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

    # Error checker #11
    # Thrown when there are NAs in the record_type column after the case_when
    if((TRUE %in% is.na(permit_dupes$record_type)) == TRUE){
      message("Error 11: Some permit duplicates not identified as HIP or PMT.")
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
        # Check records for most recent registration year
        x_reg_yr =
          # Skip frame shift issues and only evaluate for years in correct yyyy
          # format
          ifelse(
            str_detect(registration_yr, "^[0-9]{4}$"),
            ifelse(
              registration_yr ==
                ifelse(
                  !is.na(registration_yr),
                  as.character(max(as.numeric(registration_yr), na.rm = TRUE)),
                  NA),
              n(),
              NA),
            999))%>%
      ungroup()

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
            # the registration year and bag checks above, keep it
            x_reg_yr == 1 & x_bags == 1 ~ "keeper",
            # When there isn't a 1 value in either of the checking columns, it's
            # a duplicate still and we will need to randomly choose which record
            # in the group to keep later
            !(1 %in% x_reg_yr)&!(1 %in% x_bags) ~ "dupl",
            # For rare cases that have two records: two registration years, one
            # record all 1s and the other not all 1s, keep the record with not
            # all 1s
            x_reg_yr == 2 & x_bags == 1 ~ "keeper",
            # Another rare case: two records, one with all 1s and the other not
            # all 1s, BUT the record without all 1s is from the "wrong"
            # registration year (although issue dates are the same for both);
            # keep the record with the not all 1s bag values
            is.na(x_reg_yr) & x_bags == 1 ~ "keeper",
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
      select(-c("other_sum", "x_reg_yr", "x_bags"))

    # Error checker #12
    # Thrown when there are still NAs in the decision column (which means some
    # cases in the data were not addressed in the case_when above)
    if(nrow(hip_dupes %>% filter(is.na(decision))) > 0){
      message("Error 12: Internal data issue. NAs left over after processing.")
      print(
        hip_dupes %>%
          filter(is.na(decision)))}

    # Error checker #13
    # Thrown when the case_when in the script above accidentally set more than
    # one record per hunter as a "keeper"
    if(
      nrow(
        hip_dupes %>%
        filter(decision == "keeper") %>%
        group_by(duplicate, decision) %>%
        filter(n() > 1)) != 0){
      message("Error 13: More than one record marked to keep per hunter.")
      print(
        hip_dupes %>%
          group_by(duplicate, decision) %>%
          filter(n() > 1))}

    # Error checker #14
    # Thrown when the number of unique hunters in the final permit_dupes table
    # are not equal to the initial number of distinct hunters initially
    if(n_distinct(hip_dupes$duplicate) !=
       n_distinct(
         permit_dupes %>%
         filter(record_type == "HIP") %>%
         select(duplicate))
    ){
      message("Error 14: Not all duplicate hunters were handled.")
      print(
        anti_join(
          duplicates %>%
            filter(dl_state %in% permit_states),
          hip_dupes,
          by = c("duplicate"))
      )}

    # Filter permit_dupes to exclude HIP records since they are in their own
    # table now with 1 record per person
    permit_dupes %<>%
      filter(record_type == "PMT")

    # --------------------------------------------------------------------------
    # PART 5: Combine all resolved records into one tibble

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
      # These records should all be HIP, unless they are a solo IAD record
      mutate(
        record_type =
          ifelse(
            dl_state == "IA" &
            as.numeric(dove_bag) > 0 &
              ducks_bag == "0" &
              geese_bag == "0" &
              woodcock_bag == "0" &
              coots_snipe == "0" &
              rails_gallinules == "0" &
              cranes == "0" &
              band_tailed_pigeon == "0" &
              brant == "0" &
              seaducks == "0",
          "IAD",
          "HIP")) %>%
      # Add back in the resolved duplicates
      bind_rows(
        af_dupes %>% select(-duplicate),
        ia_hips %>% select(-duplicate),
        ia_doves %>% select(-duplicate),
        other_dupes %>% select(-duplicate),
        permit_dupes %>% select(-c("duplicate", "other_sum")),
        hip_dupes %>% select(-c("duplicate", "decision"))) %>%
      distinct()

    # Error checker #15
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
          "Error 15: One or more hunters lost in the duplicate resolution",
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
