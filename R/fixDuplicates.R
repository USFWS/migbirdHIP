#' Fix duplicates
#'
#' Consolidate duplicate records that stem from HIP data and special permit
#' information being stored in separate rows. If other duplicates exist not due
#' to this specific reason, move the records to a separate table. Records in the
#' consolidated table will indicate they are combined HIP/Permit records (value
#' "hip-permit") in a new column labeled "record_type". All other original
#' non-duplicated records will be indicated in the "record_type" field as being
#' HIP only (value "hip").
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
#' @importFrom dplyr lead
#' @importFrom dplyr select
#' @importFrom dplyr desc
#' @importFrom stringr str_replace
#'
#' @param x A cleaned data table created by \code{\link{clean}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

fixDuplicates <-
  function(x){

    # Data duplicates
    duplicates <-
      x %>%
      # Group by registrant information; name, city, state, birthday, dl_state
      group_by(
        firstname,
        lastname,
        city,
        state,
        birth_date,
        dl_state) %>%
      # Identify duplicates
      mutate(
        duplicate =
          ifelse(
            n() > 1,
            paste0("duplicate_", cur_group_id()),
            "no")) %>%
      ungroup() %>%
      # Filter out non-duplicate records
      filter(str_detect(duplicate, "duplicate")) %>%
      # Sort tibble
      arrange(
        firstname,
        lastname,
        city,
        state,
        birth_date,
        dl_state)

    # Washington, Oregon, Colorado, and South Dakota permit duplicates
    state_dupes <-
      duplicates %>%
      # Filter the duplicates to those that occur in permit states
      filter(str_detect(dl_state, "WA|OR|CO|SD")) %>%
      # Set "." to 0 in take fields and make them numeric
      mutate_at(
        vars(matches("bag|coots|rails|cranes|pigeon|brant|seaducks")),
        ~str_replace(., "\\.", "0") %>%
          as.numeric(.)
      ) %>%
      # Calculate sums of permit spp and other spp to use as a proxy
      mutate(
        special_sum = brant + seaducks + band_tailed_pigeon + cranes,
        other_sum = rowSums(across(matches("bag|coots|rails")))
      ) %>%
      # Filter out records with horizontal 0s... this happens when hunters submit
      # HIP data (as 1 record) and go back later for a seaduck permit (which adds 2
      # records; one as all 0s and another as the seaduck take)
      filter(special_sum + other_sum != 0)

    # Filter out "duplicates" that only have 1 record per group after the
    # previous filter removed records with horizontal 0s; these are not
    # duplicates and we can keep them as "good records"
    dup_1record <-
      state_dupes %>%
      group_by(duplicate) %>%
      filter(n() == 1) %>%
      ungroup()

    # Pull out the multi-duplicates... not sure what to do with these yet
    dup_3record <-
      state_dupes %>%
      group_by(duplicate) %>%
      filter(n() > 2) %>%
      mutate(n_grp = n()) %>%
      ungroup() %>%
      mutate(record_type = "unsolved") %>%
      # Unselect the sum cols, which we no longer need
      select(-c("special_sum", "other_sum"))

    # Work with the true HIP/Permit duplicates (2 records per group)
    dup_2record <-
      state_dupes %>%
      # Make sure duplicates still have 2 per group after the previous
      # horizontal 0s filter got rid of some records
      group_by(duplicate) %>%
      filter(n() == 2) %>%
      # Filter to the hunters with duplicates that harvested cranes, BTPI, brant
      # or seaducks
      filter(length(unique(special_sum)) > 1) %>%
      # Filter to the hunters with duplicates that also harvested other birds
      filter(length(unique(other_sum)) > 1) %>%
      # Arrange by special_sum
      arrange(duplicate, special_sum) %>%
      ungroup()

    # Handle dup2_record if nrow == 0
    # This bypasses "Error: Can't subset elements that don't exist." when trying
    # to mutate an empty table...
    if(nrow(dup_2record) > 0){
      dup_2record <-
        dup_2record %>%
        group_by(duplicate) %>%
        # Consolidate the tibble If a hunter's record is == 0 for special_sum,
        # paste that record's crane, BTPI, brant & seaducks values into the other
        # record where special_sum > 0
        mutate(
          cranes =
            case_when(
              str_detect(dl_state, "SD|CO") ~
                ifelse(
                  special_sum == 0,
                  lead(cranes),
                  999999),
              TRUE ~ cranes),
          band_tailed_pigeon =
            case_when(
              str_detect(dl_state, "WA|OR") ~
                ifelse(
                  special_sum == 0,
                  lead(band_tailed_pigeon),
                  999999),
              TRUE ~ band_tailed_pigeon),
          brant =
            case_when(
              str_detect(dl_state, "WA|OR") ~
                ifelse(
                  special_sum == 0,
                  lead(brant),
                  999999),
              TRUE ~ brant),
          seaducks =
            case_when(
              str_detect(dl_state, "WA|OR") ~
                ifelse(
                  special_sum == 0,
                  lead(seaducks),
                  999999),
              TRUE ~ seaducks)
        ) %>%
        ungroup() %>%
        # Remove the duplicates
        # Remaining values are consolidated records
        filter(special_sum == 0) %>%
        select(-c("duplicate", "special_sum", "other_sum"))}
    else{
      dup_2record <- dup_2record
    }

    # Remove duplicates and add in the consolidated records
    fixed_x <-
      x %>%
      # Add a field indicating these are original records
      mutate(record_type = "hip") %>%
      filter(!record_key %in% duplicates$record_key) %>%
      # Bind in recovered 1-record "duplicates"
      bind_rows(
        dup_1record %>%
          mutate_at(
            vars(matches("bag|coots|rails|cranes|pigeon|brant|seaducks")),
            as.character) %>%
          mutate(record_type = "hip")) %>%
      # Bind in fixed 2-record duplicates
      bind_rows(
        dup_2record %>%
          mutate_at(
            vars(matches("bag|coots|rails|cranes|pigeon|brant|seaducks")),
            as.character) %>%
          # Add a field indicating these are consolidated records now
          mutate(record_type = "hip-permit")) %>%
      # Unselect the sum cols, which we no longer need
      select(-c("special_sum", "other_sum"))

    # Return a list:
    # 1. the de-duplicated data
    # 2. unsolvable > 2 duplicate table

    return(
      list(
        fixed_duplicates = fixed_x,
        unsolved_duplicates = dup_3record)
      )

  }
