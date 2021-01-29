#' Find duplicates
#'
#' Determine how many duplicate records are in the data. Plot and tabulate which fields are duplicates of individual hunters (i.e. data grouped by first name, last name, city, state, and birth date).
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import ggplot2
#'
#' @param x A proofed data table created by \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

findDuplicates <-
  function(x){

    # Group by name and address

    duplicates <-
      x %>%
      mutate(hunter_key = paste0("hunter_", row_number())) %>%
      group_by(
        firstname,
        lastname,
        city,
        state,
        birth_date) %>%
      mutate(
        duplicate =
          ifelse(
            n() > 1,
            "duplicate",
            "1")) %>%
      ungroup() %>%
      filter(duplicate == "duplicate") %>%
      arrange(
        firstname,
        lastname,
        city,
        state,
        birth_date)

    duplicate_total <-
      duplicates %>%
      nrow()

    duplicate_individuals <-
      duplicates %>%
      select(
        firstname,
        lastname,
        city,
        state,
        birth_date) %>%
      distinct() %>%
      nrow()

    dupl_tibble <-
      duplicates %>%
      select(-c("hunter_key", "duplicate")) %>%
      group_by(firstname, lastname, city, state, birth_date) %>%
      mutate(
        # Hunter key per individual (not per row)
        hunter_key = cur_group_id(),
        # Find the reason for the duplicates
        dupl = "",
        # Iterate over each field in order to paste the field names together
        # (can't be done with case_when)
        dupl =
          ifelse(
            n_distinct(title) > 1,
            paste(dupl, "title", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(middle) > 1,
            paste(dupl, "middle", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(suffix) > 1,
            paste(dupl, "suffix", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(address) > 1,
            paste(dupl, "address", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(zip) > 1,
            paste(dupl, "zip", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(birth_date) > 1,
            paste(dupl, "birth_date", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(issue_date) > 1,
            paste(dupl, "issue_date", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(hunt_mig_birds) > 1,
            paste(dupl, "hunt_mig_birds", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(registration_yr) > 1,
            paste(dupl, "registration_yr", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(email) > 1,
            paste(dupl, "email", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(dl_state) > 1,
            paste(dupl, "dl_state", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(dl_date) > 1,
            paste(dupl, "dl_date", sep = "-"),
            dupl),
        dupl =
          ifelse(
            n_distinct(dl_cycle) > 1,
            paste(dupl, "dl_cycle", sep = "-"),
            dupl),
        dupl = ifelse(str_detect(dupl, "^$"), "bag", dupl),
        dupl = str_remove(dupl, "^\\-")
      ) %>%
      ungroup() %>%
      select(hunter_key, dupl) %>%
      distinct()

    dupl_plot <-
      dupl_tibble %>%
      mutate(
        dupl =
          case_when(
            str_detect(dupl, "[a-z|a-z\\_a-z|a-z|a-z\\_a-z\\_a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "5+ fields",
            str_detect(dupl, "[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "4 fields",
            str_detect(dupl, "[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "3 fields",
            str_detect(dupl, "[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "2 fields",
            TRUE ~ dupl)
      ) %>%
      ggplot(aes(x = dupl)) +
      geom_bar(stat = "count") +
      geom_text(
        aes(
          x = dupl,
          label = stat(count),
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

    dupl_summary <-
      suppressMessages(
        dupl_tibble %>%
          group_by(dupl) %>%
          summarize(count = n()) %>%
          ungroup())

    message(
      paste(
        "There are", duplicate_individuals,
        "hunters with duplicates;", duplicate_total,
        "total duplicated records.", sep = " ")
    )

    print(dupl_plot)

    return(dupl_summary)

  }
