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

    dupl_message <-
      paste(
        "There are", duplicate_individuals,
        "hunters with duplicates;", duplicate_total,
        "total duplicated records.", sep = " ")

    dupl_tibble <-
      bind_rows(
        duplicates %>%
          group_by(firstname, lastname, city, state, birth_date, issue_date) %>%
          mutate(
            dupl =
              ifelse(
                n() > 1,
                "issue_date",
                NA)) %>%
          ungroup(),
        duplicates %>%
          group_by(firstname, lastname, city, state, birth_date, dl_date) %>%
          mutate(
            dupl =
              ifelse(
                n() > 1,
                "dl_date",
                NA)) %>%
          ungroup(),
        duplicates %>%
          group_by(firstname, lastname, city, state, birth_date, dl_state) %>%
          mutate(
            dupl =
              ifelse(
                n() > 1,
                "dl_state",
                NA)) %>%
          ungroup()
      ) %>%
      select(hunter_key, dupl) %>%
      group_by(hunter_key) %>%
      mutate(
        duplicate_type =
          paste(dupl, collapse = "-")) %>%
      ungroup() %>%
      mutate(
        duplicate_type =
          ifelse(
            str_detect(duplicate_type, "NA\\-|\\-NA"),
            str_remove_all(duplicate_type, "NA\\-|\\-NA"),
            duplicate_type),
        # Can't pipe '.' so repeat mutate
        duplicate_type =
          ifelse(
            str_detect(duplicate_type, "^NA$"),
            NA,
            duplicate_type)
      ) %>%
      arrange(hunter_key) %>%
      select(hunter_key, duplicate_type) %>%
      distinct()

    dupl_plot <-
      dupl_tibble %>%
      ggplot(aes(x = duplicate_type)) +
      geom_bar(stat = "count") +
      geom_text(
        aes(x = duplicate_type, label = stat(count)),
        stat = "count",
        vjust = -1) +
      labs(
        x = "Duplicate Cause",
        y = "Count",
        title = "Types of duplicates") +
      scale_y_continuous(expand = expansion(mult = c(-0, 0.2))) +
      scale_x_discrete(
        labels =
          c("dl_date" = "DL Date",
            "dl_date-dl_state" = "DL Date\nDL State",
            "dl_state" = "DL State",
            "issue_date" = "Issue Date",
            "issue_date-dl_date" = "Issue Date\nDL Date",
            "issue_date-dl_date-dl_state" = "Issue Date\nDL Date\nDL State",
            "issue_date-dl_state" = "Issue Date\nDL State",
            "NA" = "Other")
      ) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

    dupl_summary <-
      suppressMessages(
        dupl_tibble %>%
          group_by(duplicate_type) %>%
          summarize(count = n()) %>%
          ungroup())

    dupl_list <-
      list(
        dupl_message,
        dupl_summary,
        dupl_plot)

    return(dupl_list)

  }
