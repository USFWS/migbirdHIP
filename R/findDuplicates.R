#' Find duplicates
#'
#' Determine how many duplicate records are in the data. Plot and tabulate which fields are duplicates of individual hunters (i.e. data grouped by first name, last name, city, state, and birth date).
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr cur_group_id
#' @importFrom stringr str_detect
#' @importFrom dplyr case_when
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 stat
#' @importFrom dplyr count
#' @importFrom dplyr summarize
#'
#' @param x A cleaned data table created by \code{\link{clean}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

findDuplicates <-
  function(x){

    duplicates <-
      x %>%
      # Create a row key
      mutate(hunter_key = paste0("hunter_", row_number())) %>%
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
            "duplicate",
            "1")) %>%
      ungroup() %>%
      # Filter out non-duplicate records
      filter(duplicate == "duplicate") %>%
      # Sort tibble
      arrange(
        firstname,
        lastname,
        city,
        state,
        birth_date,
        dl_state)

    # Total number of records that are a duplicate
    duplicate_total <-
      duplicates %>%
      nrow()

    # Number of registrations that are duplicated
    duplicate_individuals <-
      duplicates %>%
      select(
        firstname,
        lastname,
        city,
        state,
        birth_date,
        dl_state) %>%
      distinct() %>%
      nrow()

    # Determine which fields are different between the duplicates so we can try
    # to figure out why hunters are in the data more than once
    dupl_tibble <-
      duplicates %>%
      select(-c("hunter_key", "duplicate")) %>%
      group_by(firstname, lastname, city, state, birth_date, dl_state) %>%
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
      ) %>%
      ungroup() %>%
      select(hunter_key, dupl) %>%
      distinct()

    if(nrow(dupl_tibble) == 0){

      message(
        paste(
          "There are", duplicate_individuals,
          "registrations with duplicates;", duplicate_total,
          "total duplicated records.", sep = " ")
      )

    }

    else{

      dupl_plot <-
        dupl_tibble %>%
        # Bin into generic "2+ fields" if more than one field contributes to a
        # duplicate
        mutate(
          dupl =
            case_when(
              str_detect(dupl, "[a-z|a-z\\_a-z|a-z|a-z\\_a-z\\_a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "2+ fields", #5+ fields
              str_detect(dupl, "[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "2+ fields", #4 fields
              str_detect(dupl, "[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "2+ fields", #3 fields
              str_detect(dupl, "[a-z|a-z\\_a-z]{1,}\\-[a-z|a-z\\_a-z]{1,}") ~ "2+ fields",
              TRUE ~ dupl)
        ) %>%
        # Make a new col to reorder the bars
        group_by(dupl) %>%
        mutate(total_count = n()) %>%
        ungroup() %>%
        ggplot(aes(x = reorder(dupl, -total_count))) +
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
          "registrations with duplicates;", duplicate_total,
          "total duplicated records.", sep = " ")
      )

      print(dupl_plot)

      return(dupl_summary)
    }

  }
