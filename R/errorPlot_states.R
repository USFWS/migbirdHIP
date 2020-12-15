#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import ggplot2

errorPlot_states <-
  # x = proofed data tibble
  # type = "proportion" for proportions plot or "count" for counts
  function(x, type) {

    states_provinces_and_canada <-
      c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI",
        "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN",
        "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
        "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA",
        "WV", "WI", "WY", "AS", "GU", "MP", "PR", "VI", "UM", "FM", "MH", "PW",
        "AA", "AE", "AP", "CM", "CZ", "NB", "PI", "TT", "ON", "QC", "NS", "NB",
        "MB", "BC", "PE", "SK", "AB", "NL")

    if(type == "proportion") {

      # Plot proportion of errors by state
      state_plot <-
        suppressWarnings(
          suppressMessages(
            x %>%
              select(errors, state) %>%
              mutate(temp_key = row_number()) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              rename(errors = value) %>%
              filter(state %in% states_provinces_and_canada) %>%
              group_by(state) %>%
              summarize(
                count_errors = sum(!is.na(errors)),
                count_correct = sum(is.na(errors))) %>%
              ungroup() %>%
              mutate(
                proportion = count_errors / (count_errors + count_correct)) %>%
              filter(count_errors > 100) %>%
              ggplot() +
              geom_bar(aes(
                y = proportion,
                x = reorder(state, proportion)),
                stat = "identity") +
              geom_text(
                aes(
                  y = proportion,
                  x = reorder(state, proportion),
                  label = round(proportion, digits = 2),
                  angle = 90),
                vjust = 0.2,
                hjust = -0.2) +
              labs(
                x = "State",
                y = "Incorrect Fields (proportion)",
                title = "Proportion of errors in states with > 100 incorrect records") +
              scale_y_continuous(expand = expansion(mult = c(-0, 0.2))) +
              theme_classic() +
              theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
          )
        )
      return(state_plot)
    }

    else if(type == "count"){
      # Plot count of errors by state
      state_plot <-
        suppressWarnings(
          suppressMessages(
            x %>%
              select(errors, state) %>%
              mutate(temp_key = row_number()) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              rename(errors = value) %>%
              filter(state %in% states_provinces_and_canada) %>%
              group_by(state) %>%
              summarize(
                count_errors = sum(!is.na(errors)),
                count_correct = sum(is.na(errors))) %>%
              ungroup() %>%
              filter(count_errors > 100) %>%
              ggplot() +
              geom_bar(aes(
                y = count_errors,
                x = reorder(state, count_errors)),
                stat = "identity") +
              geom_text(
                aes(
                  y = count_errors,
                  x = reorder(state, count_errors),
                  label = count_errors,
                  angle = 90),
                vjust = 0.2,
                hjust = -0.2) +
              labs(
                x = "State",
                y = "Incorrect Fields (count)",
                title = "Count of errors in states with > 100 incorrect records") +
              scale_y_continuous(expand = expansion(mult = c(-0, 0.2))) +
              theme_classic() +
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
          )
        )
      return(state_plot)
    }
    else{
      message("Error: Invalid plot type supplied.")
    }

  }
