#' Plot errors by state
#'
#' Create a bar plot of errors by state, either by count or proportion.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import ggplot2
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param type Type of plot to create. Acceptable values include:
#'  \itemize{
#'  \item proportion - Number of errors divided by the number of records per state
#'  \item count - Count of total errors per state
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

errorPlot_states <-
  function(x, type) {

    if(type == "proportion") {

      # Plot proportion of errors by state
      state_plot <-
        suppressWarnings(
          suppressMessages(
            x %>%
              select(errors, dl_state) %>%
              mutate(temp_key = row_number()) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              rename(errors = value) %>%
              filter(!is.na(dl_state)) %>%
              group_by(dl_state) %>%
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
                x = reorder(dl_state, proportion)),
                stat = "identity") +
              geom_text(
                aes(
                  y = proportion,
                  x = reorder(dl_state, proportion),
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
              select(errors, dl_state) %>%
              mutate(temp_key = row_number()) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              rename(errors = value) %>%
              filter(!is.na(dl_state)) %>%
              group_by(dl_state) %>%
              summarize(
                count_errors = sum(!is.na(errors)),
                count_correct = sum(is.na(errors))) %>%
              ungroup() %>%
              filter(count_errors > 100) %>%
              ggplot() +
              geom_bar(aes(
                y = count_errors,
                x = reorder(dl_state, count_errors)),
                stat = "identity") +
              geom_text(
                aes(
                  y = count_errors,
                  x = reorder(dl_state, count_errors),
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
