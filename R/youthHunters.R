#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import ggplot2

youthHunters <-
  # x = proofed data tibble, year = current HIP year
  function(x, year){

    total_hunters <-
      suppressMessages(
        x %>%
          group_by(dl_state) %>%
          summarize(total_registered = n()) %>%
          ungroup()
      )

    youth_hunters <-
      suppressMessages(
        x %>%
          select(dl_state, birth_date) %>%
          mutate(
            birth_year = str_extract(birth_date, "(?<=\\/)[0-9]{4}$")) %>%
          filter(birth_date > year - 16) %>%
          group_by(dl_state) %>%
          summarize(registered_youth = n()) %>%
          ungroup() %>%
          left_join(total_hunters, by = "dl_state") %>%
          mutate(
            youth_proportion = registered_youth/total_registered)
      )

    youth_plot <-
      youth_hunters %>%
      mutate(youth_proportion = round(youth_proportion, digits = 2)) %>%
      ggplot(aes(x = reorder(dl_state, youth_proportion))) +
      geom_bar(aes(y = youth_proportion,
                   x = reorder(dl_state, youth_proportion)),
               stat = "identity") +
      geom_text(
        aes(
          y = youth_proportion,
          x = reorder(dl_state, youth_proportion),
          label = youth_proportion,
          angle = 90),
        vjust = 0.2,
        hjust = -0.2) +
      labs(x = "State",
           y = "Youth hunters (proportion)",
           title = "Youth hunters (< 16 years of age)") +
      scale_y_continuous(expand = expansion(mult = c(-0, 0.2))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

    youth_list <-
      list(
        youth_hunters,
        youth_plot)

    return(youth_list)

  }
