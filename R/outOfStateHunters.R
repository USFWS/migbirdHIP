#' Find out-of-state hunters
#'
#' Create a tibble and plot of hunters who have registered outside of their home state.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr ungroup
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom stats reorder
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
#'
#' @param x A proofed data table created by \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

outOfStateHunters <-
  function(x){

    # Tibble of hunters from out-of-state

    out_of_staters <-
      suppressWarnings(
        # Suppress summarize groups message
        suppressMessages(
          x %>%
            select(state, dl_state) %>%
            # Filter to out-of-staters
            filter(state != dl_state) %>%
            # Count
            group_by(dl_state) %>%
            summarize(out_of_state = n()) %>%
            ungroup()
        )
      )

    # Proportion calculation

    oos_proportion <-
      suppressWarnings(
        # Suppress summarize .groups message
        suppressMessages(
          x %>%
            select(dl_state) %>%
            group_by(dl_state) %>%
            summarize(n_total = n()) %>%
            ungroup() %>%
            left_join(out_of_staters, by = "dl_state") %>%
            mutate(
              outofstate_prop = out_of_state/n_total,
              outofstate_prop = round(outofstate_prop, digits = 2)
            ) %>%
            select(-out_of_state) %>%
            filter(!is.na(outofstate_prop))
          )
        )

    # Out of state hunter proportion plot

    oos_plot <-
      oos_proportion %>%
      ggplot(aes(x = reorder(dl_state, outofstate_prop))) +
      geom_bar(
        aes(
          y = outofstate_prop,
          x = reorder(dl_state, outofstate_prop)),
        stat = "identity") +
      geom_text(
        aes(
          y = outofstate_prop,
          x = reorder(dl_state, outofstate_prop),
          label = n_total,
          angle = 90),
        vjust = 0.2,
        hjust = -0.2) +
      labs(
        x = "State",
        y = "Out-of-state hunters (proportion)",
        title = "Hunters with addresses not in the download state") +
      scale_y_continuous(expand = expansion(mult = c(-0, 0.2))) +
      theme_classic() +
      theme(
        axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
      print(oos_plot)

    return(oos_proportion)

  }
