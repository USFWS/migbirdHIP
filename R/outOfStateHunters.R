#' Find out-of-state hunters
#'
#' Create a tibble and plot of hunters who have reported hunting migratory waterfowl outside of the state associated with their address.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import ggplot2
#' @importFrom stats reorder
#'
#' @param x A proofed data table created by \code{\link{proof}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

outOfStateHunters <-
  function(x){

    out_of_staters <-
      suppressWarnings(
        suppressMessages(
          x %>%
            select(state, dl_state) %>%
            filter(state != dl_state) %>%
            group_by(dl_state) %>%
            summarize(out_of_state = n()) %>%
            ungroup()
        )
      )

    oos_plot <-
      out_of_staters %>%
      ggplot(aes(x = reorder(dl_state, out_of_state))) +
      geom_bar(aes(y = out_of_state,
                   x = reorder(dl_state, out_of_state)),
               stat = "identity") +
      geom_text(
        aes(
          y = out_of_state,
          x = reorder(dl_state, out_of_state),
          label = out_of_state,
          angle = 90),
        vjust = 0.2,
        hjust = -0.2) +
      labs(x = "State",
           y = "Out-of-state hunters (count)",
           title = "Hunters with addresses not in the download state") +
      scale_y_continuous(expand = expansion(mult = c(-0, 0.2))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

    print(oos_plot)

    return(out_of_staters)
  }
