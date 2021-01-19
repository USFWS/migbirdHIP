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
#' @param type Type of plot to create
#' Acceptable values include:
#'  \itemize{
#'  \item proportion - Number of out-of-state hunters divided by the total hunters per state
#'  \item count - Count of out-of-state hunters per state
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

outOfStateHunters <-
  function(x, type){
    if (type == "count") {

      out_of_staters <-
        suppressWarnings(suppressMessages(
          x %>%
            select(state, dl_state) %>%
            filter(state != dl_state) %>%
            group_by(dl_state) %>%
            summarize(out_of_state = n()) %>%
            ungroup()
        ))

      oos_plot <-
        out_of_staters %>%
        ggplot(aes(x = reorder(dl_state, out_of_state))) +
        geom_bar(
          aes(
            y = out_of_state,
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
        labs(
          x = "State",
          y = "Out-of-state hunters (count)",
          title = "Hunters with addresses not in the download state") +
        scale_y_continuous(expand = expansion(mult = c(-0, 0.2))) +
        theme_classic() +
        theme(
          axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))

      print(oos_plot)
      return(out_of_staters)
    }
    else if (type == "proportion") {

      out_of_staters <-
        suppressWarnings(suppressMessages(
          x %>%
            select(state, dl_state) %>%
            filter(state != dl_state) %>%
            group_by(dl_state) %>%
            summarize(out_of_state = n()) %>%
            ungroup()
        ))

      oos_proportion <-
        suppressWarnings(suppressMessages(
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
        ))

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
            label = outofstate_prop,
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
    else{
      message("Error: Invalid plot type supplied.")
    }
  }
