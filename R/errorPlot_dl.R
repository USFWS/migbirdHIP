#' Plot errors across download cycles
#'
#' Create a plot of errors per download cycle, either by all states in the data set or a specific state, province, or territory.
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#'
#' @param x A proofed data table created by \code{\link{proof}} or \code{\link{correct}}
#' @param loc Which location the error data should be plotted for. Acceptable values include:
#'  \itemize{
#'  \item all - All states, provinces, and/or territories that exist in the data
#'  \item A specific state represented by one of the following abbreviations:
#'  \itemize{
#'  \item AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY}
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

errorPlot_dl <-
  function(x, loc = "all") {
    if(!loc %in% c(unique(x$dl_state), "all")) {

      message(
        paste0(
          "Error! The value you supplied to the `loc` parameter is incorrect.",
          " Please supply a two-letter state abbreviation for the `loc`",
          " parameter, or `loc = 'all'` to plot all available states. States",
          " that submitted data for this download include:\n",
          paste(unique(x$dl_state), collapse = ", "))
      )
    }
    else {
      if(loc == "all") {

        # Plot for all states

        dl_plot <-
          x |>
          select(errors, dl_cycle) |>
          # Pull errors apart, delimited by hyphens
          separate_wider_delim(
            errors, delim = "-", names_sep = "_", too_few = "align_start") |>
          # Transform errors into a single column
          pivot_longer(starts_with("errors"), names_to = "name") |>
          select(-name) |>
          group_by(dl_cycle) |>
          summarize(
            errors = sum(!is.na(value)),
            total = n()) |>
          ungroup() |>
          mutate(proportion = errors/total) |>
          # Plot
          ggplot() +
          geom_bar(aes(x = dl_cycle, y = proportion), stat = "identity") +
          geom_text(
            stat = "identity",
            aes(x = dl_cycle, y = proportion, label = errors, angle = 90),
            vjust = 0.2, hjust = -0.2) +
          labs(
            x = "Download Cycle",
            y = "Error Proportion",
            title = "Errors per download cycle") +
          scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

      } else {

        # Plot for specified state

        dl_plot <-
          x |>
          # Keep data only for specified state
          filter(dl_state == loc) |>
          select(errors, dl_cycle) |>
          # Pull errors apart, delimited by hyphens
          separate_wider_delim(
            errors, delim = "-", names_sep = "_", too_few = "align_start") |>
          # Transform errors into a single column
          pivot_longer(starts_with("errors"), names_to = "name") |>
          select(-name) |>
          group_by(dl_cycle) |>
          summarize(
            errors = sum(!is.na(value)),
            total = n()) |>
          ungroup() |>
          mutate(proportion = errors/total) |>
          # Plot
          ggplot() +
          geom_bar(aes(x = dl_cycle, y = proportion), stat = "identity") +
          geom_text(
            stat = "identity",
            aes(x = dl_cycle, y = proportion, label = errors, angle = 90),
            vjust = 0.2, hjust = -0.2) +
          labs(
            x = "Download Cycle",
            y = "Error Proportion",
            title = paste0("Errors per download cycle in ", loc)) +
          scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      }

      return(dl_plot)
    }

  }
