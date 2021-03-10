#' Plot errors across download cycles
#'
#' Create a plot of errors per download cycle, either by all states in the data set or a specific state, province, or territory.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom tidyr separate
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
#'  \item A specific state, province, or territory represented by one of the following abbreviations:
#'  \itemize{
#'  \item AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY, AS, GU, MP, PR, VI, UM, FM, MH, PW, AA, AE, AP, CM, CZ, NB, PI, TT, ON, QC, NS, NB, MB, BC, PE, SK, AB, NL}
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

errorPlot_dl <-
  function(x, loc = "all"){

    # Plot for all states

    dl_plot <-
      # Suppress warning: "Expected 25 pieces. Missing pieces filled with `NA`
      # in ... rows". We start by splitting errors for plotting purposes; if
      # there are less than the full amount of errors in a row, the warning
      # happens.
      suppressWarnings(
        if(loc == "all") {
          x %>%
            select(errors, dl_cycle) %>%
            # Pull errors apart, delimited by hyphens
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            # Transform errors into a single column
            pivot_longer(1:25, names_to = "name") %>%
            select(-name) %>%
            group_by(dl_cycle) %>%
            summarize(
              errors = sum(!is.na(value)),
              total = n(),
              .groups = "keep") %>%
            ungroup() %>%
            mutate(proportion = errors/total) %>%
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
        }
        else{

          # Plot for specified state

          x %>%
            # Keep data only for specified state
            filter(state == loc) %>%
            select(errors, dl_cycle) %>%
            # Pull errors apart, delimited by hyphens
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            # Transform errors into a single column
            pivot_longer(1:25, names_to = "name") %>%
            select(-name) %>%
            group_by(dl_cycle) %>%
            summarize(
              errors = sum(!is.na(value)),
              total = n(),
              .groups = "keep") %>%
            ungroup() %>%
            mutate(proportion = errors/total) %>%
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
      )

    return(dl_plot)

  }
