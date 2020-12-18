#' Plot errors by field
#'
#' Create a bar plot of errors per field, either by all states in the data set or a specific state, province, or territory.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#' @import ggplot2
#'
#' @param x A proofed data table created by \code{\link{proof}}
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

errorPlot_fields <-
  function(x, loc = "all"){

    # Plot errors by field name
    fields_plot <-
      suppressWarnings(
        if(loc == "all"){
          x %>%
            select(errors) %>%
            mutate(temp_key = row_number()) %>%
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            pivot_longer(1:25, names_to = "name") %>%
            select(-name) %>%
            rename(errors = value) %>%
            filter(!is.na(errors)) %>%
            ggplot() +
            geom_bar(aes(x = errors), stat = "count") +
            geom_text(
              stat = "count", aes(x = errors, label = stat(count), angle = 90),
              vjust = 0.2,
              hjust = -0.2
            ) +
            labs(x = "Field", y = "Error Count", title = "Errors per field") +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        }
        else{
          x %>%
            filter(state == loc) %>%
            select(errors) %>%
            mutate(temp_key = row_number()) %>%
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            pivot_longer(1:25, names_to = "name") %>%
            select(-name) %>%
            rename(errors = value) %>%
            filter(!is.na(errors)) %>%
            ggplot() +
            geom_bar(aes(x = errors), stat = "count") +
            geom_text(
              stat = "count", aes(x = errors, label = stat(count), angle = 90),
              vjust = 0.2,
              hjust = -0.2
            ) +
            labs(x = "Field", y = "Error Count",
                 title = paste0("Errors per field", " in ", loc)) +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
        }
      )

    return(fields_plot)

  }
