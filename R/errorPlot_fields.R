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
#' @param year The year in which the Harvest Information Program data were collected. This parameter allows youth hunters (< 16 years of age) to be plotted, revealing the remaining birth_date errors for further consideration.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

errorPlot_fields <-
  function(x, loc = "all", year = NA){

    # Plot errors by field name
    fields_plot <-
      suppressWarnings(
        if(!is.na(year)){
          if(loc == "all"){
            x %>%
              select(errors, birth_date) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              filter(!is.na(value)) %>%
              rename(errors = value) %>%
              mutate(
                specifics =
                  # Denote when birth date error is due to youth hunters
                  ifelse(
                    str_detect(errors, "birth_date") &
                      (year - as.numeric(str_extract(birth_date, "[0-9]{4}$"))) < 16,
                    "Youth Hunter",
                    NA)
              ) %>%
              ggplot() +
              geom_bar(aes(x = errors, fill = specifics), stat = "count") +
              geom_text(
                stat = "count",
                aes(x = errors, label = stat(count), angle = 90),
                vjust = 0.2,
                hjust = -0.2
              ) +
              labs(
                x = "Field",
                y = "Error Count",
                title = "Errors per field",
                fill = "Specifics") +
              scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
              theme_classic() +
              theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
          }
          else{
            x %>%
              filter(state == loc) %>%
              select(errors, birth_date) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              filter(!is.na(value)) %>%
              rename(errors = value) %>%
              mutate(
                specifics =
                  # Denote when birth date error is due to youth hunters
                  ifelse(
                    str_detect(errors, "birth_date") &
                      (year - as.numeric(str_extract(birth_date, "[0-9]{4}$"))) < 16,
                    "Youth Hunter",
                    NA)
              ) %>%
              ggplot() +
              geom_bar(aes(x = errors, fill = specifics), stat = "count") +
              geom_text(
                stat = "count",
                aes(x = errors, label = stat(count), angle = 90),
                vjust = 0.2,
                hjust = -0.2
              ) +
              labs(
                x = "Field",
                y = "Error Count",
                title = paste0("Errors per field", " in ", loc),
                fill = "Specifics") +
              scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
              theme_classic() +
              theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
          }
        }
        else{
          if(loc == "all"){
            x %>%
              select(errors) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              filter(!is.na(value)) %>%
              rename(errors = value) %>%
              ggplot() +
              geom_bar(aes(x = errors), stat = "count") +
              geom_text(
                stat = "count",
                aes(x = errors, label = stat(count), angle = 90),
                vjust = 0.2,
                hjust = -0.2
              ) +
              labs(
                x = "Field",
                y = "Error Count",
                title = "Errors per field") +
              scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
              theme_classic() +
              theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
          }
          else{
            x %>%
              filter(state == loc) %>%
              select(errors) %>%
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              pivot_longer(1:25, names_to = "name") %>%
              select(-name) %>%
              filter(!is.na(value)) %>%
              rename(errors = value) %>%
              ggplot() +
              geom_bar(aes(x = errors), stat = "count") +
              geom_text(
                stat = "count",
                aes(x = errors, label = stat(count), angle = 90),
                vjust = 0.2,
                hjust = -0.2
              ) +
              labs(
                x = "Field",
                y = "Error Count",
                title = paste0("Errors per field", " in ", loc)) +
              scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
              theme_classic() +
              theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
          }
        }
      )

    return(fields_plot)

  }
