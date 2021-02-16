#' Plot errors by field
#'
#' Create a bar plot of proportion of error per field. The plot defaults to all 49 states, but location can be specified.
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
#' @param specify The year in which the Harvest Information Program data were collected must be supplied to activate this feature. This parameter breaks down *birth_date* errors into color-coded categories to display youth hunters (< 16 years of age) and "regular errors" in which the birth date is simply incorrect.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

errorPlot_fields <-
  function(x, loc = "all", specify = NA){

    # Suppress warning: "Expected 25 pieces. Missing pieces filled with `NA`
    # in ... rows". We start by splitting errors for plotting purposes; if
    # there are less than the full amount of errors in a row, the warning
    # happens.
    suppressWarnings(
      if(!is.na(specify)){
        if(loc == "all"){

          # Plot all states *with* special legend colors

          # Step 1: table of errors
          table_1 <-
            x %>%
            mutate(
              birth_year = str_extract(birth_date, "(?<=\\/)[0-9]{4}$"),
              special =
                ifelse(
                  birth_year > specify - 16,
                  "Youth Hunter",
                  NA)) %>%
            select(errors, special) %>%
            # Pull errors apart, delimited by hyphens
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            # Transform errors into a single column
            pivot_longer(1:25, names_to = "name") %>%
            select(errors = value, special) %>%
            filter(!is.na(errors))

          # Step 2: table of errors with proportions calculated -- the youth
          # errors must be row bound in
          table_2 <-
            table_1 %>%
            group_by(errors) %>%
            # Count number of correct and incorrect values
            summarize(count_errors = sum(!is.na(errors))) %>%
            ungroup() %>%
            filter(errors != "birth_date") %>%
            bind_rows(
              table_1 %>%
                group_by(errors, special) %>%
                summarize(count_errors = sum(!is.na(errors))) %>%
                ungroup() %>%
                filter(errors == "birth_date")) %>%
            # Calculate error proportion
            mutate(
              total = nrow(x),
              proportion = count_errors / nrow(x))

          # Labels for bar plot (birth_date color stack doesn't cooperate with
          # positioning 2 labels, so we only label that field once at the top of
          # the bar)
          barlabels <-
            table_2 %>%
            select(-c("special", "total")) %>%
            group_by(errors) %>%
            mutate(
              count_errors = sum(count_errors),
              proportion = sum(proportion)) %>%
            ungroup()

          # Plot
          fields_plot <-
            table_2 %>%
            ggplot() +
            geom_bar(aes(x = errors, y = proportion, fill = special), stat = "identity") +
            geom_text(
              aes(x = barlabels$errors, y = barlabels$proportion, label = barlabels$count_errors, angle = 90),
              vjust = 0.2, hjust = -0.2) +
            labs(
              x = "Field",
              y = "Error proportion",
              title = "Error proportion per field",
              fill = "Specifics") +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
            scale_fill_discrete(labels = "Youth Hunter", breaks = "Youth Hunter")

          return(fields_plot)
          }
        else{

          # Plot specific state *with* special legend colors

          # Step 1: table of errors
          table_1 <-
            x %>%
            filter(dl_state == loc) %>%
            mutate(
              birth_year = str_extract(birth_date, "(?<=\\/)[0-9]{4}$"),
              special =
                ifelse(
                  birth_year > specify - 16,
                  "Youth Hunter",
                  NA)) %>%
            select(errors, special) %>%
            # Pull errors apart, delimited by hyphens
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            # Transform errors into a single column
            pivot_longer(1:25, names_to = "name") %>%
            select(errors = value, special) %>%
            filter(!is.na(errors))

          # Step 2: table of errors with proportions calculated -- the youth
          # errors must be row bound in
          table_2 <-
            table_1 %>%
            group_by(errors) %>%
            # Count number of correct and incorrect values
            summarize(count_errors = sum(!is.na(errors))) %>%
            ungroup() %>%
            filter(errors != "birth_date") %>%
            bind_rows(
              table_1 %>%
                group_by(errors, special) %>%
                summarize(count_errors = sum(!is.na(errors))) %>%
                ungroup() %>%
                filter(errors == "birth_date")) %>%
            # Calculate error proportion
            mutate(
              total = nrow(x),
              proportion = count_errors / nrow(x))

          # Labels for bar plot (birth_date color stack doesn't cooperate with
          # positioning 2 labels, so we only label that field once at the top of
          # the bar)
          barlabels <-
            table_2 %>%
            select(-c("special", "total")) %>%
            group_by(errors) %>%
            mutate(
              count_errors = sum(count_errors),
              proportion = sum(proportion)) %>%
            ungroup()

          # Plot
          fields_plot <-
            table_2 %>%
            ggplot() +
            geom_bar(aes(x = errors, y = proportion, fill = special), stat = "identity") +
            geom_text(
              aes(x = barlabels$errors, y = barlabels$proportion, label = barlabels$count_errors, angle = 90),
              vjust = 0.2, hjust = -0.2) +
            labs(
              x = "Field",
              y = "Error proportion",
              title = paste0("Error proportion per field in ", loc),
              fill = "Specifics") +
            scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
            theme_classic() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
            scale_fill_discrete(labels = "Youth Hunter", breaks = "Youth Hunter")

          return(fields_plot)
          }
        }
        else{
          if(loc == "all"){

            # Plot all states without special legend colors

            fields_plot <-
              x %>%
              select(errors) %>%
              # Pull errors apart, delimited by hyphens
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              # Transform errors into a single column
              pivot_longer(1:25, names_to = "name") %>%
              select(errors = value) %>%
              filter(!is.na(errors)) %>%
              group_by(errors) %>%
              # Count number of correct values
              summarize(count_errors = sum(!is.na(errors))) %>%
              ungroup() %>%
              # Calculate error proportion
              mutate(
                total = nrow(x),
                proportion = count_errors / nrow(x)) %>%
              # Plot
              ggplot() +
              geom_bar(aes(x = errors, y = proportion), stat = "identity") +
              geom_text(
                aes(x = errors, y = proportion, label = count_errors, angle = 90),
                vjust = 0.2, hjust = -0.2) +
              labs(
                x = "Field",
                y = "Error proportion",
                title = "Error proportion per field") +
              scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
              theme_classic() +
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

            return(fields_plot)
          }
          else{

            # Plot specific state without special legend colors

            fields_plot <-
              x %>%
              filter(state == loc) %>%
              select(errors) %>%
              # Pull errors apart, delimited by hyphens
              separate(errors, into = as.character(c(1:25)), sep = "-") %>%
              # Transform errors into a single column
              pivot_longer(1:25, names_to = "name") %>%
              select(errors = value) %>%
              filter(!is.na(errors)) %>%
              group_by(errors) %>%
              # Count number of correct
              summarize(count_errors = sum(!is.na(errors))) %>%
              ungroup() %>%
              # Calculate error proportion
              mutate(
                total = nrow(x),
                proportion = count_errors / nrow(x)) %>%
              # Plot
              ggplot() +
              geom_bar(aes(x = errors, y = proportion), stat = "identity") +
              geom_text(
                aes(x = errors, y = proportion, label = count_errors, angle = 90),
                vjust = 0.2, hjust = -0.2) +
              labs(
                x = "Field",
                y = "Error proportion",
                title = paste0("Error proportion per field in ", loc)) +
              scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
              theme_classic() +
              theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

            return(fields_plot)
          }
        }
      )
  }
