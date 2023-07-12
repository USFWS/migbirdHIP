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
            x = "Download cycle",
            y = "Error proportion",
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
            x = "Download cycle",
            y = "Error proportion",
            title = paste0("Errors per download cycle in ", loc)) +
          scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
      }

      return(dl_plot)
    }

  }

#' Plot HIP errors by field
#'
#' Create a bar plot of proportion of error per field. The plot defaults to all 49 states, but location can be specified.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_extract
#' @importFrom dplyr select
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr bind_rows
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 scale_fill_discrete
#' @importFrom ggplot2 expansion
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param loc The location that errors should be plotted for. Acceptable values include:
#'  \itemize{
#'  \item all - All states, provinces, and/or territories that exist in the data
#'  \item A specific state represented by one of the following abbreviations:
#'  \itemize{
#'  \item AL, AK, AZ, AR, CA, CO, CT, DE, DC, FL, GA, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY}
#'  }
#' @param year The year in which the HIP data were collected.
#' @param youth Optional. If set to TRUE, will plot youth hunters in addition to normal birth date errors.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

errorPlot_fields <-
  function(x, loc = "all", year, youth = FALSE){
    if(!loc %in% c(unique(x$dl_state), "all")) {

      message(
        paste0(
          "Error! The value you supplied to the `loc` parameter is incorrect.",
          " Please supply a two-letter state abbreviation for the `loc`",
          " parameter, or `loc = 'all'` to plot all available states. States",
          " that submitted data for this download include:\n",
          paste(unique(x$dl_state), collapse = ", "))
      )
    } else {
      if (loc != "all") {
        x <-
          x |>
          filter(dl_state == loc)
      }
      if (youth == TRUE) {

        # Plot *with* special legend colors

        # Step 1: table of errors
        table_1 <-
          x |>
          mutate(
            special =
              ifelse(
                str_extract(birth_date, "(?<=\\/)[0-9]{4}$") > year - 16,
                "Youth Hunter",
                NA)) |>
          select(errors, special) |>
          # Pull errors apart, delimited by hyphens
          separate_wider_delim(
            errors, delim = "-", names_sep = "_", too_few = "align_start") |>
          # Transform errors into a single column
          pivot_longer(starts_with("errors"), names_to = "name") |>
          filter(!is.na(value)) |>
          select(errors = value, special)

        # Step 2: table of errors with proportions calculated -- the youth
        # errors must be row bound in
        table_2 <-
          table_1 |>
          group_by(errors) |>
          # Count number of correct and incorrect values
          summarize(count_errors = sum(!is.na(errors))) |>
          ungroup() |>
          filter(errors != "birth_date") |>
          bind_rows(
            table_1 |>
              group_by(errors, special) |>
              summarize(
                count_errors = sum(!is.na(errors)), .groups = "drop") |>
              filter(errors == "birth_date")) |>
          # Calculate error proportion
          mutate(
            total = nrow(x),
            proportion = count_errors / nrow(x))

        # Labels for bar plot (birth_date color stack doesn't cooperate with
        # positioning 2 labels, so we only label that field once at the top of
        # the bar)
        barlabels <-
          table_2 |>
          select(-c("special", "total")) |>
          group_by(errors) |>
          mutate(
            count_errors = sum(count_errors),
            proportion = sum(proportion)) |>
          ungroup()

        # Plot
        fields_plot <-
          table_2 |>
          ggplot() +
          geom_bar(
            aes(
              x = reorder(errors, proportion),
              y = proportion,
              fill = special),
            stat = "identity") +
          geom_text(
            aes(x = barlabels$errors,
                y = barlabels$proportion,
                label = barlabels$count_errors,
                angle = 90),
            vjust = 0.2, hjust = -0.2) +
          labs(
            x = "Field",
            y = "Error proportion",
            fill = "Specifics") +
          scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
          theme_classic() +
          theme(
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
          scale_fill_discrete(
            labels = "Youth Hunter", breaks = "Youth Hunter")

      } else {

        # Plot all states without special legend colors

        fields_plot <-
          x |>
          select(errors) |>
          # Pull errors apart, delimited by hyphens
          separate_wider_delim(
            errors, delim = "-", names_sep = "_", too_few = "align_start") |>
          # Transform errors into a single column
          pivot_longer(starts_with("errors"), names_to = "name") |>
          select(errors = value) |>
          filter(!is.na(errors)) |>
          group_by(errors) |>
          # Count number of correct values
          summarize(count_errors = sum(!is.na(errors))) |>
          ungroup() |>
          # Calculate error proportion
          mutate(
            total = nrow(x),
            proportion = count_errors / nrow(x)) |>
          # Plot
          ggplot() +
          geom_bar(
            aes(x = reorder(errors, proportion), y = proportion),
            stat = "identity") +
          geom_text(
            aes(x = errors, y = proportion, label = count_errors, angle = 90),
            vjust = 0.2, hjust = -0.2) +
          labs(
            x = "Field",
            y = "Error proportion") +
          scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

      }
      if (loc == "all") {
        return(
          fields_plot +
            labs(title = "Error proportion per field"))
      } else {
        return(
          fields_plot +
            labs(title = paste0("Error proportion per field in ", loc)))
      }
    }
  }

#' Plot errors by state
#'
#' Create a bar plot of errors by state, either by count or proportion.
#'
#' @importFrom dplyr select
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom stats reorder
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 expansion
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 element_text
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param threshold Optional. A decimal value above which error proportions should be plotted.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

errorPlot_states <-
  function(x, threshold = NA) {

    # Generate a table of error proportions
    state_tbl <-
      x |>
      select(errors, dl_state) |>
      # Pull errors apart, delimited by hyphens
      separate_wider_delim(
        errors, delim = "-", names_sep = "_", too_few = "align_start") |>
      # Transform errors into a single column
      pivot_longer(starts_with("errors"), names_to = "name") |>
      select(-name) |>
      rename(errors = value) |>
      filter(!is.na(dl_state)) |>
      group_by(dl_state) |>
      # Count number of correct and incorrect values
      summarize(
        count_errors = sum(!is.na(errors)),
        count_correct = sum(is.na(errors))) |>
      ungroup() |>
      # Calculate the proportion
      mutate(proportion = count_errors/(count_errors + count_correct))

    if(is.na(threshold)) {

      # Proportion plot: no threshold specified
      state_plot <-
        state_tbl |>
        # Plot
        ggplot() +
        geom_bar(
          aes(y = proportion, x = reorder(dl_state, proportion)),
          stat = "identity") +
        geom_text(
          aes(
            y = proportion,
            x = reorder(dl_state, proportion),
            label = count_errors,
            angle = 90),
          vjust = 0.2, hjust = -0.2) +
        labs(
          x = "State",
          y = "Error proportion",
          title = "Error proportion by state") +
        scale_y_continuous(expand = expansion(mult = c(-0, 0.3))) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

    } else {

      # Proportion plot: threshold specified
      state_tbl <-
        state_tbl |>
        # Keep only the states with more than specified error percentage
        filter(proportion >= threshold)

      if(nrow(state_tbl) == 0) {

        # If the threshold was set too high, return a message that says so
        message(
          paste0(
            "Error! Threshold too great; no data to plot. Reduce threshold ",
            "value.")
        )

      } else {

        # If the threshold wasn't set too high, make a plot
        state_plot <-
          state_tbl |>
          ggplot() +
          geom_bar(aes(y = proportion, x = reorder(dl_state, proportion)),
                   stat = "identity") +
          geom_text(
            aes(
              y = proportion,
              x = reorder(dl_state, proportion),
              label = count_errors,
              angle = 90),
            vjust = 0.2, hjust = -0.2) +
          labs(
            x = "State",
            y = "Error proportion",
            title =
              paste0(
                "Error proportion by state (> ",
                as.character(threshold),
                ")")) +
          scale_y_continuous(expand = expansion(mult = c(-0, 0.3))) +
          theme_classic() +
          theme(
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

      }
    }

    if(exists("state_plot")) {
      return(state_plot)
    }

  }
