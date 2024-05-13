#' Find out-of-state hunters
#'
#' Create a tibble or plot of hunters who have registered outside of their home state.
#'
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
#' @param return Return a "plot" (default) or "table"
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

outOfStateHunters <-
  function(x, return = "plot") {

    # Fail if incorrect return supplied
    stopifnot("Error: Incorrect value supplied for `return` parameter. Please choose: 'plot' or 'table'." = return %in% c("plot", "table"))

    # Tibble of hunters from out-of-state

    out_of_staters <-
      suppressWarnings(
        # Suppress summarize groups message
        suppressMessages(
          x |>
            select(state, dl_state) |>
            # Filter to out-of-staters
            filter(state != dl_state) |>
            # Count
            group_by(dl_state) |>
            summarize(out_of_state = n()) |>
            ungroup()
        )
      )

    # Proportion calculation

    oos_proportion <-
      suppressWarnings(
        # Suppress summarize .groups message
        suppressMessages(
          x |>
            select(dl_state) |>
            group_by(dl_state) |>
            summarize(n_total = n()) |>
            ungroup() |>
            left_join(out_of_staters, by = "dl_state") |>
            mutate(
              outofstate_prop = out_of_state/n_total,
              outofstate_prop = round(outofstate_prop, digits = 2)
            ) |>
            select(-out_of_state) |>
            filter(!is.na(outofstate_prop))
        )
      )

    if(return == "plot") {

      # Out of state hunter proportion plot

      oos_plot <-
        oos_proportion |>
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

    } else if(return == "table") {
      return(oos_proportion)
    }
  }

#' Youth hunters
#'
#' Create a plot of youth hunters (hunters born < 16 years ago). Bar labels are counts.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom stringr str_extract
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
#' @importFrom stats reorder
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param year The year in which the Harvest Information Program data were collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

youthHunters <-
  function(x, year){

    # Fail if incorrect year supplied
    stopifnot("Error: `year` parameter must be numeric." = is.numeric(year))
    stopifnot("Error: Incorrect value supplied for `year` parameter. Please use a 4-digit year in the 2020s, e.g. 2024." = str_detect(year, "^202[0-9]{1}$"))

    # Make a table of total number of hunters per state
    total_hunters <-
      # Suppress group message from summarize function
      suppressMessages(
        x |>
          group_by(dl_state) |>
          summarize(total_registered = n()) |>
          ungroup())

    # Count number and calculate proportion of youth hunters by state
    # Suppress group message from summarize function
    suppressMessages(
      x |>
        select(dl_state, birth_date) |>
        mutate(birth_year = str_extract(birth_date, "(?<=\\/)[0-9]{4}$")) |>
        filter(birth_year > year - 16) |>
        group_by(dl_state) |>
        summarize(registered_youth = n()) |>
        ungroup() |>
        # Join in previous tibble of total hunters per state for proportion
        # calculation
        left_join(total_hunters, by = "dl_state") |>
        filter(!is.na(dl_state)) |>
        # Calculate proportion
        mutate(youth_proportion = registered_youth/total_registered) |>
        filter(!is.na(dl_state)) |>
        # Plot
        ggplot() +
        geom_bar(
          aes(
            x = reorder(dl_state, youth_proportion),
            y = youth_proportion),
          stat = "identity") +
        geom_text(
          aes(
            y = youth_proportion,
            x = reorder(dl_state, youth_proportion),
            label = registered_youth,
            angle = 90),
          vjust = 0.2,
          hjust = -0.2) +
        labs(
          x = "State",
          y = "Youth hunters (proportion)",
          title = "Youth hunters (< 16 years of age)") +
        scale_y_continuous(expand = expansion(mult = c(-0, 0.2))) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    )

  }
