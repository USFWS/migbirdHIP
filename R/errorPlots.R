#' Plot errors across download cycles
#'
#' Create a plot of errors per download cycle, either by all states in the data
#' set or a specific state, province, or territory.
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr starts_with
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
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}} or \code{\link{correct}}
#' @param loc Which location the error data should be plotted for. Acceptable
#'   values include:
#'  \itemize{
#'  \item "all" - all states
#'  \item a two-letter abbreviation for a US state (excluding HI)
#'  }
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

errorPlotDL <-
  function(proofed_data, loc = "all") {
    failProofed(proofed_data)

    # Fail if incorrect loc supplied
    stopifnot("Error: Incorrect value supplied for `loc` parameter. Please supply a two-letter state abbreviation of a `dl_state` value contained within the data, or 'all'." = loc %in% c(unique(proofed_data$dl_state), "all"))

    if(loc == "all") {
      # Plot for all states
      dl_plot <-
        proofed_data |>
        select(c("errors", "dl_cycle")) |>
        # Pull errors apart, delimited by hyphens
        separate_wider_delim(
          .data$errors,
          delim = "-", names_sep = "_", too_few = "align_start") |>
        # Transform errors into a single column
        pivot_longer(starts_with("errors"), names_to = "name") |>
        select(-"name") |>
        summarize(
          errors = sum(!is.na(.data$value)),
          total = n(),
          .by = "dl_cycle") |>
        mutate(proportion = .data$errors/.data$total) |>
        # Plot
        ggplot() +
        geom_bar(
          aes(x = .data$dl_cycle, y = .data$proportion), stat = "identity") +
        geom_text(
          stat = "identity",
          aes(x = .data$dl_cycle, y = .data$proportion, label = .data$errors,
              angle = 90),
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
          proofed_data |>
          # Keep data only for specified state
          filter(.data$dl_state == loc) |>
          select(c("errors", "dl_cycle")) |>
          # Pull errors apart, delimited by hyphens
            separate_wider_delim(
              .data$errors,
              delim = "-", names_sep = "_", too_few = "align_start") |>
            # Transform errors into a single column
            pivot_longer(starts_with("errors"), names_to = "name") |>
            select(-"name") |>
            summarize(
              errors = sum(!is.na(.data$value)),
              total = n(),
              by = "dl_cycle") |>
            mutate(proportion = .data$errors/.data$total) |>
            # Plot
            ggplot() +
            geom_bar(aes(x = .data$dl_cycle, y = .data$proportion),
                     stat = "identity") +
            geom_text(
              stat = "identity",
              aes(x = .data$dl_cycle, y = .data$proportion,
                  label = .data$errors, angle = 90),
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

#' Plot HIP errors by field
#'
#' Create a bar plot of proportion of error per field. The plot defaults to all
#' 49 states, but location can be specified.
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 expansion
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}} or \code{\link{correct}}
#' @param loc The location that errors should be plotted for. Acceptable values
#'   include:
#'  \itemize{
#'  \item "all" - all states
#'  \item a two-letter abbreviation for a US state (excluding HI)
#'  }
#' @param year The year in which the HIP data were collected.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

errorPlotFields <-
  function(proofed_data, loc = "all", year) {

    # Fail if incorrect loc supplied
    stopifnot("Error: Incorrect value supplied for `loc` parameter. Please supply a two-letter state abbreviation of a `dl_state` value contained within the data, or 'all'." = loc %in% c(unique(proofed_data$dl_state), "all"))

    failYear(year)
    failProofed(proofed_data)

    if (loc != "all") {
      proofed_data <-
        proofed_data |>
        filter(.data$dl_state == loc)
    }

    # Plot all states without special legend colors
    fields_plot <-
      errorLevelErrorsByField(proofed_data) |>
      # Plot
      ggplot() +
      geom_bar(
        aes(x = reorder(.data$errors, .data$proportion), y = .data$proportion),
        stat = "identity") +
      geom_text(
        aes(x = .data$errors, y = .data$proportion, label = .data$count_errors,
            angle = 90),
        vjust = 0.2, hjust = -0.2) +
      labs(
        x = "Field",
        y = "Error proportion") +
      scale_y_continuous(expand = expansion(mult = c(-0, 0.25))) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

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

#' Plot errors by state
#'
#' Create a bar plot of errors by state, either by count or proportion.
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
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
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}} or \code{\link{correct}}
#' @param threshold Optional. A decimal value above which error proportions
#'   should be plotted.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

errorPlotStates <-
  function(proofed_data, threshold = NA) {

    failProofed(proofed_data)

    # Fail if incorrect threshold supplied
    stopifnot("Error: `threshold` parameter must be numeric." = (is.numeric(threshold) | is.na(threshold)))
    stopifnot("Error: Please supply a value between 0 and 1 for the `threshold` parameter." = ((0 <= threshold & threshold <= 1) | is.na(threshold)))

    # Generate a table of error proportions
    state_tbl <- errorLevelErrorsByState(proofed_data)

    if(is.na(threshold)) {

      # Proportion plot: no threshold specified
      state_plot <-
        state_tbl |>
        # Plot
        ggplot() +
        geom_bar(
          aes(
            y = .data$proportion,
            x = reorder(.data$dl_state, .data$proportion)),
          stat = "identity") +
        geom_text(
          aes(
            y = .data$proportion,
            x = reorder(.data$dl_state, .data$proportion),
            label = .data$count_errors,
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
        filter(.data$proportion >= threshold)

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
          geom_bar(
            aes(
              y = .data$proportion,
              x = reorder(.data$dl_state, .data$proportion)
            ),
          stat = "identity") +
          geom_text(
            aes(
              y = .data$proportion,
              x = reorder(.data$dl_state, .data$proportion),
              label = .data$count_errors,
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

#' Calculate error-level errors by state
#'
#' The internal \code{errorLevelErrorsByState} function calculates a summary
#' table of the count of errors, count of correct values, and proportion of
#' erroneous values by state.
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr starts_with
#' @importFrom dplyr filter
#' @importFrom dplyr reframe
#' @importFrom dplyr n
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}} or \code{\link{correct}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

errorLevelErrorsByState <-
  function(proofed_data){
    failProofed(proofed_data)

    proofed_data |>
      select(c("errors", "dl_state")) |>
      mutate(total_records = n(), .by = "dl_state") |>
      separate_wider_delim(
        .data$errors, delim = "-", names_sep = "_", too_few = "align_start") |>
      # Transform errors into a single column
      pivot_longer(starts_with("errors"), names_to = "name") |>
      filter(!is.na(.data$value)) |>
      select(c("dl_state", "total_records", errors = "value")) |>
      reframe(
        count_errors = n(),
        count_correct = (.data$total_records*14) - .data$count_errors,
        proportion = .data$count_errors/(.data$total_records*14),
        .by = "dl_state") |>
      distinct()
  }

#' Calculate error-level errors by field
#'
#' The internal \code{errorLevelErrorsByField} function calculates a summary
#' table of the count of errors and proportion of erroneous values by field.
#'
#' @importFrom dplyr select
#' @importFrom tidyr separate_longer_delim
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @param proofed_data The object created after error flagging data with
#'   \code{\link{proof}} or \code{\link{correct}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

errorLevelErrorsByField <-
  function(proofed_data){
    failProofed(proofed_data)

    proofed_data |>
      select("errors") |>
      # Pull errors apart, delimited by hyphens
      separate_longer_delim(.data$errors, delim = "-") |>
      filter(!is.na(.data$errors)) |>
      # Count number of correct values
      summarize(
        count_errors = sum(!is.na(.data$errors)),
        .by = "errors") |>
      # Calculate error proportion
      mutate(
        total = nrow(proofed_data),
        proportion = .data$count_errors/nrow(proofed_data))
  }

#' Pull bad data
#'
#' Create a tibble of error data by state or field. Data are reported using a
#' threshold of proportion of error.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr select
#' @importFrom dplyr relocate
#' @importFrom stringr str_detect
#' @importFrom rlang .data
#'
#' @param proofed_data A proofed data table created by \code{\link{proof}}
#' @param type Type of tibble to report. Acceptable values include:
#'  \itemize{
#'  \item state
#'  \item field
#'  }
#' @param threshold Value above which errors should be tabulated
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

redFlags <-
  function(proofed_data, type, threshold = 0) {

    failProofed(proofed_data)

    # Fail if incorrect type supplied
    stopifnot("Error: Please supply 'state' or 'field' for `type` parameter." = type %in% c("state", "field"))

    # Fail if incorrect threshold supplied
    stopifnot("Error: `threshold` parameter must be numeric." = is.numeric(threshold))
    stopifnot("Error: Please supply a value between 0 and 1 for the `threshold` parameter." = (0 <= threshold & threshold <= 1))

    if (type == "state") {

      # State red flags
      rf <-
        errorLevelErrorsByState(proofed_data) |>
        mutate(
          flag =
            ifelse(
              .data$proportion > threshold,
              paste0("error > ", threshold),
              NA)) |>
        # Filter out errors that didn't exceed the threshold
        filter(!is.na(.data$flag)) |>
        arrange(desc(.data$proportion))

      if (nrow(rf) > 0) {
        return(rf)
      } else {
        message("No states with error exceeding the threshold.")
      }
    } else if (type == "field") {

      # Field red flags
      rf <-
        errorLevelErrorsByField(proofed_data) |>
        mutate(
          count_correct = .data$total - .data$count_errors,
          flag =
            ifelse(
              .data$proportion > threshold,
              paste0("error > ", threshold),
              NA)) |>
        select(-"total") |>
        relocate("count_correct", .before = "proportion") |>
        # Filter out errors that didn't exceed the threshold
        filter(!is.na(.data$flag)) |>
        arrange(desc(.data$proportion))

      if (nrow(rf) > 0) {
        return(rf)
      } else {
        message("No fields with error exceeding the threshold.")
      }
    } else {
      message("Error: Invalid type provided.")
    }
  }

