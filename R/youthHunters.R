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

    # Make a table of total number of hunters per state
    total_hunters <-
      # Suppress group message from summarize function
      suppressMessages(
        x %>%
          group_by(dl_state) %>%
          summarize(total_registered = n()) %>%
          ungroup())

    # Count number and calculate proportion of youth hunters by state
    # Suppress group message from summarize function
    suppressMessages(
      x %>%
        select(dl_state, birth_date) %>%
        mutate(birth_year = str_extract(birth_date, "(?<=\\/)[0-9]{4}$")) %>%
        filter(birth_date > year - 16) %>%
        group_by(dl_state) %>%
        summarize(registered_youth = n()) %>%
        ungroup() %>%
        # Join in previous tibble of total hunters per state for proportion
        # calculation
        left_join(total_hunters, by = "dl_state") %>%
        filter(!is.na(dl_state)) %>%
        # Calculate proportion
        mutate(youth_proportion = registered_youth/total_registered) %>%
      # Round proportion to limit decimal places
      mutate(youth_proportion = round(youth_proportion, digits = 2)) %>%
      filter(!is.na(dl_state)) %>%
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
