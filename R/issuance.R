#' Check issue dates and license years
#'
#' After cleaning the data with \code{\link{clean}}, ensure each record is
#' assigned the appropriate registration_yr.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#' @importFrom assertthat assert_that
#'
#' @param clean_data The object created after cleaning data with
#'   \code{\link{clean}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#' @param plot Create a plot? Default is FALSE
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

issueCheck <-
  function(clean_data, year, plot = FALSE) {
    failYear(year)
    failTF(plot)

    if (year != REF_CURRENT_SEASON) {
      message("! Are you sure you want to run this using year = ", year, "?")
    }

    # Stop if all values in record_key field are NA (causes problems with
    # joining later)
    assert_that(
      unique(is.na(clean_data$record_key)) != TRUE,
      msg = "All values in record_key are NA.")

    # Determine the destination of each record
    issue_assignments <- issueAssign(clean_data, year)

    # Return messages
    issueMessages(clean_data, issue_assignments)

    # Print results
    issuePrint(issue_assignments)

    # Plot results
    if (plot == TRUE) {
      issuePlot(issue_assignments, year)
    }

    # Create a frame of current and future data; past data filtered out
    current_data <-
      issue_assignments |>
      # Filter out decision values: invalid, past, bad issue date
      filter(.data$decision %in% c("current", "future", "MS")) |>
      select(-"decision")

    return(current_data)
  }

#' Return messages to console for issueCheck insights
#'
#' The internal \code{issueMessages} function is used inside of
#' \code{\link{issueCheck}} to return messages for bad issue_date values, count
#' of past registrations, count of future registrations, and 2-season overlap
#' registrations.
#'
#' @param clean_data The product of \code{\link{clean}}
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

issueMessages <-
  function(clean_data, issue_assignments) {
    regYearEditMessage(clean_data, issue_assignments)
    zeroDateMessage(clean_data)
    badDateMessage(issue_assignments)
    timeTravelMessage(issue_assignments)
    futureDateMessage(issue_assignments)
    pastDateMessage(issue_assignments)
    invalidDateMessage(issue_assignments)
    twoSeasonMessage(issue_assignments)
  }

#' Return message for changed registration_yr values
#'
#' The internal \code{regYearEditMessage} function is used inside of
#' \code{\link{issueMessages}} and \code{\link{issueCheck}} to return a message
#' for how many \code{registration_yr} values were changed.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom rlang .data
#'
#' @param clean_data The product of \code{\link{clean}}
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

regYearEditMessage <-
  function(clean_data, issue_assignments) {

    edited_years <-
      left_join(
        clean_data |>
          select(c("record_key", "dl_state", original_yr = "registration_yr")),
        issue_assignments |>
          select(c("record_key", edited_yr = "registration_yr")),
        by = "record_key") |>
      filter(.data$original_yr != .data$edited_yr) |>
      count(.data$dl_state, .data$original_yr, .data$edited_yr)

    if (nrow(edited_years) >= 1) {
      message(
        paste(
          "A total of", nrow(edited_years),
          "registration_yr values were changed.", sep = " "))
      print(
        edited_years |>
          rename(original = "original_yr", new = "edited_yr") |>
          arrange(desc(.data$n))
      )
    }
  }

#' Return message for 00/00/0000 issue_date value(s)
#'
#' The internal \code{zeroDateMessage} function is used inside of
#' \code{\link{issueMessages}} and \code{\link{issueCheck}} to return a message
#' if "00/00/0000" values are detected in the \code{issue_date} field.
#'
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param clean_data The product of \code{\link{clean}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

zeroDateMessage <-
  function(clean_data) {
    # Return message if issue_date = "00/00/0000" detected
    if (TRUE %in% str_detect(clean_data$issue_date, "00/00/0000")) {
      message(
        paste(
          "Error: issue_date value of 00/00/0000 detected in",
          nrow(filter(clean_data, .data$issue_date == "00/00/0000")),
          "record(s), which will be dropped."
        )
      )
    }
  }

#' Return message if bad issue dates detected
#'
#' The internal \code{badDateMessage} function is used inside of
#' \code{\link{issueMessages}} and \code{\link{issueCheck}} to return a message
#' if invalid \code{issue_date} values are detected.
#'
#' @importFrom stringr str_detect
#'
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

badDateMessage <-
  function(issue_assignments) {
    # Return message if "bad issue dates" detected
    if (TRUE %in% str_detect(issue_assignments$decision, "bad issue dates")) {
      message("Error: Bad issue_date value(s) detected.")
    }
  }

#' Return message if an issue date is after the file was submitted
#'
#' The internal \code{timeTravelMessage} function is used inside of
#' \code{\link{issueMessages}} and \code{\link{issueCheck}} to return a message
#' if \code{issue_date} values are after the date the file was submitted.
#'
#' @importFrom dplyr filter
#' @importFrom lubridate ymd
#' @importFrom lubridate mdy
#' @importFrom dplyr count
#' @importFrom rlang .data
#'
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

timeTravelMessage <-
  function(issue_assignments) {

    timetravel <-
      issue_assignments |>
      filter(mdy(.data$issue_date) > ymd(.data$dl_date))

    # Return message if any issue_date is after the file was submitted
    if (nrow(timetravel) > 0) {
      message(
        "Error: issue_date in the future detected (relative to file name).")
      print(
        timetravel |>
          count(
            .data$source_file,
            .data$dl_state,
            .data$issue_date,
            .data$registration_yr,
            .data$dl_date
          )
      )
    }
  }

#' Return message if future issue_date values are detected
#'
#' The internal \code{futureDateMessage} function is used inside of
#' \code{\link{issueMessages}} and \code{\link{issueCheck}} to return a message
#' if future \code{issue_date} values are detected.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

futureDateMessage <-
  function(issue_assignments) {

    futures <-
      issue_assignments |>
      filter(.data$decision == "future")

    # Return message for how many future records detected
    if (nrow(futures) == 0) {
      message("* 0 records need to be postponed for next season.")
    } else {
      message(
        paste(
          "*",
          format.default(
            nrow(futures),
            big.mark = ","),
          "records detected after their state's last day of hunting.",
          "Their registration year has been changed to registration_yr + 1."),
        sep = " ")
    }
  }

#' Return message if past issue_date values are detected
#'
#' The internal \code{futureDateMessage} function is used inside of
#' \code{\link{issueMessages}} and \code{\link{issueCheck}} to return a message
#' if past \code{issue_date} values are detected.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

pastDateMessage <-
  function(issue_assignments) {

    pasts <-
      issue_assignments |>
      filter(.data$decision == "past")

    # Return message for how many past records were found
    if (nrow(pasts) == 0) {
      message("* 0 past records detected.")
    } else {
      message(
        paste(
          "*",
          format.default(
            nrow(pasts),
            big.mark = ","),
          "past records detected. They have been filtered out.", sep = " "))
    }
  }

#' Return message if invalid issue_date values are detected
#'
#' The internal \code{invalidDateMessage} function is used inside of
#' \code{\link{issueMessages}} and \code{\link{issueCheck}} to return a message
#' if invalid \code{issue_date} values are detected.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

invalidDateMessage <-
  function(issue_assignments) {

    invalids <-
      issue_assignments |>
      filter(.data$decision == "invalid")

    # Return message for how many past records were found
    if (nrow(pasts) == 0) {
      message("* 0 invalid records detected.")
    } else {
      message(
        paste(
          "*",
          format.default(
            nrow(invalids),
            big.mark = ","),
          "invalid records detected. They have been filtered out.", sep = " "))
    }
  }

#' Return message if registrations are detected from two-season overlap windows
#'
#' The internal \code{twoSeasonMessage} function is used inside of
#' \code{\link{issueMessages}} and \code{\link{issueCheck}} to return a message
#' if registrations are detected from two-season states with \code{issue_date}
#' value(s) that occur during the overlap window.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

twoSeasonMessage <-
  function(issue_assignments) {

    overlaps <-
      issue_assignments |>
      filter(.data$decision == "overlap")

    # Return message for how many 2-season records detected
    if (nrow(overlaps) == 0) {
      message("* 0 records needed to be evaluated using registration_yr.")
    } else {
      message(
        paste(
          "*",
          format.default(
            nrow(overlaps),
            big.mark = ","),
          "records detected with issue date during a 2-season overlap window."),
        sep = " ")
    }
  }

#' Print results of issueCheck changes
#'
#' The internal \code{issuePrint} function is used inside of
#' \code{\link{issueCheck}} to print the number of registration types (past,
#' future, current, etc) per download state and registration year.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr arrange
#' @importFrom rlang .data
#'
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

issuePrint <-
  function(issue_assignments) {
    print(
      suppressMessages(
        issue_assignments |>
          filter(!.data$decision %in% c("MS", "current")) |>
          count(.data$dl_state, .data$registration_yr, .data$decision) |>
          arrange(.data$decision)
      )
    )
  }

#' Assign registration_yr values using issueDecide
#'
#' The internal \code{issueAssign} function is used inside of
#' \code{\link{issueCheck}} to assign a new registration year to each record
#' depending on the outcome of \code{\link{issueDecide}}.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @param clean_data The object created after cleaning data with
#'   \code{\link{clean}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

issueAssign <-
  function(clean_data, year) {
    failYear(year)

    clean_data |>
      # Filter out bad issue_date values
      filter(.data$issue_date != "00/00/0000") |>
      # Join in licensing dates
      left_join(
        REF_DATES |>
          rename(dl_state = "state"),
        by = "dl_state") |>
      # Create decision field
      issueDecide(year = year) |>
      # Drop unneeded fields
      select(-c("hunting_season", "issue_start", "issue_end")) |>
      # Overwrite registration_yr
      mutate(
        # For 1-season states: use current year for current records; year + 1
        # for future records
        registration_yr =
          case_when(
            .data$decision == "future" ~ as.character(year + 1),
            .data$decision == "current" ~ as.character(year),
            TRUE ~ .data$registration_yr)
      )
  }

#' Assign decisions to records using issue date and registration year
#'
#' The internal \code{issueDecide} function is used inside of
#' \code{\link{issueAssign}} and \code{\link{issueCheck}} to create the
#' \code{decision} field, which assigns records as current, past, future, etc;
#' no data changes are made other than creating the \code{deicison} field.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom lubridate mdy
#' @importFrom lubridate interval
#' @importFrom lubridate %within%
#' @importFrom rlang .data
#'
#' @param clean_data The object created after cleaning data with
#'   \code{\link{clean}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

issueDecide <-
  function(clean_data, year) {
    failYear(year)

    clean_data |>
      mutate(
        # Create decision field
        decision =
          ifelse(
            # Don't evaluate MS records
            .data$dl_state != "MS",
            case_when(
              # Records are past when issue_date is before issue_start
              mdy(.data$issue_date) < .data$issue_start ~ "past",
              # For 2-season states with overlapping start/end dates, records
              # are either current or future depending on the registration year
              # value
              dl_state %in% REF_STATES_2SEASON &
                mdy(.data$issue_date) %within%
                interval(.data$issue_start + 365, .data$issue_end) ~
                "overlap",
              # Records are current when the issue_date falls between
              # issue_start and issue_end
              mdy(.data$issue_date) %within%
                interval(.data$issue_start, .data$issue_end) ~
                "current",
              # Records are invalid when they fall outside of the issue window
              # (after issue end date but before next season's start date)
              !mdy(.data$issue_date) %within%
                interval(.data$issue_start, .data$issue_end) &
                !mdy(.data$issue_date) %within%
                  interval(.data$issue_start + 365, .data$issue_end + 365) &
                mdy(.data$issue_date) %within%
                  interval(.data$issue_end, .data$issue_end + 365) ~
                  "invalid",
              # Future records are issued after the last day of hunting and
              # after the projected issue start date for next season (the
              # registration_yr may need to be changed to +1)
              mdy(.data$issue_date) > .data$issue_end &
                mdy(.data$issue_date) %within%
                  interval(.data$issue_start + 365, .data$issue_end + 365) ~
                  "future",
              TRUE ~ "bad issue dates"),
            "MS")
      )
  }

#' Plot issue date errors
#'
#' The internal \code{issuePlot} function plots good and bad \code{issue_date}
#' values.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr distinct
#' @importFrom lubridate mdy
#' @importFrom dplyr tibble
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 scale_x_date
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_classic
#' @importFrom rlang .data
#'
#' @param issue_assignments An intermediate tibble in \code{\link{issueCheck}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

issuePlot <-
  function(issue_assignments, year) {
    failYear(year)

    rectangles <-
      tibble(
        season = c(paste(year - 1, year, sep = "-"),
                   paste(year, year + 1, sep = "-"),
                   paste(year + 1, year + 2, sep = "-")),
        values = c("#F0E442", "#56B4E9", "#0072B2"),
        xmin = c(as.Date(paste(year - 1, "09-01", sep = "-")),
                 as.Date(paste(year, "09-01", sep = "-")),
                 as.Date(paste(year + 1, "09-01", sep = "-"))),
        xmax = c(as.Date(paste(year, "03-11", sep = "-")),
                 as.Date(paste(year + 1, "03-11", sep = "-")),
                 as.Date(paste(year + 2, "03-11", sep = "-"))),
        ymin = -Inf,
        ymax = Inf)

    badplot_data <-
      issue_assignments |>
      filter(.data$decision != "current" & .data$dl_state != "MS") |>
      select(
        c("dl_state", "source_file", "issue_date", "registration_yr",
          "decision")) |>
      left_join(
        REF_DATES |>
          rename(dl_state = "state"),
        by = "dl_state") |>
      distinct()

    if (nrow(badplot_data > 0)) {
      badplot <-
        badplot_data |>
        ggplot() +
        geom_rect(
          data = rectangles,
          aes(
            xmin = .data$xmin,
            xmax = .data$xmax,
            ymin = .data$ymin,
            ymax = .data$ymax,
            fill = .data$season
          ),
          alpha = 0.3, inherit.aes = F) +
        geom_boxplot(
          aes(
            x = mdy(.data$issue_date),
            y = .data$dl_state,
            color = .data$registration_yr
          ),
          fill = "#FFFFFF", width = 0, size = 3, position = "identity") +
        geom_point(
          aes(x = .data$issue_start, y = .data$dl_state,
              shape = "Issue start")) +
        geom_point(
          aes(x = .data$issue_end, y = .data$dl_state,
              shape = "Issue end")) +
        labs(x = "Date", y = "State",
             title = "Non-current registrations",
             color = "Registration year") +
        scale_fill_manual("Seasons",
                          labels = c(paste(year - 1, year, sep = "-"),
                                     paste(year, year + 1, sep = "-"),
                                     paste(year + 1, year + 2, sep = "-")),
                          values = c("#F0E442", "#56B4E9", "#0072B2")) +
        scale_shape_manual(name = "", values = c(4, 1, 2)) +
        scale_x_date(breaks = c(rectangles$xmin[1], rectangles$xmax[1],
                                rectangles$xmin[2], rectangles$xmax[2],
                                rectangles$xmin[3], rectangles$xmax[3])) +
        theme_classic() +
        theme(axis.text = element_text(size = 11),
              axis.title = element_text(size = 14),
              panel.background = element_rect(color = "white"),
              panel.grid.major.y =
                element_line(color = "#666666", linetype = "dotted"))

      print(badplot)
    } else {
      message("* No bad data to plot.")
    }
  }
