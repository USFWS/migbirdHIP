#' Check issue dates and license years
#'
#' After cleaning the data with \code{\link{clean}}, ensure each record is assigned the appropriate registration_yr.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr count
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom lubridate ymd
#' @importFrom lubridate mdy
#'
#' @param data The object created after cleaning data with \code{\link{clean}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#' @param plot Create a plot? Default is FALSE
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

issueCheck <-
  function(data, year, plot = FALSE){

    # Determine the destination of each record
    issue_assignments <- issueAssign(data, year)

    # Return message if any issue_date is after the file was submitted
    if(nrow(filter(data, mdy(issue_date) > ymd(dl_date))) > 0) {
      message("Error: issue_date in the future detected.")
      print(
        filter(issue_assignments, mdy(issue_date) > ymd(dl_date)) |>
          select(source_file, dl_state, issue_date, registration_yr, dl_date)
      )
    }
    # Return message if issue_eval is NA (issue_date or reg yr not evaluated)
    if(TRUE %in% is.na(issue_assignments$issue_eval)) {
      message("Error: NA in issue_eval.")
      print(
        filter(issue_assignments, is.na(issue_eval)) |>
          select(dl_state, issue_date, registration_yr)
        )
    }
    # Return message if reg_yr_eval is NA (issue_date or reg yr not evaluated)
    if(TRUE %in% is.na(issue_assignments$reg_yr_eval)) {
      message("Error: NA in reg_yr_eval.")
      print(
        filter(issue_assignments, is.na(reg_yr_eval)) |>
          select(dl_state, issue_date, registration_yr)
      )
    }
    # Return message if issue_date = "00/00/0000" detected
    if(TRUE %in% str_detect(data$issue_date, "00/00/0000")) {
      message("Error: issue_date value of 00/00/0000 detected.")
    }
    # Return message if "bad issue dates" detected
    if(TRUE %in% str_detect(issue_assignments$decision, "bad issue dates")) {
      message("Error: Bad issue_date value(s) detected.")
    }
    # Return messages for how many records need to be recycled
    if(nrow(filter(issue_assignments, decision == "copy")) == 0) {
      message("* 0 records need to be recycled for next season.")
    }
    # Return message for number of non-MS records that need to be recycled
    if(
      nrow(
        filter(issue_assignments, decision == "copy" & dl_state != "MS")) > 0) {
      message(
        paste(
          "*",
          nrow(
            filter(issue_assignments, decision == "copy" & dl_state != "MS")),
          "non-MS records must be recycled for next season."), sep = " ")
    }
    # Return message for how many records must be postponed
    if(nrow(filter(issue_assignments, decision == "postpone")) == 0) {
      message("* 0 records need to be postponed for next season.")
    } else {
      message(
        paste(
          "*", nrow(filter(issue_assignments, decision == "postpone")),
          "records must be postponed for next year."), sep = " ")
    }
    # Return message for how many past records were found
    if(nrow(issue_assignments |> filter(decision == "past")) == 0) {
      message("* 0 past records detected.")
    } else {
      message(
        paste(
          "*", nrow(issue_assignments |> filter(decision == "past")),
          "past records detected. They have been filtered out.", sep = " "))
    }

    # Print results
    print(
      suppressMessages(
        issue_assignments |>
          count(source_file, issue_eval, reg_yr_eval, decision) |>
          group_by(reg_yr_eval, issue_eval, decision) |>
          summarize(sum_n = sum(n)) |>
          ungroup() |>
          arrange(issue_eval) |>
          mutate(decision = ifelse(decision == "nochange", "current", decision))
        )
    )

    # Plot results
    if(plot == TRUE){
      issuePlot(issue_assignments, year)
    }

    # Create a frame of current data
    current_data <-
      issue_assignments |>
      filter(decision != "past") |>
      select(-c("decision", "reg_yr_eval", "issue_eval"))

    return(current_data)
  }

#' Assign decisions to records on how to process them using issue dates and license years
#'
#' The internal \code{issueAssign} function is used inside of \code{\link{issueCheck}} to determine which records in the output from \code{\link{clean}} are current, past, future, or incorrect.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom lubridate mdy
#' @importFrom lubridate interval
#' @importFrom lubridate %within%
#' @importFrom lubridate years
#' @importFrom stringr str_detect
#' @importFrom dplyr select
#'
#' @param data The object created after cleaning data with \code{\link{clean}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

issueAssign <-
  function(data, year) {
    data |>
      left_join(
        licenses_ref |>
          rename(dl_state = state),
        by = "dl_state") |>
      # Filter out bad issue_date values
      filter(issue_date != "00/00/0000") |>
      mutate(
        # Calculate registration_yr for all one-season states
        # (does not include MS)
        issue_eval =
          ifelse(
            !dl_state %in% c(states_twoseason, "MS"),
            case_when(
              mdy(issue_date) %within%
                interval(issue_start, issue_end) ~ "current",
              mdy(issue_date) < issue_start ~ "past",
              mdy(issue_date) > issue_end ~ "future",
              TRUE ~ "bad issue dates"),
            "two season state"),
        # More calculations for 2-season states, and evaluate registration year
        # for 1-season states
        reg_yr_eval =
          case_when(
            # For records from two-season states with an issue_date between
            # issue_start and issue_end, copy them for next year (and count them
            # for this year)
            dl_state %in% states_twoseason &
              mdy(issue_date) %within% interval(issue_start, issue_end) ~
              "copy",
            # For records from two-season states with an issue date after the
            # PREDICTED issue_start for next season, postpone sampling them
            # until next season
            dl_state %in% states_twoseason &
              mdy(issue_date) >= (issue_start + years(1)) ~ "postpone",
            # For records from two-season states with an old issue date, past
            dl_state %in% states_twoseason &
              mdy(issue_date) < issue_start ~ "past",
            # For records from Mississippi, if the issue_date is within
            # Mississippi's hunting season, copy them for next year (and count
            # them for this year)
            dl_state == "MS" &
              mdy(issue_date) %within% interval(MS_firstday, MS_lastday) ~
              "copy",
            # For records from Mississippi with an issue date after the last day
            # of hunting, postpone sampling them until next season
            dl_state == "MS" & mdy(issue_date) > MS_lastday ~ "postpone",
            # For records from Mississippi with an issue date 365 days or more
            # before the first day of hunting
            dl_state == "MS" & mdy(issue_date) < MS_firstday - years(1) ~
              "past",
            # For records from Mississippi with an issue date
            # within 365 days of the last day of hunting, current
            dl_state == "MS" & mdy(issue_date) %within%
              interval(MS_lastday - years(1), MS_lastday) ~
              "current",
            # Postpone all other future registration_yr values, for all states
            as.numeric(registration_yr) > year ~ "postpone",
            # Check for old registration_yr values for all states
            as.numeric(registration_yr) < year ~ "past",
            # Current registration year
            as.numeric(registration_yr) == year ~ "current",
            TRUE ~ NA_character_),
        # Assign destination of each record to decision
        decision =
          case_when(
            is.na(reg_yr_eval) ~ NA_character_,
            dl_state == "MS" & reg_yr_eval == "past" ~ "past",
            dl_state == "MS" & reg_yr_eval == "copy" ~ "copy",
            dl_state == "MS" & reg_yr_eval == "postpone" ~ "postpone",
            dl_state == "MS" & reg_yr_eval == "current" ~ "nochange",
            issue_eval == "bad issue dates" ~ "bad",
            issue_eval == "current" ~ "nochange",
            issue_eval == "past" ~ "past",
            issue_eval == "future" ~ "postpone",
            issue_eval == "two season state" & reg_yr_eval == "current" ~
              "nochange",
            issue_eval == "two season state" & reg_yr_eval == "past" ~ "past",
            issue_eval == "two season state" & reg_yr_eval == "copy" ~ "copy",
            issue_eval == "two season state" & reg_yr_eval == "postpone" ~
              "postpone",
            TRUE ~ NA_character_)
      ) |>
      select(-c("hunting_season", "issue_start", "issue_end",
                "last_day_migbird_hunting", "category"))
  }


#' Plot issue date errors
#'
#' The internal \code{issuePlot} function plots the output of \code{\link{issueAssign}}.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr distinct
#' @importFrom lubridate mdy
#' @importFrom tibble tibble
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
#'
#' @param assigned_data The object created by \code{\link{issueAssign}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

issuePlot <-
  function(assigned_data, year) {
    rectangles <-
      tibble(
        season = c(paste(year-1, year, sep = "-"),
                   paste(year, year+1, sep = "-"),
                   paste(year+1, year+2, sep = "-")),
        values = c("#F0E442", "#56B4E9", "#0072B2"),
        xmin = c(as.Date(paste(year-1, "09-01", sep = "-")),
                 as.Date(paste(year, "09-01", sep = "-")),
                 as.Date(paste(year+1, "09-01", sep = "-"))),
        xmax = c(as.Date(paste(year, "03-11", sep = "-")),
                 as.Date(paste(year+1, "03-11", sep = "-")),
                 as.Date(paste(year+2, "03-11", sep = "-"))),
        ymin = -Inf,
        ymax = Inf)

    badplot_data <-
      assigned_data |>
      filter(decision == "bad") |>
      select(dl_state, source_file, issue_date, registration_yr, decision) |>
      left_join(
        licenses_ref |>
          rename(dl_state = state),
        by = "dl_state") |>
      distinct()

    if(nrow(badplot_data > 0)) {
      badplot <-
        badplot_data |>
        ggplot() +
        geom_rect(
          data = rectangles,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
              fill = season),
          alpha = 0.3, inherit.aes = F) +
        geom_boxplot(
          aes(x = mdy(issue_date), y = dl_state, color = registration_yr),
          fill = "#FFFFFF", width = 0, size = 3, position = "identity") +
        geom_point(
          aes(x = last_day_migbird_hunting, y = dl_state,
              shape = "Last day of hunting")) +
        geom_point(
          aes(x = issue_start, y = dl_state,
              shape = "Issue start")) +
        geom_point(
          aes(x = issue_end, y = dl_state,
              shape = "Issue end")) +
        labs(x = "Date", y = "State",
             title = "Bad issue_date/registration_yr combinations",
             color = "Registration year") +
        scale_fill_manual("Seasons",
                          labels = c(paste(year-1, year, sep = "-"),
                                     paste(year, year+1, sep = "-"),
                                     paste(year+1, year+2, sep = "-")),
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
