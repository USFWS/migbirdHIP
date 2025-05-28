#' Check issue dates and license years
#'
#' After cleaning the data with \code{\link{clean}}, ensure each record is assigned the appropriate registration_yr.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr count
#' @importFrom dplyr rename
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd
#' @importFrom lubridate mdy
#' @importFrom stringr str_detect
#'
#' @param clean_data The object created after cleaning data with \code{\link{clean}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#' @param plot Create a plot? Default is FALSE
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

issueCheck <-
  function(clean_data, year, plot = FALSE){

    # Fail if incorrect year supplied
    stopifnot("Error: `year` parameter must be numeric." = is.numeric(year))
    stopifnot("Error: Incorrect value supplied for `year` parameter. Please use a 4-digit year in the 2020s, e.g. 2024." = str_detect(year, "^202[0-9]{1}$"))

    # Fail if incorrect plot supplied
    stopifnot("Error: Please supply TRUE or FALSE for `plot` parameter." = plot %in% c(TRUE, FALSE, T, F))

    # Return message if all values in record_key field are NA (causes problems
    # with joining later)
    if (TRUE == unique(is.na(unique(clean_data$record_key)))) {
      message("Error: All values in record_key are NA.")
    }

    # Determine the destination of each record
    issue_assignments <- issueAssign(clean_data, year)

    # Return message for future registration years being changed to the current
    # year for registrations with a current issue_date (e.g. current record with
    # registration_yr = X changed to X-1)
    eval_yrs <-
      left_join(
        clean_data |>
          select(record_key, dl_state, orig_yr = registration_yr),
        issue_assignments |>
          select(record_key, eval_yr = registration_yr),
        by = "record_key") |>
      filter(orig_yr != eval_yr) |>
      count(dl_state, orig_yr, eval_yr)

    if(nrow(eval_yrs) >= 1) {
      message(
        paste0(
          "Current registrations with registration_yr values not equal to ",
          year, " changed to ", year, "."))
      print(
        eval_yrs |>
          rename(original = orig_yr, new = eval_yr) |>
          arrange(desc(n))
      )
    }
    # Return message if any issue_date is after the file was submitted
    if(nrow(filter(clean_data, mdy(issue_date) > ymd(dl_date))) > 0) {
      message(
        "Error: issue_date in the future detected (relative to file name).")
      print(
        filter(issue_assignments, mdy(issue_date) > ymd(dl_date)) |>
          count(source_file, dl_state, issue_date, registration_yr, dl_date)
      )
    }
    # Return message if issue_date = "00/00/0000" detected
    if(TRUE %in% str_detect(clean_data$issue_date, "00/00/0000")) {
      message("Error: issue_date value of 00/00/0000 detected.")
    }
    # Return message if "bad issue dates" detected
    if(TRUE %in% str_detect(issue_assignments$decision, "bad issue dates")) {
      message("Error: Bad issue_date value(s) detected.")
    }
    # Return message for how many future records detected
    if(nrow(filter(issue_assignments, decision == "future")) == 0) {
      message("* 0 records need to be postponed for next season.")
    } else {
      message(
        paste(
          "*",
          format.default(
            nrow(filter(issue_assignments, decision == "future")),
            big.mark = ","),
          "records detected after their state's last day of hunting.",
          "Their registration year has been changed to registration_yr + 1."),
        sep = " ")
    }
    # Return message for how many past records were found
    if(nrow(issue_assignments |> filter(decision == "past")) == 0) {
      message("* 0 past records detected.")
    } else {
      message(
        paste(
          "*",
          format.default(
            nrow(issue_assignments |> filter(decision == "past")),
            big.mark = ","),
          "past records detected. They have been filtered out.", sep = " "))
    }

    # Print results
    print(
      suppressMessages(
        issue_assignments |>
          filter(!decision %in% c("MS", "current")) |>
          count(dl_state, registration_yr, decision) |>
          arrange(decision)
      )
    )

    # Plot results
    if(plot == TRUE){
      issuePlot(issue_assignments, year)
    }

    # Create a frame of current data
    current_data <-
      issue_assignments |>
      # Filter out past data
      filter(decision != "past") |>
      select(-decision)

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
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#'
#' @param clean_data The object created after cleaning data with \code{\link{clean}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

issueAssign <-
  function(clean_data, year) {

    # Fail if incorrect year supplied
    stopifnot("Error: `year` parameter must be numeric." = is.numeric(year))
    stopifnot("Error: Incorrect value supplied for `year` parameter. Please use a 4-digit year in the 2020s, e.g. 2024." = str_detect(year, "^202[0-9]{1}$"))

    clean_data |>
      # Join in licensing dates
      left_join(
        REF_DATES |>
          rename(dl_state = state),
        by = "dl_state") |>
      # Filter out bad issue_date values
      filter(issue_date != "00/00/0000") |>
      mutate(
        # Evaluate each record
        decision =
          ifelse(
            dl_state != "MS",
            case_when(
              # If the issue_date falls between issue_start and issue_end for
              # that state, it's a current record (no change needed)
              mdy(issue_date) %within%
                interval(issue_start, last_day_migbird_hunting) ~ "current",
              # Past records are issued before the issue_start date; these will
              # be filtered out later
              mdy(issue_date) < issue_start ~ "past",
              # If the record has an issue_date after the last day of hunting
              # for that state, it's a future record and the registration_yr
              # needs to be +1
              mdy(issue_date) > last_day_migbird_hunting ~ "future",
              TRUE ~ "bad issue dates"),
            "MS"),
        # Edit registration_yr: current year for current records, yr+1 for
        # future
        registration_yr =
          ifelse(
            decision == "future",
            as.character(year+1),
            as.character(year))
      ) |>
      select(-c("hunting_season", "issue_start", "issue_end",
                "last_day_migbird_hunting", "category"))
  }

#' Plot issue date errors
#'
#' The internal \code{issuePlot} function plots the output of \code{\link{issueAssign}}.
#'
#' @importFrom stringr str_detect
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
#'
#' @param issue_assigned_data The object created by \code{\link{issueAssign}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

issuePlot <-
  function(issue_assigned_data, year) {

    # Fail if incorrect year supplied
    stopifnot("Error: `year` parameter must be numeric." = is.numeric(year))
    stopifnot("Error: Incorrect value supplied for `year` parameter. Please use a 4-digit year in the 2020s, e.g. 2024." = str_detect(year, "^202[0-9]{1}$"))

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
      issue_assigned_data |>
      filter(decision != "current" & dl_state != "MS") |>
      select(dl_state, source_file, issue_date, registration_yr, decision) |>
      left_join(
        REF_DATES |>
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
             title = "Non-current registrations",
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
