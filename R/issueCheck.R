#' Check issue dates and license years
#'
#' After cleaning the data with \code{\link{clean}}, ensure each record is assigned the appropriate registration_yr.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom lubridate mdy
#' @importFrom lubridate interval
#' @importFrom lubridate %within%
#' @importFrom stringr str_detect
#' @importFrom utils write.csv
#' @importFrom tibble tibble
#' @importFrom ggplot2 geom_rect
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 scale_x_date
#'
#' @param data The object created after cleaning data with \code{\link{clean}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#' @param future_outpath Directory where future data should be saved as .csv
#' @param past_outpath Directory where past data should be saved as .csv
#' @param plot Create a plot? Default is FALSE
#' @param write Defaults to TRUE; should future data be written as .csv?
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

issueCheck <-
  function(data, year, future_outpath = NA, past_outpath = NA, plot = FALSE, write = TRUE){

    # Determine the destination of each record
    issue_assignments <-
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
                interval(issue_start, last_day_migbird_hunting) ~ "current",
              mdy(issue_date) < issue_start ~ "past",
              mdy(issue_date) > last_day_migbird_hunting ~ "future",
              TRUE ~ "bad issue dates"),
            "two season state"),
        reg_yr_eval =
          case_when(
            # For records from two-season states with a future registration year
            # and issue_date before the last day of hunting, copy them for next
            # year (and count them for this year)
            dl_state %in% states_twoseason &
              registration_yr == as.character(year + 1) &
              mdy(issue_date) %within%
              interval(issue_start, last_day_migbird_hunting) ~ "copy",
            # For records from two-season states with a current registration
            # year and issue date after the last day of hunting, postpone
            # sampling them until next year
            dl_state %in% states_twoseason &
              registration_yr == as.character(year) &
              mdy(issue_date) > last_day_migbird_hunting ~ "postpone",
            # For records from Mississippi, if the issue_date is within
            # Mississippi's hunting season, copy them for next year (and count
            # them for this year)
            dl_state == "MS" &
              !str_detect(source_file, "future") &
              mdy(issue_date) %within%
              interval(MS_firstday, MS_lastday) ~ "copy",
            # Postpone all other future registration_yr values, for all states
            registration_yr == as.character(year + 1) ~ "postpone",
            # Check for yr-1 registration_yr values for all states
            registration_yr == as.character(year - 1) ~ "past",
            TRUE ~ "nochange"),
        # Assign destination of each record to decision
        decision =
          case_when(
            issue_eval == "current" & reg_yr_eval == "nochange" ~ "nochange",
            issue_eval == "current" & reg_yr_eval == "past" ~ "bad",
            issue_eval == "current" & reg_yr_eval == "copy" ~ "copy",
            issue_eval == "current" & reg_yr_eval == "postpone" ~ "postpone",
            issue_eval == "past" & reg_yr_eval == "nochange" ~ "bad",
            issue_eval == "past" & reg_yr_eval == "past" ~ "past",
            issue_eval == "past" & reg_yr_eval == "copy" ~ "bad",
            issue_eval == "past" & reg_yr_eval == "postpone" ~ "bad",
            issue_eval == "future" & reg_yr_eval == "nochange" ~ "bad",
            issue_eval == "future" & reg_yr_eval == "past" ~ "bad",
            issue_eval == "future" & reg_yr_eval == "copy" ~ "copy",
            issue_eval == "future" & reg_yr_eval == "postpone" ~ "postpone",
            issue_eval == "two season state" & reg_yr_eval == "nochange" ~
              "nochange",
            issue_eval == "two season state" & reg_yr_eval == "past" ~ "past",
            issue_eval == "two season state" & reg_yr_eval == "copy" ~ "copy",
            issue_eval == "two season state" & reg_yr_eval == "postpone" ~
              "postpone",
            TRUE ~ NA_character_)
      ) |>
      select(-c("hunting_season", "issue_start", "issue_end",
                "last_day_migbird_hunting", "category"))

    # Return message if issue_date = "00/00/0000" detected
    if(TRUE %in% str_detect(data$issue_date, "00/00/0000")){
      message("Error: issue_date value of 00/00/0000 detected.")
    }

    # Return message if "bad issue dates" detected
    if(TRUE %in% str_detect(issue_assignments$decision, "bad issue dates")){
      message("Error: Bad issue_date value(s) detected.")
    }

    # Create a frame of current data
    current_data <-
      issue_assignments |>
      filter(!decision %in% c("past", "postpone")) |>
      mutate(
        registration_yr =
          ifelse(
            decision == "copy" & dl_state != "MS",
            as.character(as.numeric(registration_yr) - 1),
            registration_yr)) |>
      select(-c("decision", "reg_yr_eval", "issue_eval"))

    # Return messages for how many records have been copied
    # Message for NOT copied non-MS records
    if(
      nrow(
        issue_assignments |>
          filter(decision == "copy" & dl_state != "MS")) == 0){
      message(
        paste0(
          "* 0 future records need to be modified (registration_yr - 1) and co",
          "pied for next year."))
      }

    # Message for NOT copied MS records
    if(
      nrow(
        issue_assignments |>
          filter(decision == "copy" & dl_state == "MS")) == 0){
      message(
        paste0(
          "* 0 future Mississippi records need to be copied for next year."))
      }

    # Message for copied non-MS records
    if(
      nrow(
        issue_assignments |>
          filter(decision == "copy" & dl_state != "MS")) > 0){
      message(
        paste0(
          "* ",
          nrow(
            issue_assignments |>
              filter(decision == "copy" & dl_state != "MS")),
          " future records have been modified to have registration_yr - 1. The",
          "y have been copied and saved for next season in the future data .cs",
          "v with their original registration_yr."))
    }

    # Message for copied MS records with registration_yr change
    if(
      nrow(
        issue_assignments |>
        filter(
          decision == "copy" &
          dl_state == "MS" &
          as.numeric(registration_yr) > year)) > 0){
      message(
        paste0(
          "* ",
          nrow(
            issue_assignments |>
              filter(
                decision == "copy" &
                  dl_state == "MS" &
                  as.numeric(registration_yr) > year)), " Mississippi records ",
          "detected with registration_yr + 1. Please fix."))
    }

    # Message for copied MS records without registration_yr change
    if(
      nrow(
        issue_assignments |>
        filter(
          decision == "copy" &
          dl_state == "MS" &
          as.numeric(registration_yr) == year)) > 0){
      message(
        paste0(
          "* ",
          nrow(
            issue_assignments |>
              filter(
                decision == "copy" &
                  dl_state == "MS" &
                  as.numeric(registration_yr) == year)), " future Mississippi ",
          "records have been copied and saved for next season. Copied records ",
          "have been modified to have registration_yr + 1."))
    }

    # Return messages for how many records have been postponed
    if(nrow(issue_assignments |> filter(decision == "postpone")) == 0){
      message(
        paste0(
          "* 0 records need to be postponed for next year."))
    }else{
      message(
        paste0(
          "* ", nrow(issue_assignments |> filter(decision == "postpone")), " ",
          "future records have been postponed for next year. They will be filt",
          "ered out from this download and saved in the future data .csv with ",
          "their original registration_yr."))
    }

    # Return messages for how many past records have been set aside
    if(nrow(issue_assignments |> filter(decision == "past")) == 0){
      message(
        paste0(
          "* 0 past records detected."))
    }else{
      message(
        paste0(
          "* ", nrow(issue_assignments |> filter(decision == "past")), " past",
          " records have been set aside. They will be filtered out from this d",
          "ownload and returned for review in the download report."))
    }

    # Print results
    print(
      suppressMessages(
        issue_assignments |>
          group_by(source_file, issue_eval, reg_yr_eval, decision) |>
          summarize(n = n()) |>
          ungroup() |>
          group_by(reg_yr_eval, issue_eval, decision) |>
          summarize(sum_n = sum(n)) |>
          ungroup())
    )

    # Plot results
    if(plot == TRUE){

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
        issue_assignments |>
        filter(decision == "bad") |>
        select(dl_state, source_file, issue_date, registration_yr, decision) |>
        left_join(
          licenses_ref |>
            rename(dl_state = state),
          by = "dl_state") |>
        distinct()

      if(nrow(badplot_data > 0)){
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
               title = "Bad issue_date/registration_yr combinations") +
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
      }else{
        message("* No bad data to plot.")
      }
    }

    # Create future data frame
    future_data <-
      issue_assignments |>
      filter(decision %in% c("copy", "postpone")) |>
      mutate(
        registration_yr =
          ifelse(
            dl_state == "MS" & as.numeric(registration_yr) == year,
            as.character(as.numeric(registration_yr) + 1),
            registration_yr)) |>
      select(-c("decision", "reg_yr_eval", "issue_eval"))

    # Create past data frame
    past_data <-
      issue_assignments |>
      filter(decision == "past") |>
      select(-c("decision", "reg_yr_eval", "issue_eval"))

    # Write out future data if needed
    if(nrow(future_data) > 0 & write == TRUE){
      if(!str_detect(future_outpath, "/$")){
          future_outpath <- paste0(future_outpath, "/")
        }
      write.csv(
        future_data,
        paste0(
          future_outpath,
          "2022-2023_DL", future_data$dl_cycle[1], "_future_data.csv"),
        row.names = FALSE)
      message(
        paste0(
          "FUTURE DATA written to path:\n",
          future_outpath,
          "2022-2023_DL", future_data$dl_cycle[1], "_future_data.csv"))
      print(
        future_data |>
          group_by(source_file) |>
          summarize(n = n()) |>
          ungroup())
    }else if(nrow(future_data) > 0 & write == FALSE){
      message("* Future data write-out skipped.")
    }else if(nrow(future_data) == 0 & write == TRUE){
      message("* Future data write-out skipped; no data.")
    }

    # Write out past data if needed
    if(nrow(past_data) > 0 & write == TRUE){
      if(!str_detect(past_outpath, "/$")){
        past_outpath <- paste0(past_outpath, "/")
      }
      write.csv(
        past_data,
        paste0(past_outpath,
               "2022-2023_DL", past_data$dl_cycle[1], "_past_data.csv"),
        row.names = FALSE)
      message(
        paste0("PAST DATA written to path:\n",
               past_outpath,
               "2022-2023_DL", past_data$dl_cycle[1], "_past_data.csv"))
      print(
        past_data |>
          group_by(source_file) |>
          summarize(n = n()) |>
          ungroup())
    }else if(nrow(past_data) > 0 & write == FALSE){
      message("* Past data write-out skipped.")
    }else if(nrow(past_data) == 0 & write == TRUE){
      message("* Past data write-out skipped; no data.")
    }
    return(current_data)
  }
