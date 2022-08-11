#' Check issue dates and license years
#'
#' After cleaning the data with \code{\link{clean}}, ensure each record is assigned the appropriate registration_yr.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom lubridate mdy
#' @importFrom lubridate interval
#' @importFrom lubridate %within%
#' @importFrom stringr str_detect
#' @importFrom utils write.csv
#'
#' @param data The object created after cleaning data with \code{\link{clean}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#' @param outpath Directory where future data should be saved as .csv
#' @param write Defaults to TRUE; should future data be written as .csv?
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

issueCheck <-
  function(data, year, outpath = NA, write = TRUE){

    twoseasonstates <-
      licenses_ref %>%
      filter(category == "2 season") %>%
      select(state) %>%
      pull()

    issue_assignments <-
      data %>%
      left_join(
        licenses_ref %>%
          rename(dl_state = state),
        by = "dl_state") %>%
      # Filter out bad issue_date values
      filter(issue_date != "00/00/0000") %>%
      mutate(
        issue_id =
          case_when(
            # For records from two-season states with a future registration year
            # and issue_date before the last day of hunting, copy them for next
            # year (and count them for this year)
            dl_state %in% twoseasonstates &
              registration_yr == as.character(year + 1) &
              mdy(issue_date) %within%
              interval(issue_start, last_day_migbird_hunting) ~ "copy",
            # For records from two-season states with a current registration
            # year and issue date after the last day of hunting, postpone
            # sampling them until next year
            dl_state %in% twoseasonstates &
              registration_yr == as.character(year) &
              mdy(issue_date) > last_day_migbird_hunting ~ "postpone",
            # For records from Mississippi, if the issue_date is within
            # Mississippi's hunting season, copy them for next year (and count
            # them for this year)
            dl_state == "MS" &
              mdy(issue_date) %within%
              interval(MS_firstday, last_day_migbird_hunting) ~ "copy",
            # Postpone all other future registration_yr values, for all states
            registration_yr == as.character(year + 1) ~ "postpone",
            TRUE ~ "nochange")) %>%
      select(-c("hunting_season", "issue_start", "issue_end",
                "last_day_migbird_hunting", "category"))

    current_data <-
      issue_assignments %>%
      filter(issue_id != "postpone") %>%
      mutate(
        registration_yr =
          ifelse(
            issue_id == "copy",
            as.character(as.numeric(registration_yr) - 1),
            registration_yr)) %>%
      select(-issue_id)

    if(nrow(issue_assignments %>% filter(issue_id == "copy")) == 0){
      message(
        paste0(
          "CURRENT DATA is nrow == 0.\nNo records need to be modified (regist",
          "ration_yr - 1) and copied for next year."))
    }else{
      message(
        paste0(
          nrow(issue_assignments %>% filter(issue_id == "copy")), " records fr",
          "om two-season states have been modified to have registration_yr - 1",
          ". They will be copied for next season with their original registrat",
          "ion_yr."))
    }

    future_data <-
      issue_assignments %>%
      filter(issue_id %in% c("copy", "postpone")) %>%
      select(-issue_id)

    if(nrow(future_data) == 0){
      message(
        paste0(
          "FUTURE DATA is nrow == 0.\nNo records need to be saved for next yea",
          "r."))
    }else{
      if(write == TRUE){
        if(!str_detect(outpath, "/$")){
          outpath <- paste0(outpath, "/")
        }
        write.csv(
          future_data,
          paste0(outpath, "DL", future_data$dl_cycle[1], "_future_data.csv"),
          row.names = FALSE)
        message(
          paste0("FUTURE DATA written to path:\n",
                 outpath, "DL", future_data$dl_cycle[1], "_future_data.csv"))
      }
    }
    return(current_data)
  }
