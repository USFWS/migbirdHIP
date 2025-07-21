#' Summary table of how many registrations were dropped
#'
#' Internal function that summarizes the number of dropped registrations.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr tibble
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom assertthat assert_that
#'
#' @param raw_data The object created after reading in data with
#'   \code{\link{read_hip}}
#' @param clean_data The object created after cleaning data with
#'   \code{\link{clean}}
#' @param current_data The object created after filtering to current data with
#'   \code{\link{issueCheck}}
#' @param deduplicated_data The object created after deduplicating data with
#'   \code{\link{duplicateFix}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

nDropped <-
  function(raw_data, clean_data, current_data, deduplicated_data, year) {
    failYear(year)

    # Dropped by clean
    n_dropped_clean <- nDroppedClean(raw_data, clean_data)

    # Dropped by issueCheck
    n_dropped_current <- nDroppedCurrent(clean_data, current_data, year)

    # Dropped by fixDuplicates
    n_dropped_fixed <- nrow(current_data) - nrow(deduplicated_data)

    drop_sums <- sum(n_dropped_clean$n, n_dropped_current$n, n_dropped_fixed)
    drop_check <- drop_sums == nrow(raw_data) - nrow(deduplicated_data)

    stopifnot(
      "Error: Number of dropped records does not match." = drop_check == TRUE)

    # Output
    bind_rows(
      n_dropped_clean,
      n_dropped_current,
      tibble(decision = "duplicate records", n = n_dropped_fixed)) |>
      filter(.data$n != 0)
  }

#' Summary table of how many registrations were dropped by clean
#'
#' Internal function that helps \code{\link{nDropped}} by summarizing the number
#' of dropped registrations by \code{\link{clean}}.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr tibble
#' @importFrom dplyr left_join
#' @importFrom tidyr unite
#' @importFrom stringr str_remove_all
#' @importFrom dplyr count
#' @importFrom rlang .data
#'
#' @param raw_data The object created after reading in data with
#'   \code{\link{read_hip}}
#' @param clean_data The object created after cleaning data with
#'   \code{\link{clean}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

nDroppedClean <-
  function(raw_data, clean_data) {
    ndropped_clean_total <- nrow(raw_data) - nrow(clean_data)

    ndropped_clean_NA_0 <-
      raw_data |>
      filter(!!LOGIC_ZERO_BAGS) |>
      select("record_key") |>
      mutate(r1 = "records with all-NA or all-0 bag values")

    ndropped_clean_nondigit <-
      raw_data |>
      filter(!!LOGIC_NONDIGIT_BAGS) |>
      select("record_key") |>
      mutate(r2 = "records where any bag value is non-digit")

    ndropped_clean_test <-
      raw_data |>
      filter(!!LOGIC_TEST_RECORD) |>
      select("record_key") |>
      mutate(r3 = "test records")

    ndropped_clean_PII <-
      raw_data |>
      filter(
        !!LOGIC_MISSING_PII |
          !!LOGIC_MISSING_ADDRESSES |
          !!LOGIC_MISSING_CITY_ZIP_EMAIL) |>
      select("record_key") |>
      mutate(r4 = "records missing critical PII")

    # Create a tibble of reasons for being dropped by joining the above tibbles
    # using record_key, since some registrations qualify for more than one
    # reason and we don't want to double count them
    ndropped_clean_decisions <-
      tibble(
        record_key =
          unique(
            c(ndropped_clean_NA_0$record_key,
              ndropped_clean_nondigit$record_key,
              ndropped_clean_test$record_key,
              ndropped_clean_PII$record_key))) |>
      left_join(ndropped_clean_NA_0, by = "record_key") |>
      left_join(ndropped_clean_nondigit, by = "record_key") |>
      left_join(ndropped_clean_test, by = "record_key") |>
      left_join(ndropped_clean_PII, by = "record_key")

    # Summarize the number of records dropped per decision
    ndropped_clean_decision_counts <-
      ndropped_clean_decisions |>
      unite("r1":"r4", col = "decision", sep = " & ", na.rm = T) |>
      mutate(decision = str_remove_all(.data$decision, "(?<=\\&) records")) |>
      count(.data$decision)

    # Check to see if the number of registrations dropped using the logical
    # expressions in clean() equals the difference in rows between raw_data and
    # clean_data
    ndropped_clean_check1 <-
      nrow(ndropped_clean_decisions) == ndropped_clean_total

    stopifnot(
      "Error: Number of dropped records does not match." =
        ndropped_clean_check1 == TRUE)

    # Check to see if the non-unique sum of registrations dropped is equal to OR
    # exceeds the difference in rows between raw_data and clean_data
    ndropped_clean_check2 <-
      sum(ndropped_clean_decision_counts$n) >= ndropped_clean_total

    stopifnot(
      "Error: Number of dropped records does not match." =
        ndropped_clean_check2 == TRUE)

    return(ndropped_clean_decision_counts)
  }

#' Summary table of how many registrations were dropped by issueCheck
#'
#' Internal function that helps \code{\link{nDropped}} by summarizing the number
#' of dropped registrations by \code{\link{issueCheck}}.
#'
#' @importFrom dplyr count
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @param clean_data The object created after cleaning data with
#'   \code{\link{clean}}
#' @param current_data The object created after filtering to current data with
#'   \code{\link{issueCheck}}
#' @param year The year of the HIP season (e.g. 2022 for the 2022-2023 season)
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}

nDroppedCurrent <-
  function(clean_data, current_data, year) {
    ndropped_current_total <- nrow(clean_data) - nrow(current_data)

    # Create a tibble of how many records were dropped by issueCheck
    ndropped_current_decisions <-
      issueAssign(clean_data, year) |>
      count(.data$decision) |>
      filter(!.data$decision %in% c("current", "MS", "future")) |>
      mutate(
        decision =
          case_when(
            .data$decision == "past" ~ "past records",
            .data$decision == "invalid" ~ "invalid records",
            TRUE ~ .data$decision
          ))

    # Check to see if the sum of records dropped based on
    # issueAssign/issueDecision is equal to the difference in rows between
    # clean_data and current_data
    ndropped_current_check <-
      sum(ndropped_current_decisions$n) == ndropped_current_total

    stopifnot(
      "Error: Number of dropped records does not match." =
        ndropped_current_check == TRUE)

    return(ndropped_current_decisions)
  }
