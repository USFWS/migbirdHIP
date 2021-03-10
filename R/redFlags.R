#' Pull bad data
#'
#' Create a tibble of error data by state or field. Data are reported using a threshold of proportion of error.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom tidyr separate_rows
#' @importFrom tidyr separate
#' @importFrom tidyr pivot_longer
#'
#' @param x A proofed data table created by \code{\link{proof}}
#' @param type Type of tibble to report. Acceptable values include:
#'  \itemize{
#'  \item state
#'  \item field
#'  }
#' @param threshold Value above which errors should be tabulated
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

redFlags <-
  function(x, type, threshold = 0){
    if(type == "state"){

      # State red flags

      # Suppress warning: "Expected 25 pieces. Missing pieces filled with `NA`
      # in ... rows". We start by splitting errors for plotting purposes; if
      # there are less than the full amount of errors in a row, the warning
      # happens.
      suppressWarnings(
        # Suppress message from summarize: "`summarise()` regrouping output by
        # 'dl_state' (override with `.groups` argument)
        suppressMessages(
          x %>%
            select(errors, dl_state) %>%
            # Pull errors apart, delimited by hyphens
            separate(errors, into = as.character(c(1:25)), sep = "-") %>%
            # Transform errors into a single column
            pivot_longer(1:25, names_to = "name") %>%
            select(-name) %>%
            rename(errors = value) %>%
            group_by(dl_state) %>%
            # Count correct and incorrect values
            summarize(
              count_errors = sum(!is.na(errors)),
              count_correct = sum(is.na(errors))) %>%
            ungroup() %>%
            # Calculate proportion
            mutate(
              proportion = count_errors / (count_errors + count_correct)) %>%
            mutate(
              flag =
                ifelse(
                  proportion > threshold,
                  paste0("error > ", threshold),
                  NA)) %>%
            # Filter out errors that didn't exceed the threshold
            filter(!is.na(flag)) %>%
            arrange(desc(proportion))
        )
      )
    }
    else if(type == "field"){

      # Field red flags

      # Suppress warning: "Expected 25 pieces. Missing pieces filled with `NA`
      # in ... rows". We start by splitting errors for plotting purposes; if
      # there are less than the full amount of errors in a row, the warning
      # happens.
      suppressWarnings(
        # Suppress message from summarize: "`summarise()` regrouping output by
        # 'dl_state' (override with `.groups` argument)
        suppressMessages(
          x %>%
            select(errors) %>%
            # Pull errors apart, delimited by hyphens
            separate_rows(errors, sep = "-") %>%
            group_by(errors) %>%
            # Count the errors by field
            summarize(count_errors = sum(!is.na(errors))) %>%
            ungroup() %>%
            # Calculate number correct and proportion of error
            mutate(
              count_correct = nrow(x) - count_errors,
              proportion = count_errors / nrow(x)) %>%
            mutate(
              flag =
                ifelse(
                  proportion > threshold,
                  paste0("error > ", threshold),
                  NA)
            ) %>%
            # Filter out errors that didn't exceed the threshold
            filter(!is.na(flag)) %>%
            arrange(desc(proportion))
        )
      )
    }
    else{
      message("Error: Invalid type provided.")
    }

  }
