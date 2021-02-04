#' Sample hunters
#'
#' After finalizing the data with \code{\link{correct}}, select a subset of hunters with valid registrations and addresses.
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#'
#' @param x The object created after finalizing data with \code{\link{correct}}
#' @param random Optional ability to supply the sample size desired as an integer. The sample will be subset from the data at random and is not reproducible.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

hunterSample <-
  function(x, random = NA) {

    if(is.na(random)){

      sample_x <-
        x %>%
        # No errors in the record
        filter(is.na(errors)) %>%
        # Select relevant fields
        select(
          title:zip,
          email,
          hunt_mig_birds,
          issue_date,
          registration_yr,
          dl_state:errors)

      return(sample_x)

    }
    else{

      random_sample <-
        x %>%
        # No errors in the record
        filter(is.na(errors)) %>%
        # Select relevant fields
        select(
          title:zip,
          email,
          hunt_mig_birds,
          issue_date,
          registration_yr,
          dl_state:errors) %>%
        # Subset random rows
        slice_sample(n = random)

      return(random_sample)

    }
  }
