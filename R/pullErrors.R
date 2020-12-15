#' @import dplyr
#' @import stringr

pullErrors <-
  # x = proofed data tibble, error = field you want to check
  function(x, error){

    pulled_error <-
      x %>%
      select(error, errors) %>%
      filter(str_detect(errors, error)) %>%
      select(error) %>%
      distinct() %>%
      pull()

    if(is_empty(pulled_error)) message("Success! All values are correct.")

    else(return(pulled_error))

  }
