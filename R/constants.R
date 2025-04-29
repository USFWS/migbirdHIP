#' @importFrom rlang expr

# Define variables to evaluate data consistently across functions

# Define inline permits that did not hunt
LOGIC_INLINE_PMT_DNH <-
  expr(
    dl_state %in% c("OR", "WA") &
      hunt_mig_birds != "2" &
      sum(as.numeric(band_tailed_pigeon),
          as.numeric(brant),
          as.numeric(seaducks)) >= 2)
