#' Correct data
#'
#' After flagging errors in the data with \code{\link{proof}}, attempt corrections in all fields. Errors that cannot be programmatically corrected will be reported for manual correction.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import tibble
#'
#' @param x The object created after error flagging data with \code{\link{proof}}
#' @param year The year in which the Harvest Information Program data were collected
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHarvestData}
#'
#' @export

correct <-
  function(x, year){

    corrected_x <-
      x %>%
      mutate(
        # Title correction
        title =
          # Set to NA if title is flagged
          ifelse(str_detect(errors, "title"), NA, title),
        # Suffix correction
        suffix =
          # Set to NA if suffix is flagged
          ifelse(str_detect(errors, "suffix"), NA, suffix),
        # Zip code correction
        zip =
          # Insert a hyphen in continuous 9 digit zip codes
          ifelse(
            str_detect(zip, "^[0-9]{9}$"),
            paste0(
              str_extract(zip, "^[0-9]{5}"),
              "-",
              str_extract(zip,"[0-9]{4}$")),
            zip),
        zip =
          # Insert a hyphen in 9 digit zip codes with a middle space
          ifelse(
            str_detect(zip, "^[0-9]{5}\\s[0-9]{4}$"),
            str_replace(zip, "\\s", "\\-"),
            zip),
        zip =
          # Remove trailing -0000
          ifelse(
            str_detect(zip, "\\-0000"),
            str_remove(zip, "\\-0000"),
            zip),
        zip =
          # Remove trailing -___
          ifelse(
            str_detect(zip, "\\-\\_+"),
            str_remove(zip, "\\-\\_+"),
            zip),
        # Email
        email =
          # Delete spaces
          str_remove_all(email, " "),
        email =
          # Delete slashes /
          str_remove_all(email, "\\/"),
        email =
          # Delete commas
          str_remove_all(email, "\\,"),
        email =
          # To lowercase
          tolower(email),
        email =
          # Fix multiple @
          ifelse(
            str_detect(email, "\\@\\@+"),
            str_replace(email, "\\@\\@+", "\\@"),
            email),
        email =
          # Add endings to common domains
          case_when(
            str_detect(email, "\\@gmail$") ~
              str_replace(email, "\\@gmail$", "\\@gmail\\.com"),
            str_detect(email, "\\@yahoo$") ~
              str_replace(email, "\\@yahoo$", "\\@yahoo\\.com"),
            str_detect(email, "\\@aol$") ~
              str_replace(email, "\\@aol$", "\\@aol\\.com"),
            str_detect(email, "\\@comcast$") ~
              str_replace(email, "\\@comcast$", "\\@comcast\\.net"),
            str_detect(email, "\\@verizon$") ~
              str_replace(email, "\\@verizon$", "\\@verizon\\.net"),
            str_detect(email, "\\@cox$") ~
              str_replace(email, "\\@cox$", "\\@cox\\.net"),
            str_detect(email, "\\@outlook$") ~
              str_replace(email, "\\@outlook$", "\\@outlook\\.com"),
            str_detect(email, "\\@hotmail$") ~
              str_replace(email, "\\@hotmail$", "\\@hotmail\\.com"),
            TRUE ~ email
          ),
        email =
          # Fix punctuation
          case_when(
            # Correct .ccom typos
            str_detect(email, "(?<=\\.)ccom$") ~
              str_replace(email, "ccom$", "com"),
            # Add period if missing
            str_detect(email, "(?<=[^\\.])com$") ~
              str_replace(email, "com$", "\\.com"),
            str_detect(email, "(?<=[^\\.])net$") ~
              str_replace(email, "net$", "\\.net"),
            str_detect(email, "(?<=[^\\.])edu$") ~
              str_replace(email, "edu$", "\\.edu"),
            str_detect(email, "(?<=[^\\.])gov$") ~
              str_replace(email, "gov$", "\\.gov"),
            str_detect(email, "(?<=[^\\.])org$") ~
              str_replace(email, "org$", "\\.org"),
            TRUE ~ email
          ),
        email =
          # Replace with NA
          case_when(
            # If there is no @
            !str_detect(email, "\\@") ~ NA_character_,
            # If the email is invalid
            str_detect(email, "noemail\\@") ~ NA_character_,
            str_detect(email, "^none@none$") ~ NA_character_,
            # If there are any remaining "NA" type strings
            str_detect(email, "n\\/a") ~ NA_character_,
            # If there is only an @
            str_detect(email, "^@$") ~ NA_character_,
            TRUE ~ email
          )
      ) %>%
      # Hunt migratory birds correction
      # Bag correction: ducks
      left_join(
        hip_bags_ref %>%
          mutate(ducks_bag = as.character(stateBagValue)) %>%
          filter(spp == "ducks_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "ducks_bag")
      ) %>%
      mutate(ducks_bag = FWSstratum) %>%
      select(-FWSstratum) %>%
      # Bag correction: geese
      left_join(
        hip_bags_ref %>%
          mutate(geese_bag = as.character(stateBagValue)) %>%
          filter(spp == "geese_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "geese_bag")
      ) %>%
      mutate(geese_bag = FWSstratum) %>%
      select(-FWSstratum) %>%
      # Bag correction: dove
      left_join(
        hip_bags_ref %>%
          mutate(dove_bag = as.character(stateBagValue)) %>%
          filter(spp == "dove_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "dove_bag")
      ) %>%
      mutate(dove_bag = FWSstratum) %>%
      select(-FWSstratum) %>%
      # Bag correction: woodcock
      left_join(
        hip_bags_ref %>%
          mutate(woodcock_bag = as.character(stateBagValue)) %>%
          filter(spp == "woodcock_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "woodcock_bag")
      ) %>%
      mutate(woodcock_bag = FWSstratum) %>%
      select(-FWSstratum) %>%
      # Bag correction: coots and snipe
      left_join(
        hip_bags_ref %>%
          mutate(coots_snipe_bag = as.character(stateBagValue)) %>%
          filter(spp == "coots_snipe_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "coots_snipe_bag")
      ) %>%
      mutate(coots_snipe_bag = FWSstratum) %>%
      select(-FWSstratum) %>%
      # Bag correction: rails and gallinules
      left_join(
        hip_bags_ref %>%
          mutate(rails_gallinules_bag = as.character(stateBagValue)) %>%
          filter(spp == "rails_gallinules_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "rails_gallinules_bag")
      ) %>%
      mutate(rails_gallinules_bag = FWSstratum) %>%
      select(-FWSstratum) %>%
      # Bag correction: cranes
      left_join(
        hip_bags_ref %>%
          mutate(cranes = as.character(stateBagValue)) %>%
          filter(spp == "cranes") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "cranes")
      ) %>%
      mutate(cranes = FWSstratum) %>%
      select(-FWSstratum) %>%
      # Bag correction: band-tailed pigeon
      left_join(
        hip_bags_ref %>%
          mutate(band_tailed_pigeon = as.character(stateBagValue)) %>%
          filter(spp == "band_tailed_pigeon") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "band_tailed_pigeon")
      ) %>%
      mutate(band_tailed_pigeon = FWSstratum) %>%
      select(-FWSstratum) %>%
      # Bag correction: brant
      left_join(
        hip_bags_ref %>%
          mutate(brant = as.character(stateBagValue)) %>%
          filter(spp == "brant") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "brant")
      ) %>%
      mutate(brant = FWSstratum) %>%
      select(-FWSstratum) %>%
      # Bag correction: seaducks
      left_join(
        hip_bags_ref %>%
          mutate(seaducks = as.character(stateBagValue)) %>%
          filter(spp == "seaducks") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "seaducks")
      ) %>%
      mutate(seaducks = FWSstratum) %>%
      select(-FWSstratum)

    # Check zip codes...

    prefix_3 <-
      zip_code_ref %>%
      filter(str_detect(zipPrefix, "^[0-9]{3}$")) %>%
      select(state) %>%
      distinct() %>%
      arrange(state) %>%
      pull()

    prefix_2 <-
      zip_code_ref %>%
      filter(str_detect(zipPrefix, "^[0-9]{2}$")) %>%
      select(state) %>%
      distinct() %>%
      arrange(state) %>%
      pull()

    prefix_1 <-
      zip_code_ref %>%
      filter(str_detect(zipPrefix, "^[0-9]{1}$")) %>%
      select(state) %>%
      distinct() %>%
      arrange(state) %>%
      pull()

    # Re-run the proof script to get an updated errors column

    corrproof_x <-
      proof(corrected_x, year = year) %>%
      # Proof the zip codes -- are they associated with the correct states?
      # Make a zipPrefix to join the reference table by
      mutate(
        # Pull the appropriate number of zip numbers according to the state
        zipPrefix =
          case_when(
            state %in% prefix_3 ~ str_extract(zip, "^[0-9]{3}"),
            state %in% prefix_2 ~ str_extract(zip, "^[0-9]{2}"),
            state %in% prefix_1 ~ str_extract(zip, "^[0-9]{1}"),
            TRUE ~ NA_character_),
        # Since NY has 1 and 3 zipPrefix numbers, do a quick correction
        zipPrefix =
          ifelse(
            state == "NY" & str_detect(zip, "^5"),
            "5",
            zipPrefix)) %>%
      left_join(
        zip_code_ref %>%
          mutate(zipPrefix = as.character(zipPrefix)) %>%
          select(zipPrefix, zipState = state),
        by = "zipPrefix") %>%
      # Add an error if the state doesn't match zipState
      mutate(
        errors =
          case_when(
            state != zipState & is.na(errors) ~ "zip",
            state != zipState & !is.na(errors) & !str_detect(errors, "zip") ~
              paste0(errors, "-zip"),
            TRUE ~ errors)
      ) %>%
      select(-zipPrefix)

    return(corrproof_x)

  }
