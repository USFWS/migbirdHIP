#' Modify corrected data table format and write as csv
#'
#' After correcting errors in the data with \code{\link{correct}}, this final step will shape up the dataframe into a format ready for the database, and write the data to csv.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom readr write_csv
#' @importFrom purrr walk
#' @importFrom data.table fwrite
#'
#' @param x The object created after correcting data with \code{\link{correct}}
#' @param path The file path and file name to write the final table
#' @param split Split the output into one .csv file per .txt file? Default is TRUE.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

write_hip <-
  function(x, path, split = TRUE){

    final_table <-
      x %>%
      # Exclude unwanted columns
      select(-c("dl_date", "dl_key", "record_key", "errors")) %>%
      # Use internal data from hip_bags_ref to join FWS strata,
      # Add strata: ducks
      left_join(
        hip_bags_ref %>%
          mutate(ducks_bag = as.character(stateBagValue)) %>%
          filter(spp == "ducks_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "ducks_bag")
      ) %>%
      rename(
        S_ducks = FWSstratum,
        dl = dl_cycle) %>%
      # Add strata: geese
      left_join(
        hip_bags_ref %>%
          mutate(geese_bag = as.character(stateBagValue)) %>%
          filter(spp == "geese_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "geese_bag")
      ) %>%
      rename(S_geese = FWSstratum) %>%
      # Add strata: dove
      left_join(
        hip_bags_ref %>%
          mutate(dove_bag = as.character(stateBagValue)) %>%
          filter(spp == "dove_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "dove_bag")
      ) %>%
      rename(S_doves = FWSstratum) %>%
      # Add strata: woodcock
      left_join(
        hip_bags_ref %>%
          mutate(woodcock_bag = as.character(stateBagValue)) %>%
          filter(spp == "woodcock_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "woodcock_bag")
      ) %>%
      rename(S_woodcock = FWSstratum) %>%
      # Add strata: coots and snipe
      left_join(
        hip_bags_ref %>%
          mutate(coots_snipe = as.character(stateBagValue)) %>%
          filter(spp == "coots_snipe_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "coots_snipe")
      ) %>%
      rename(S_coot_snipe = FWSstratum) %>%
      # Add strata: rails and gallinules
      left_join(
        hip_bags_ref %>%
          mutate(rails_gallinules = as.character(stateBagValue)) %>%
          filter(spp == "rails_gallinules_bag") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "rails_gallinules")
      ) %>%
      rename(S_rail_gallinule = FWSstratum) %>%
      # Add strata: cranes
      left_join(
        hip_bags_ref %>%
          mutate(cranes = as.character(stateBagValue)) %>%
          filter(spp == "cranes") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "cranes")
      ) %>%
      rename(S_cranes = FWSstratum) %>%
      # Add strata: band-tailed pigeon
      left_join(
        hip_bags_ref %>%
          mutate(band_tailed_pigeon = as.character(stateBagValue)) %>%
          filter(spp == "band_tailed_pigeon") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "band_tailed_pigeon")
      ) %>%
      rename(S_bt_pigeons = FWSstratum) %>%
      # Add strata: brant
      left_join(
        hip_bags_ref %>%
          mutate(brant = as.character(stateBagValue)) %>%
          filter(spp == "brant") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "brant")
      ) %>%
      rename(S_brant = FWSstratum) %>%
      # Add strata: seaducks
      left_join(
        hip_bags_ref %>%
          mutate(seaducks = as.character(stateBagValue)) %>%
          filter(spp == "seaducks") %>%
          select(-c("stateBagValue", "spp")),
        by = c("state", "seaducks")
      ) %>%
      rename(S_seaducks = FWSstratum) %>%
      # Rename columns to desired output
      rename(
        postal_code = zip,
        Q_ducks = ducks_bag,
        Q_geese = geese_bag,
        Q_doves = dove_bag,
        Q_woodcock = woodcock_bag,
        Q_coot_snipe = coots_snipe,
        Q_rail_gallinule = rails_gallinules,
        Q_cranes = cranes,
        Q_bt_pigeons = band_tailed_pigeon,
        Q_brant = brant,
        Q_seaducks = seaducks) %>%
      mutate(
        # Only include file names in the "source_file" field, not folder names.
        # The field theoretically shouldn't include DL folder names if the
        # package functions are run on a download-by-download basis, but this
        # will tidy the field values just in case it's needed
        source_file =
          ifelse(
            str_detect(source_file, "\\/"),
            str_remove(source_file, "^.+(?=\\/)"),
            source_file)) %>%
      mutate(
        # Do one last pass through source_file to remove the last "/", couldn't
        # pipe a dot above
        source_file = str_remove(source_file, "\\/"))

    if(split == TRUE){
      # Split data and write each input file to its own output file
      final_list <- split(final_table, f = final_table$source_file)
      rm(final_table)

      walk(
        1:length(final_list),
        ~fwrite(
          final_list[[.x]],
          file = str_replace(
            paste0(path,
                   final_list[[.x]] %>%
                     select(source_file) %>%
                     distinct() %>%
                     pull()),
            ".txt$",
            ".csv"),
          na = ""))
    }else{
      # Write data to csv
      fwrite(
        final_table,
        file = path,
        na = "")
    }

  }
