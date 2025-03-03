#' Modify corrected data table format and write as csv
#'
#' After correcting errors in the data with \code{\link{correct}}, this final step will shape up the dataframe into a format ready for the database, and write the data to csv.
#'
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom rlang sym
#' @importFrom rlang :=
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom purrr walk
#' @importFrom purrr map
#' @importFrom data.table fwrite
#'
#' @param x The object created after correcting data with \code{\link{correct}}
#' @param path The file path and file name to write the final table
#' @param type The type of HIP file being written out, one of: "HIP", "BT", or "CR"
#' @param split Split the output into one .csv file per .txt file? Default is TRUE.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

write_hip <-
  function(x, path, type, split = TRUE){

    # Add a final "/" if not included already
    if(!str_detect(path, "\\/$")) {
      path <- paste0(path, "/")
    }

    # Fail if incorrect split supplied
    stopifnot(
      "Error: Please supply TRUE or FALSE for `split` parameter." =
        split %in% c(TRUE, FALSE, T, F))

    # Fail if incorrect type supplied
    stopifnot(
      "Error: Please supply one of 'HIP', 'BT', or 'CR' for `type` parameter." =
        type %in% c("HIP", "BT", "CR"))

    # Fail if HIP file does not contain record_type = HIP
    if (type == "HIP") {
      stopifnot(
        "Error: HIP files must contain record_type = HIP." =
          unique(x$record_type) == "HIP")
    }

    if (type == "BT") {
      # Fail if BT permit file does not contain record_type = HIP
      stopifnot(
        "Error: BTPI permit files must contain record_type = HIP." =
          unique(x$record_type) == "HIP")

      # Fail if BT permit file does not contain 2 for BT
      stopifnot(
        "Error: BTPI permit files must contain band_tailed_pigeon = 2." =
          unique(x$band_tailed_pigeon) == "2")

      # Fail if BT permit file does not contain non-0 values for DV
      stopifnot(
        "Error: BTPI permit files must have values other than 0 in dove_bag." =
          length(unique(x$dove_bag)) > 1)

      # Fail if any bag field other than BT or DV is not 0
      stopifnot(
        "Error: Ducks bag must be 0 in BTPI permit files." =
          unique(x$ducks_bag) == 0)
      stopifnot(
        "Error: Geese bag must be 0 in BTPI permit files." =
          unique(x$geese_bag) == 0)
      stopifnot(
        "Error: Woodcock bag must be 0 in BTPI permit files." =
          unique(x$woodcock_bag) == 0)
      stopifnot(
        "Error: Coots_snipe bag must be 0 in BTPI permit files." =
          unique(x$coots_snipe) == 0)
      stopifnot(
        "Error: Rails_gallinules bag must be 0 in BTPI permit files." =
          unique(x$rails_gallinules) == 0)
      stopifnot(
        "Error: Cranes bag must be 0 in BTPI permit files." =
          unique(x$cranes) == 0)
      stopifnot(
        "Error: Brant bag must be 0 in BTPI permit files." =
          unique(x$brant) == 0)
      stopifnot(
        "Error: Sea ducks bag must be 0 in BTPI permit files." =
          unique(x$seaducks) == 0)
    }

    if (type == "CR") {
      # Fail if CR permit file does not contain record_type = PMT
      stopifnot(
        "Error: CR permit files must contain record_type = PMT." =
          unique(x$record_type) == "PMT")

      # Fail if CR permit file does not contain 2 for CR
      stopifnot(
        "Error: CR permit files must contain cranes = 2." =
          unique(x$cranes) == "2")

      # Fail if any bag field other than CR is not 0
      stopifnot(
        "Error: Ducks bag must be 0 in CR permit files." =
          unique(x$ducks_bag) == 0)
      stopifnot(
        "Error: Geese bag must be 0 in CR permit files." =
          unique(x$geese_bag) == 0)
      stopifnot(
        "Error: Dove bag must be 0 in CR permit files." =
          unique(x$dove_bag) == 0)
      stopifnot(
        "Error: Woodcock bag must be 0 in CR permit files." =
          unique(x$woodcock_bag) == 0)
      stopifnot(
        "Error: Coots_snipe bag must be 0 in CR permit files." =
          unique(x$coots_snipe) == 0)
      stopifnot(
        "Error: Rails_gallinules bag must be 0 in CR permit files." =
          unique(x$rails_gallinules) == 0)
      stopifnot(
        "Error: Band tailed pigeon bag must be 0 in CR permit files." =
          unique(x$band_tailed_pigeon) == 0)
      stopifnot(
        "Error: Brant bag must be 0 in CR permit files." =
          unique(x$brant) == 0)
      stopifnot(
        "Error: Sea ducks bag must be 0 in CR permit files." =
          unique(x$seaducks) == 0)
    }

    stratanames <-
      c("S_ducks", "S_geese", "S_doves", "S_woodcock", "S_coot_snipe",
        "S_rail_gallinule", "S_cranes", "S_bt_pigeons", "S_brant", "S_seaducks")

    # Generate a list of translated bags for each species/species group
    bag_translations <-
      map(
        1:length(ref_bagfields),
        ~hip_bags_ref |>
          filter(spp == ref_bagfields[.x]) |>
          mutate(!!sym(ref_bagfields[.x]) := as.character(stateBagValue)) |>
          select(-c("stateBagValue", "spp")) |>
          rename(
            dl_state = state,
            !!sym(stratanames[.x]) := FWSstratum)
      )

    # Left join all the bag translations to the corrected data
    for(i in 1:length(ref_bagfields)) {
      x <-
        x |>
        left_join(
          bag_translations[[i]],
          by = c("dl_state", ref_bagfields[i])
        )
    }

    # Generate a list of zeros for each no-season state/species
    zero_translations <-
      map(
        1:length(ref_bagfields),
        ~hip_bags_ref |>
          select(-stateBagValue) |>
          group_by(state, spp) |>
          filter(n() == 1) |>
          ungroup() |>
          filter(spp == ref_bagfields[.x]) |>
          mutate(!!sym(stratanames[.x]) := NA) |>
          select(-spp) |>
          rename(
            dl_state = state,
            !!sym(paste0(stratanames[.x], "_0s")) := FWSstratum)
      )

    # If a season doesn't exist, make sure the translation is 0 (not NA)
    for(i in 1:length(ref_bagfields)) {
      if(nrow(zero_translations[[i]]) > 0) {
        x <-
          x |>
          left_join(
            zero_translations[[i]],
            by = c("dl_state", stratanames[i])) |>
          mutate(
            !!sym(stratanames[i]) :=
              ifelse(
                is.na(!!sym(paste0(stratanames[i]))) &
                  !is.na(!!sym(paste0(stratanames[i], "_0s"))),
                !!sym(paste0(stratanames[i], "_0s")),
                !!sym(paste0(stratanames[i]))
              )
          ) |>
          select(-!!sym(paste0(stratanames[i], "_0s")))
      }
    }

    # Generate the polished output table
    final_table <-
      x |>
      # Exclude unwanted columns
      select(-c("dl_date", "dl_key", "record_key", "errors")) |>
      # Rename columns to desired output
      rename(
        dl = dl_cycle,
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
        Q_seaducks = seaducks) |>
      # Only include file names in the "source_file" field, not folder names.
      # The field theoretically shouldn't include DL folder names if the package
      # functions are run on a download-by-download basis, but this will tidy
      # the field values just in case it's needed
      mutate(
        source_file =
          ifelse(
            str_detect(source_file, "\\/"),
            str_remove(source_file, "^.+(?=\\/)"),
            source_file)) |>
      # Remove the last "/" (couldn't pipe a dot above)
      mutate(source_file = str_remove(source_file, "\\/"))

    if(split == TRUE) {
      # Split data and write each input file to its own output file
      final_list <- split(final_table, f = final_table$source_file)
      rm(final_table)

      walk(
        1:length(final_list),
        ~fwrite(
          final_list[[.x]],
          file =
            str_replace(
              paste0(path,
                     final_list[[.x]] |>
                       distinct(source_file) |>
                       pull()),
              "\\.(txt|xlsx|xls)$",
              ".csv"),
          na = ""))
    } else {
      # Write data to a single csv
      fwrite(
        final_table,
        file = path,
        na = "")
    }

  }
