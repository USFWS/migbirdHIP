#' Modify corrected data table format and write as csv
#'
#' After correcting errors in the data with \code{\link{correct}}, this final
#' step will shape up the dataframe into a format ready for the database, and
#' write the data to csv.
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
#' @importFrom rlang .data
#'
#' @param corrected_data The object created after correcting data with
#'   \code{\link{correct}}
#' @param path The file path and file name to write the final table
#' @param type The type of HIP file being written out, one of: "HIP", "BT", or
#'   "CR"
#' @param split Split the output into one .csv file per .txt file? Default is
#'   TRUE.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

write_hip <-
  function(corrected_data, path, type, split = TRUE) {
    failProofed(corrected_data)
    failTF(split)

    # Add a final "/" if not included already
    if (!str_detect(path, "\\/$")) {
      path <- paste0(path, "/")
    }

    # Fail if incorrect type supplied
    stopifnot(
      "Error: Please supply one of 'HIP', 'BT', or 'CR' for `type` parameter." =
        type %in% c("HIP", "BT", "CR"))

    # Fail if HIP file does not contain record_type = HIP
    if (type == "HIP") {
      stopifnot(
        "Error: HIP files must contain record_type = HIP." =
          unique(corrected_data$record_type) == "HIP")
    }

    if (type == "BT") {
      # Fail if BT permit file does not contain record_type = HIP
      stopifnot(
        "Error: BTPI permit files must contain record_type = HIP." =
          unique(corrected_data$record_type) == "HIP")

      # Fail if BT permit file does not contain 2 for BT
      stopifnot(
        "Error: BTPI permit files must contain band_tailed_pigeon = 2." =
          unique(corrected_data$band_tailed_pigeon) == "2")

      # Fail if BT permit file does not contain non-0 values for DV
      stopifnot(
        "Error: BTPI permit files must have values other than 0 in dove_bag." =
          length(unique(corrected_data$dove_bag)) > 1)

      # Fail if any bag field other than BT or DV is not 0
      stopifnot(
        "Error: Ducks bag must be 0 in BTPI permit files." =
          unique(corrected_data$ducks_bag) == 0)
      stopifnot(
        "Error: Geese bag must be 0 in BTPI permit files." =
          unique(corrected_data$geese_bag) == 0)
      stopifnot(
        "Error: Woodcock bag must be 0 in BTPI permit files." =
          unique(corrected_data$woodcock_bag) == 0)
      stopifnot(
        "Error: Coots_snipe bag must be 0 in BTPI permit files." =
          unique(corrected_data$coots_snipe) == 0)
      stopifnot(
        "Error: Rails_gallinules bag must be 0 in BTPI permit files." =
          unique(corrected_data$rails_gallinules) == 0)
      stopifnot(
        "Error: Cranes bag must be 0 in BTPI permit files." =
          unique(corrected_data$cranes) == 0)
      stopifnot(
        "Error: Brant bag must be 0 in BTPI permit files." =
          unique(corrected_data$brant) == 0)
      stopifnot(
        "Error: Sea ducks bag must be 0 in BTPI permit files." =
          unique(corrected_data$seaducks) == 0)
    }

    if (type == "CR") {
      # Fail if CR permit file does not contain record_type = PMT
      stopifnot(
        "Error: CR permit files must contain record_type = PMT." =
          unique(corrected_data$record_type) == "PMT")

      # Fail if CR permit file does not contain 2 for CR
      stopifnot(
        "Error: CR permit files must contain cranes = 2." =
          unique(corrected_data$cranes) == "2")

      # Fail if any bag field other than CR is not 0
      stopifnot(
        "Error: Ducks bag must be 0 in CR permit files." =
          unique(corrected_data$ducks_bag) == 0)
      stopifnot(
        "Error: Geese bag must be 0 in CR permit files." =
          unique(corrected_data$geese_bag) == 0)
      stopifnot(
        "Error: Dove bag must be 0 in CR permit files." =
          unique(corrected_data$dove_bag) == 0)
      stopifnot(
        "Error: Woodcock bag must be 0 in CR permit files." =
          unique(corrected_data$woodcock_bag) == 0)
      stopifnot(
        "Error: Coots_snipe bag must be 0 in CR permit files." =
          unique(corrected_data$coots_snipe) == 0)
      stopifnot(
        "Error: Rails_gallinules bag must be 0 in CR permit files." =
          unique(corrected_data$rails_gallinules) == 0)
      stopifnot(
        "Error: Band tailed pigeon bag must be 0 in CR permit files." =
          unique(corrected_data$band_tailed_pigeon) == 0)
      stopifnot(
        "Error: Brant bag must be 0 in CR permit files." =
          unique(corrected_data$brant) == 0)
      stopifnot(
        "Error: Sea ducks bag must be 0 in CR permit files." =
          unique(corrected_data$seaducks) == 0)
    }

    # Generate a list of translated bags for each species/species group
    bag_translations <-
      map(
        seq_along(REF_BAG_FIELDS),
        \(x) REF_BAGS |>
          filter(.data$spp == REF_BAG_FIELDS[x]) |>
          mutate(
            !!sym(REF_BAG_FIELDS[x]) := as.character(.data$stateBagValue)) |>
          select(-c("stateBagValue", "spp")) |>
          rename(
            dl_state = .data$state,
            !!sym(REF_STRATA_NAMES[x]) := .data$FWSstratum)
      )

    # Left join all the bag translations to the corrected data
    for (i in seq_along(REF_BAG_FIELDS)) {
      corrected_data <-
        corrected_data |>
        left_join(
          bag_translations[[i]],
          by = c("dl_state", REF_BAG_FIELDS[i])
        )
    }

    # Generate a list of zeros for each no-season state/species
    zero_translations <-
      map(
        seq_along(REF_BAG_FIELDS),
        \(x) REF_BAGS |>
          select(-"stateBagValue") |>
          group_by(.data$state, .data$spp) |>
          filter(n() == 1) |>
          ungroup() |>
          filter(.data$spp == REF_BAG_FIELDS[x]) |>
          mutate(!!sym(REF_STRATA_NAMES[x]) := NA) |>
          select(-"spp") |>
          rename(
            dl_state = .data$state,
            !!sym(paste0(REF_STRATA_NAMES[x], "_0s")) := .data$FWSstratum)
      )

    # If a season doesn't exist, make sure the translation is 0 (not NA)
    for (i in seq_along(REF_BAG_FIELDS)) {
      if (nrow(zero_translations[[i]]) > 0) {
        corrected_data <-
          corrected_data |>
          left_join(
            zero_translations[[i]],
            by = c("dl_state", REF_STRATA_NAMES[i])) |>
          mutate(
            !!sym(REF_STRATA_NAMES[i]) :=
              ifelse(
                is.na(!!sym(paste0(REF_STRATA_NAMES[i]))) &
                  !is.na(!!sym(paste0(REF_STRATA_NAMES[i], "_0s"))),
                !!sym(paste0(REF_STRATA_NAMES[i], "_0s")),
                !!sym(paste0(REF_STRATA_NAMES[i]))
              )
          ) |>
          select(-!!sym(paste0(REF_STRATA_NAMES[i], "_0s")))
      }
    }

    # Generate the polished output table
    final_table <-
      corrected_data |>
      # Exclude unwanted columns
      select(-c("dl_date", "dl_key", "record_key", "errors")) |>
      # Rename columns to desired output
      rename(
        dl = .data$dl_cycle,
        postal_code = .data$zip,
        Q_ducks = .data$ducks_bag,
        Q_geese = .data$geese_bag,
        Q_doves = .data$dove_bag,
        Q_woodcock = .data$woodcock_bag,
        Q_coot_snipe = .data$coots_snipe,
        Q_rail_gallinule = .data$rails_gallinules,
        Q_cranes = .data$cranes,
        Q_bt_pigeons = .data$band_tailed_pigeon,
        Q_brant = .data$brant,
        Q_seaducks = .data$seaducks) |>
      # Only include file names in the "source_file" field, not folder names.
      # The field theoretically shouldn't include DL folder names if the package
      # functions are run on a download-by-download basis, but this will tidy
      # the field values just in case it's needed
      mutate(
        source_file =
          ifelse(
            str_detect(.data$source_file, "\\/"),
            str_remove(.data$source_file, "^.+(?=\\/)"),
            .data$source_file)) |>
      # Remove the last "/" (couldn't pipe a dot above)
      mutate(source_file = str_remove(.data$source_file, "\\/"))

    if (split == TRUE) {
      # Split data and write each input file to its own output file
      final_list <- split(final_table, f = final_table$source_file)
      rm(final_table)

      walk(
        seq_along(final_list),
        \(x) fwrite(
          final_list[[x]],
          file =
            str_replace(
              paste0(path,
                     final_list[[x]] |>
                       distinct(.data$source_file) |>
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
