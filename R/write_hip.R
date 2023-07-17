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
#' @param split Split the output into one .csv file per .txt file? Default is TRUE.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/migbirdHIP}
#'
#' @export

write_hip <-
  function(x, path, split = TRUE){

    # Add a final "/" if not included already
    if(!str_detect(path, "\\/$")) {
      path <- paste0(path, "/")
    }

    bagfields <-
      names(x)[match("ducks_bag", names(x)):match("seaducks", names(x))]

    stratanames <-
      c("S_ducks", "S_geese", "S_doves", "S_woodcock", "S_coot_snipe",
        "S_rail_gallinule", "S_cranes", "S_bt_pigeons", "S_brant", "S_seaducks")

    # Generate a list of translated bags for each species/species group
    bag_translations <-
      map(
        1:length(bagfields),
        ~hip_bags_ref |>
          filter(spp == bagfields[.x]) |>
          mutate(!!sym(bagfields[.x]) := as.character(stateBagValue)) |>
          select(-c("stateBagValue", "spp")) |>
          rename(
            dl_state = state,
            !!sym(stratanames[.x]) := FWSstratum)
      )

    # Left join all the bag translations to the corrected data
    for(i in 1:length(bagfields)) {

      x <-
        x |>
        left_join(
          bag_translations[[i]],
          by = c("dl_state", bagfields[[i]])
        )
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
      mutate(
        # Only include file names in the "source_file" field, not folder names.
        # The field theoretically shouldn't include DL folder names if the
        # package functions are run on a download-by-download basis, but this
        # will tidy the field values just in case it's needed
        source_file =
          ifelse(
            str_detect(source_file, "\\/"),
            str_remove(source_file, "^.+(?=\\/)"),
            source_file)) |>
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
                   final_list[[.x]] |>
                     select(source_file) |>
                     distinct() |>
                     pull()),
            ".txt$",
            ".csv"),
          na = ""))
    }else{
      # Write data to a single csv
      fwrite(
        final_table,
        file = path,
        na = "")
    }

  }
