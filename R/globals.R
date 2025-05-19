#' @importFrom utils globalVariables

globalVariables(
  c(
    # Field names of HIP data
    c(REF_HUNTER_ID_FIELDS,
      "title",
      "middle",
      "suffix",
      "address",
      "city",
      "zip",
      "issue_date",
      "hunt_mig_birds",
      REF_BAG_FIELDS,
      "email"),
    # Field names in migbirdHIP:::REF_BAGS
    c("stateBagValue",
      "spp",
      "FWSstratum"),
    # Field names in migbirdHIP:::REF_DATES
    c("last_day_migbird_hunting",
      "issue_start",
      "issue_end",
      "category"),
    # Field names in migbirdHIP:::REF_ZIP_CODE
    "zipcode",
    # Used in errorPlots.R
    c("count_correct",
      "count_errors",
      "total"),
    # Used in read_hip.R
    c("filepath",
      "n_emails",
      "dl_date",
      "dl_cycle",
      "record_key",
      "source_file",
      "n_total",
      "check"),
    # Used in files.R
    c("filename",
      "jdate",
      "old"),
    # Used in glyphs.R
    c("field",
      "check"),
    # Used in shifts.R
    c("n_shift",
      "shifted"),
    # Used in issuance.R
    c("orig_yr",
      "eval_yr",
      "xmin",
      "xmax",
      "ymin",
      "ymax",
      "bags",
      "season"),
    # Used in bags.R
    c("bad_bag_value",
      "bad_bag_values",
      "expected_bag_value",
      "n_bad_bags",
      "n_state"),
    # Used in duplicates.R
    c("hunter_key",
      "duplicate_id",
      "all_ones",
      "duplicate_field",
      "sd_or_br_has_2",
      "decision",
      "x_issue_date",
      "record_type",
      "total_count",
      "fields",
      "field_name"),
    # Used in proof.R
    c("temp_key",
      "error"),
    # Used in errorTables.R
    "error_count",
    # Used in redFlags()
    "flag",
    # Multiple use
    c("name",
      "value",
      "proportion",
      "total_records",
      "zipState",
      "errors")
  )
)
