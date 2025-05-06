#' @importFrom rlang expr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr tibble
#' @importFrom dplyr if_any
#' @importFrom dplyr if_all
#' @importFrom dplyr all_of
#' @importFrom stringr str_detect
#' @importFrom rlang syms

# Define variables to evaluate data consistently across functions


# Vector of Harvest Information Program species/species group fields containing
# bag values
REF_BAG_FIELDS <-
  c("ducks_bag", "geese_bag", "dove_bag", "woodcock_bag", "coots_snipe",
    "rails_gallinules", "cranes", "band_tailed_pigeon", "brant", "seaducks")

REF_STRATA_NAMES <-
  c("S_ducks", "S_geese", "S_doves", "S_woodcock", "S_coot_snipe",
    "S_rail_gallinule", "S_cranes", "S_bt_pigeons", "S_brant", "S_seaducks")

# Vector of fields that are used to deduplicate hunters
REF_HUNTER_ID_FIELDS <-
  c("firstname", "lastname", "state", "birth_date", "dl_state",
    "registration_yr")

# US District and Territory abbreviations:
# District of Columbia, American Samoa, Guam, Northern Mariana Islands,
# Puerto Rico, Virgin Islands, US Minor Outlying Islands, Marshall Islands,
# Micronesia, Palau, Armed Forces (Americas), Armed Forces (Europe), Armed
# Forces (Pacific)
REF_ABBR_USA <-
  c("DC", "AS", "GU", "MP", "PR", "VI", "UM", "MH", "FM", "PW", "AA", "AE",
    "AP")

# Canada abbreviations: Alberta, British Columbia, Manitoba, New Brunswick,
# Newfoundland and Labrador, Nova Scotia, Northwest Territories, Nunavut,
# Ontario, Prince Edward Island, Province du Québec, Quebec, Saskatchewan,
# Yukon; Used by the download report template
REF_ABBR_CANADA <-
  c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "PQ", "QC",
    "SK", "YT")

# Continental 49 state abbreviations; Used by read_hip() and the download report
REF_ABBR_49_STATES <- datasets::state.abb[datasets::state.abb != "HI"]

# Permit state expected bag values (files received separately from HIP process)
REF_PMT_FILES <-
  bind_rows(
    # Crane permit file states
    tibble(
      dl_state = c("CO", "KS", "MN", "MT", "ND", "NM", "OK", "TX", "WY"),
      spp = "cranes",
      value = "0"),
    # Band-tailed Pigeon permit file states
    tibble(
      dl_state = c("CO", "NM", "UT"),
      spp = "band_tailed_pigeon",
      value = "0")
  )

# Permit states with in-line permit bag values
REF_PMT_INLINE <-
  bind_rows(
    # Washington in-line permits expected for BTPI, brant, and sea ducks
    tibble(
      dl_state = "WA",
      spp = c("band_tailed_pigeon", "brant", "seaducks"),
      value = "2"),
    # Oregon in-line permits expected for BTPI, brant, and sea ducks
    tibble(
      dl_state = "OR",
      spp = c("band_tailed_pigeon", "brant", "seaducks"),
      value = "2")
  )

# Define inline permits
LOGIC_INLINE_PMT <-
  expr(
    dl_state %in% c("OR", "WA") &
      ducks_bag == "0" &
      geese_bag == "0" &
      dove_bag == "0" &
      woodcock_bag == "0" &
      coots_snipe == "0" &
      rails_gallinules == "0" &
      (band_tailed_pigeon == "2" |
         brant == "2" |
         seaducks == "2"))

# Define inline permits that did not hunt
LOGIC_INLINE_PMT_DNH <-
  expr(!!LOGIC_INLINE_PMT & hunt_mig_birds != "2")

# Define a test record
LOGIC_TEST_RECORD <-
  expr(firstname == "TEST" & lastname == "TEST")

# Define non-digit bag records; used by read_hip() and clean()
LOGIC_NONDIGIT_BAGS <-
  expr(if_any(all_of(REF_BAG_FIELDS), \(x) !str_detect(x, "^[0-9]{1}$")))

# Define all-zero bag records; used by read_hip() and clean()
LOGIC_ZERO_BAGS <-
  expr(if_all(all_of(REF_BAG_FIELDS), \(x) x == "0"))

# Define missing personal information; used by read_hip() and missingPIIFilter()
LOGIC_MISSING_PII <-
  expr(if_any(c("firstname", "lastname", "state", "birth_date"), \(x) is.na(x)))

# Define missing address and email; used by read_hip() and missingPIIFilter()
LOGIC_MISSING_ADDRESSES <-
  expr(if_all(c("address", "email"), \(x) is.na(x)))

# Define missing elements of a physical address that are required to determine
# where to mail a letter; used by read_hip() and missingPIIFilter()
LOGIC_MISSING_CITY_ZIP_EMAIL <-
  expr(if_all(c("city", "zip", "email"), \(x) is.na(x)))

# Define a trailing suffix using regular expressions (e.g., suffix included in
# the firstname or lastname field). Includes values from 1-20 in Roman numerals
# and numeric, excluding XVIII (limit is 4 characters)
REGEX_SUFFIX_SEARCH <-
  paste0(
    "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI",
    "{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$")

# Define suffixes using regular expressions. Includes values from 1-20 in Roman
# numerals and numeric, excluding XVIII (limit is 4 characters)
REGEX_SUFFIX_EXACT <-
  paste0(
    "^[JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI{0,1}X|1ST|",
    "2ND|3RD|[4-9]TH|1[0-9]TH|20TH]$")

# Regular expression that selects non-permit species bag fields
REGEX_NON_PMT_SPECIES <- "bag|coots|rails"

# Combine US State, District and Territory abbreviations with Canadian Province
# and Territory abbreviations; used by proof()
REGEX_USA_CANADA <-
  paste(
    c(datasets::state.abb, REF_ABBR_USA, REF_ABBR_CANADA),
    collapse = "|")
