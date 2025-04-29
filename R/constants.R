#' @importFrom rlang expr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr tibble
#' @importFrom lubridate ymd

# Define variables to evaluate data consistently across functions

# Define inline permits that did not hunt
LOGIC_INLINE_PMT_DNH <-
  expr(
    dl_state %in% c("OR", "WA") &
      hunt_mig_birds != "2" &
      sum(as.numeric(band_tailed_pigeon),
          as.numeric(brant),
          as.numeric(seaducks)) >= 2)

# List of Harvest Information Program species/species group fields containing
# bag values
REF_BAG_FIELDS <-
  c("ducks_bag", "geese_bag", "dove_bag", "woodcock_bag", "coots_snipe",
    "rails_gallinules", "cranes", "band_tailed_pigeon", "brant", "seaducks")

# US District and Territory abbreviations:
# District of Columbia, American Samoa, Guam, Northern Mariana Islands,
# Puerto Rico, Virgin Islands, US Minor Outlying Islands, Marshall Islands,
# Micronesia, Palau, Armed Forces (Americas), Armed Forces (Europe), Armed
# Forces (Pacific)
REF_ABBR_USA <-
  c("DC", "AS", "GU", "MP", "PR", "VI", "UM", "MH", "FM", "PW", "AA", "AE",
    "AP")

# Canada abbreviations:
# Alberta, British Columbia, Manitoba, New Brunswick, Newfoundland and
# Labrador, Nova Scotia, Northwest Territories, Nunavut, Ontario, Prince
# Edward Island, Province du Québec, Quebec, Saskatchewan, Yukon
REF_ABBR_CANADA <-
  c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "PQ", "QC",
    "SK", "YT")

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

