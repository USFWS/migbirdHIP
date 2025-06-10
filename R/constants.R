#' @importFrom rlang expr
#' @importFrom dplyr bind_rows
#' @importFrom dplyr tibble
#' @importFrom dplyr if_any
#' @importFrom dplyr if_all
#' @importFrom dplyr all_of
#' @importFrom stringr str_detect
#' @importFrom rlang syms

# Define variables to evaluate data consistently across functions

# Vector of state abbreviations (NOT INCLUDING Oregon or Washington) that have a
# hunting season for both sea ducks AND brant
REF_STATES_SD_BR <-
  c("AK", "CA", "CT", "DE", "MA", "MD", "NC", "NH", "NJ", "NY", "RI", "VA")

# Maine is currently the only state with a sea duck hunting season but not a
# brant hunting season
REF_STATES_SD_ONLY <- "ME"

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

# Combine US State (include HI because this is for addresses), District and
# Territory abbreviations with Canadian Province and Territory abbreviations;
# used by proof()
REF_USA_CANADA <- c(datasets::state.abb, REF_ABBR_USA, REF_ABBR_CANADA)

# Define suffixes. Accepted values include 1-20 in Roman numerals and ordinal
# values, excluding XVIII (database limit is 4 characters)
REF_ROMAN_SUFFIXES <-
  c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X",
    "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XIX", "XX")

# Define suffixes spanning 1ST-20TH
REF_ORDINAL_SUFFIXES <- c("1ST", "2ND", "3RD", paste0(c(4:20), "TH"))

# Define junior and senior suffixes
REF_CATEGORICAL_SUFFIXES <- c("JR", "SR")

# Combine roman numeral and ordinal suffixes into one object
REF_SUFFIXES <-
  c(REF_ROMAN_SUFFIXES, REF_ORDINAL_SUFFIXES, REF_CATEGORICAL_SUFFIXES)

# Define titles. Expected values are "1" and "2", but we also accept "0" and NA
REF_TITLES <- c(NA, "0", "1", "2")

# Define valid hunt_mig_birds values; only "1" and "2" are accepted, although
# "2" is the desired/expected value
REF_HUNT_MIG_BIRDS <- c("1", "2")

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
  expr(
    (firstname == "TEST" & lastname == "TEST") |
      lastname == "INAUDIBLE" |
      str_detect(firstname, "^(INAUDIBLE|BLANK|USER|TEST|RESIDENT)$"))

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

# Regular expression that selects non-permit species bag fields
REGEX_NON_PMT_SPECIES <- "bag|coots|rails"

# First name regular expression; may contain apostrophe, space, hyphen (none of
# which consecutive) and 2+ capital letters
REGEX_FIRSTNAME <-
  paste0(
    "^(?!.*\\'\\')(?!.*\\s\\s)(?!.*\\-\\-)(?!.*\\sAKA\\s.*)[A-Z+\\'?]\\-?\\s?\\",
    "'?[A-Z\\-?\\s?\\'?]*[A-Z]+$")

# Last name regular expression; may contain apostrophe, space, hyphen, period,
# (none of which consecutive) and 2+ capital letters
REGEX_LASTNAME <-
  paste0(
    "^(?!.*\\'\\')(?!.*\\s\\s)(?!.*\\-\\-)(?!.*\\.\\.)(?!.*\\.[A-Z\\s]*\\.)",
    "[A-Z]\\-?\\s?\\'?[A-Z\\-?\\s?\\'?\\.?]*[A-Z]+$")

# Define a trailing suffix using regular expressions (e.g., suffix included in
# the firstname or lastname field). Includes values from 1-20 in Roman numerals
# and numeric
REGEX_SUFFIX_SEARCH <-
  paste0(
    "(?<=\\s)(JR|SR|I{1,3}|IV|VI{0,3}|I{0,1}X|XI{1,3}|XI{0,1}V|XVI{1,2}|XI",
    "{0,1}X|1ST|2ND|3RD|[4-9]TH|1[0-9]TH|20TH)\\.?$")

# Address regular expression for unwanted symbols
REGEX_BAD_ADDRESS <- "\\||\\t|[^\\x00-\\x7F]+"

# Regular expression for correct city name values. City names should only
# contain letters, spaces (e.g., New York City, NY), hyphens (e.g.,
# Winston-Salem, NC), apostrophes (e.g., O'Fallon, MO), and/or periods (e.g.,
# St. Augustine, FL)
REGEX_CITY <-
  paste0(
    "^(?!.*\\'\\')(?!.*\\s\\s)(?!.*\\-\\-)(?!.*\\.\\.)(?!.*\\.[A-Za-z\\s]*\\.)",
    "(?=.*[A-Za-z]{3,}.*)[A-Za-z]\\-?\\s?\\'?[A-Za-z\\-?\\s?\\'?\\.?]*[A-Za-z]",
    "+$")

# Regular expression for the expected email format. Local part may contain Latin
# lower and uppercase letters, numbers, underscores, dots, hyphens, and/or plus
# signs; must contain an @; domain may contain Latin lower and uppercase
# letters, numbers, and hyphens; subdomains acceptable when separated by a dot.
# The first character in the local part must be a letter or number, and a dot
# may not occur as the last character in the local part or last character of the
# entire address. Sequential dots are not allowed. A hyphen is not allowed to be
# the first character in the domain. Length required is a minimum of 6
# characters (e.g., a@b.io). Other ASCII characters that are theoretically
# allowed in email addresses are not valid in HIP data (e.g.,
# !#$%&'*/=?^_`{|}~).
REGEX_EMAIL <-
  paste0(
    "^(?!.+\\.\\@.+)(?!.+\\.$)(?!.+\\.\\.+.+)(?!.+\\@\\-.+)(?=.+[a-zA-Z]{2,}$)",
    "[a-zA-Z0-9]+[a-zA-Z0-9\\_\\.\\+\\-]*\\@[a-zA-Z0-9\\-]+\\.[a-zA-Z0-9\\-\\.",
    "]+$")

# Regular expression for an obfuscative email address local-part
REGEX_EMAIL_OBFUSCATIVE_LOCALPART <-
  paste0(
    "^((none)+|(none)+[0-9]+|[0-9]+(none)+|",
    "none+|",
    "o{4,100}|",
    "n{4,100}|",
    "e{4,100}|",
    "noen|nnon|onoene|nnnnnne|nonen|nonoe|nnoe|onoennoneo|nonne|nnone|nnno|",
    "onon|onoe|nonn|noeone|nonneee|nonnee|nonoeone|onne|nononon|oonono|onoen|",
    "noneo|",
    "www\\.no|www\\.none|",
    "(nope)+|(nope)+[0-9]+|[0-9]+(nope)+|",
    "(null)+|(null)+[0-9]+|[0-9]+(null)+|",
    "(no)+|(no)+[0-9]+|[0-9]+(no)+|",
    "(na)+|(na)+[0-9]+|[0-9]+(na)+|",
    "not|not[0-9]+|[0-9]+not|",
    "non|non[0-9]+|[0-9]+non|",
    "noone|noone[0-9]+|[0-9]+noone|",
    "nomail|nomail[0-9]+|[0-9]+nomail|",
    "noemail|noemail[0-9]+|[0-9]+noemail|",
    "noemailaddress|noemailaddress[0-9]+|[0-9]+noemailaddress|",
    "novalid|novalid[0-9]+|[0-9]+novalid|",
    "nonvalid|nonvalid[0-9]+|[0-9]+nonvalid|",
    "invalid|invalid[0-9]+|[0-9]+invalid|",
    "notvalidemail|notvalidemail[0-9]+|[0-9]+notvalidemail|",
    "noreply|noreply[0-9]+|[0-9]+noreply|",
    "donotreply|donotreply[0-9]+|[0-9]+donotreply|",
    "customer|",
    "unknown|unknown[0-9]+|[0-9]+unknown|",
    "notprovided|notprovided[0-9]+|[0-9]+notprovided|",
    "refused|refused[0-9]+|[0-9]+refused|",
    "refusedemail|refusedemail[0-9]+|[0-9]+refusedemail|",
    "customerrefused|customerrefused[0-9]+|[0-9]+customerrefused|",
    "nocustomer|nocustomer[0-9]+|[0-9]+nocustomer|",
    "fake|[0-9]+fake|fake[0-9]+|",
    "fake\\.fake|[0-9]+fake\\.fake|fake\\.fake[0-9]+|",
    "fakeemail|[0-9]+fakeemail|fakeemail[0-9]+|",
    "walmartfakeemail|[0-9]+walmartfakeemail|walmartfakeemail[0-9]+|",
    "www\\.tpwd|www\\.none\\.tpwd|",
    "texastpw|texastpwd|tpwdtexas|tpwtexas|",
    "tpw\\.texas|tpwd\\.texas|tpw\\.texas\\.gov|tpwd\\.texas\\.gov|",
    "tpwd\\.gov|tpw\\.gov|",
    "natpw|na\\.tpw|natpwd|na\\.tpwd|",
    "notpw|no\\.tpw|notpwd|no\\.tpwd|",
    "nontpw|non\\-tpw|non\\.tpw|",
    "nontpwd|non\\-tpwd|non\\.tpwd|",
    "nontpwd\\.texas|",
    "nonetpw|[0-9]+nonetpw|nonetpw[0-9]+|",
    "nonetpwd|[0-9]+nonetpwd|nonetpwd[0-9]+|",
    "none\\.tpw|none\\.tpwd|none\\.tpwd\\.texas|none\\.tpw\\.texas|",
    "tpwd|[0-9]+tpwd|tpwd[0-9]+|",
    "123|1234|12345|123456|1234567|12345678|123456789|",
    "0{1,100}",
    ")\\@")

# Regular expression for an obfuscative email address domain
REGEX_EMAIL_OBFUSCATIVE_DOMAIN <-
  paste0(
    "\\@(no|na|none|example|guerillamail|tpw|twp|test|spambog|fake|",
    "email\\-fake|temp\\-mail|www|spam|junk|[0-9]+gmail|[0-9]+yahoo|null).*$")

# Regular expression for an obfuscative email address
REF_EMAIL_OBFUSCATIVE_ADDRESS <-
  c("email@email.com", "email@gmail.com", "email@yahoo.com", "email@aol.com",
    "email@mail.com", "email@me.com", "email@hotmail.com")
