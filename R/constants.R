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
REF_FIELDS_BAG <-
  c("ducks_bag", "geese_bag", "dove_bag", "woodcock_bag", "coots_snipe",
    "rails_gallinules", "cranes", "band_tailed_pigeon", "brant", "seaducks")

# Vector of fields that are used to deduplicate hunters
REF_FIELDS_HUNTER_ID <-
  c("firstname", "lastname", "state", "birth_date", "dl_state",
    "registration_yr")

# All HIP field names in order
REF_FIELDS_ALL <-
  c("title", "firstname", "middle", "lastname", "suffix", "address", "city",
    "state", "zip", "birth_date", "issue_date", "hunt_mig_birds",
    REF_FIELDS_BAG, "registration_yr", "email")

REF_STRATA_NAMES <-
  c("S_ducks", "S_geese", "S_doves", "S_woodcock", "S_coot_snipe",
    "S_rail_gallinule", "S_cranes", "S_bt_pigeons", "S_brant", "S_seaducks")

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
    "^(?!.*\\'\\')(?!.*\\s\\s)(?!.*\\-\\-)(?!.*\\sAKA\\s.*)[A-Z+\\'?]\\-?\\s?",
    "\\'?[A-Z\\-?\\s?\\'?]*[A-Z]+$")

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
    "^(([0-9]+)?(none|123)*[\\.\\-\\_\\+]*(none|nope|null|not|non|no|na|",
      "fake([\\_\\.\\-]fake)?|unknown|abc|xyz|fu|name|dont)+[\\.\\-\\_\\+]*",
      "(none|123)*([0-9]+)?|",
    "none\\..+|",
    "www\\.(none|nope|null|no|na|fake|unknown|abc|xyz|(none\\.?)?tpw(d)?)+|",
    "(o|n|e){4,100}|",
    "noen|nnon|onoene|nnnnnne|nonen|nonoe|nnoe|onoennoneo|nonne|nnone|nnno|",
      "onon|onoe|nonn|noeone|nonneee|nonnee|nonoeone|onne|nononon|oonono|",
      "onoen|noneo|",
    "([0-9]+)?no((n|p)e)?[\\_\\.\\-]*(one|(e)?mai(l)?(none)?|emial|emal|eamil|",
      "emsil|thank(s|you)?)([0-9]+)?|",
    "([0-9]+)?(no(n|t)?|in)[\\_\\.\\-]*valid(email)?([0-9]+)?|",
    "([0-9]+)?(no)?[\\_\\.\\-]*(done|(un)?subscribe|customer|email[\\_\\.\\-]*",
      "address)([0-9]+)?|",
    "([0-9]+)?(no|(do)?[\\.\\-\\_]*not)[\\.\\-\\_]*reply([0-9]+)?|",
    "([0-9]+)?non(e)?(ya|yab.+|you(r)?b.+|onfile|ofyourbusiness|given|usa|text",
      "|yet|atall|walmart)([0-9]+)?|",
    "([0-9]+)?(not|none)[\\_\\.\\-]*(ava(i)?la(i)?ble|ap(p)?lic(able)?|provide",
      "(d)?|interest(e)?(d)?|me|one|you|(e)?mail|again)([0-9]+)?|",
    "([0-9]+)?(have|got|nope(aint(got)?)?)[\\_\\.\\-]*none([0-9]+)?|",
    "([0-9]+)?nope[\\_\\.\\-]((e)?mail)[\\_\\.\\-]*([0-9]+)?|",
    "([0-9]+)?(nope)?[\\_\\.\\-]*not[\\_\\.\\-]*to(t)?(d)?ay[\\_\\.\\-]*",
      "([0-9]+)?|",
    "([0-9]+)?(customer)?[\\_\\.\\-]*refused[\\_\\.\\-]*(email)?([0-9]+)?|",
    "([0-9]+)?(walmart)?[\\_\\.\\-]*fake[\\_\\.\\-]*email([0-9]+)?|",
    "([0-9]+)?(na|non*(e*)?)?([0-9]+)?[\\_\\.\\-]*(tpw(d)?|twd|twp(d)?)",
      "([0-9]+)?([0-9]+)?[\\_\\.\\-]*(texas|na|non*(e*)?)?(\\.gov)?|",
    "([0-9]+)?[\\_\\.\\-]*texas[\\_\\.\\-]*(tpw(d)?|twd|twp(d)?)[\\_\\.\\-]*",
      "([0-9]+)?|",
    "none.*texas|notexas|notex|",
    "(john|jane)\\.?doe|",
    "123|1234|12345|123456|1234567|12345678|123456789|",
    "0{1,100}",
    ")\\@")

# Regular expression for an obfuscative email address domain
REGEX_EMAIL_OBFUSCATIVE_DOMAIN <-
  paste0(
    "\\@(no|na|none|example|guerillamail|test|spambog|fake|email\\-fake|",
    "temp\\-mail|www|spam|junk|[0-9]+gmail|[0-9]+yahoo|null|dnr|that|gov|mil|",
    "org|net|edu|com|done|customer|tpw|twp|twd|twpd|twdp|tx)\\.(com|net|edu|",
    "org|us|gov|mil|biz|co|me|io)$")

# Regular expression for an obfuscative repeated letter or number email address,
# e.g. zz@zzz.com or 22@22.gov
REGEX_EMAIL_REPEATED_CHAR <-
  "^([a-z0-9])\\1*\\@\\1+\\.(com|net|edu|org|us|gov|mil|biz|co|me|io)$"

# Regular expression for a fake Texas email addresses (correct format is
# firstname.lastname@)
REGEX_EMAIL_OBFUSCATIVE_TPWD <- "^[^\\.]+\\@tpwd\\.texas\\.gov$"

# Regular expression for a fake Walmart email address
REGEX_EMAIL_OBFUSCATIVE_WALMART <- "^[0-9]+\\@walmart\\.com$"

# Reference examples of obviously obfuscative email address that can't be
# filtered using local part or domain alone
REF_EMAIL_OBFUSCATIVE_ADDRESS <-
  c("email@email.com", "email@gmail.com", "email@yahoo.com", "email@aol.com",
    "email@mail.com", "email@me.com", "email@hotmail.com",
    "mail@email.com", "mail@gmail.com", "mail@yahoo.com", "mail@aol.com",
    "mail@mail.com", "mail@me.com", "mail@hotmail.com", "bob@bob.com",
    "n@o.no", "n@a.com", "n@o.com", "f@u.com", "x@y.com", "me@u.com")
