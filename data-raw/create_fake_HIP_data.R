# Generate fake HIP data to test migbirdHIP R package

# setup -------------------------------------------------------------------

# Use charlatan package to generate fake names, addresses, dates, etc
ap <- charlatan::AddressProvider_en_US$new()
pp <- charlatan::PersonProvider_en_US$new()
ip <- charlatan::InternetProvider_en_US$new()
dtp <- charlatan::DateTimeProvider$new()

# fake hunter generator functions -----------------------------------------

# Female generator
fake_woman <-
  function(ap, pp, ip, dtp) {
    dplyr::tibble(
      title = 2,
      firstname = pp$first_name_female(),
      middle = stringr::str_to_upper(pp$random_letter()),
      lastname = pp$last_name_female(),
      suffix = pp$suffix_female(),
      address = ap$street_address(),
      city = ap$city(),
      state_name = ap$state(),
      zip = ap$postcode(),
      birth_date =
        as.Date(dtp$date_time_between("1930-01-01",
                                      paste0(REF_CURRENT_SEASON, "-12-31"))),
      email = ip$email()
    )
  }

# Male generator
fake_man <-
  function(ap, pp, ip, dtp) {
    dplyr::tibble(
      title = 1,
      firstname = pp$first_name_male(),
      middle = stringr::str_to_upper(pp$random_letter()),
      lastname = pp$last_name_male(),
      suffix = pp$suffix_male(),
      address = ap$street_address(),
      city = ap$city(),
      state_name = ap$state(),
      zip = ap$postcode(),
      birth_date =
        as.Date(dtp$date_time_between("1930-01-01",
                                      paste0(REF_CURRENT_SEASON, "-12-31"))),
      email = ip$email()
    )
  }

# Military generator
fake_military <-
  function(ap, pp, ip, dtp) {
    dplyr::tibble(
      title = 0,
      firstname = pp$first_name(),
      middle = stringr::str_to_upper(pp$random_letter()),
      lastname = pp$last_name(),
      suffix = NA,
      mil_address = ap$mil_address(),
      address = stringr::str_extract(mil_address, "^.+(?=\\\n)"),
      city = stringr::str_extract(mil_address, "(?<=\\\n).+(?= [A-Z]{2})"),
      state_name = stringr::str_extract(mil_address, "[A-Z]{2}(?= [0-9]{5})"),
      zip = stringr::str_extract(mil_address, "[0-9]{5}"),
      birth_date =
        as.Date(dtp$date_time_between("1930-01-01",
                                      paste0(REF_CURRENT_SEASON, "-12-31"))),
      email = ip$email()
    ) |>
      dplyr::select(-mil_address)
  }

fake_issue_dates <-
  function(dtp, start, end) {
    random_issue_date <-
      dtp$date_time_between(start_date = start, end_date = end)

    paste0(
      stringr::str_sub(random_issue_date, 6, 7), "/",
      stringr::str_sub(random_issue_date, 9, 10), "/",
      stringr::str_sub(random_issue_date, 1, 4))
  }

randomBag <-
  function(n, bag_field) {
    bags <-
      migbirdHIP:::REF_BAGS |>
      dplyr::filter(state == REF_ABBR_49_STATES[n] & spp == bag_field) |>
      dplyr::pull(stateBagValue)

    sample(bags, 225, replace = T) |> as.character()
  }

createBagValue <-
  function(spp_field_name) {
    purrr::map(1:49, \(x) randomBag(x, spp_field_name)) |>
      purrr::flatten_chr()
  }

# set up fake hunter data -------------------------------------------------

# Fake hunter names, birth dates, street addresses, and email addresses
hunters <-
  # Generate fake hunter data for women
  purrr::map(1:1010, ~fake_woman(ap, pp, ip, dtp)) |>
  purrr::list_rbind() |>
  dplyr::mutate(suffix = ifelse(dplyr::row_number() == 3, suffix, NA)) |>
  # Generate and row bind fake hunter data for men
  dplyr::bind_rows(
    purrr::map(1:10000, ~fake_man(ap, pp, ip, dtp)) |>
      purrr::list_rbind() |>
      # Determine if we are keeping the suffix (we don't tend to get
      # professional suffixes in the data, but we will retain some here to serve
      # as errors)
      dplyr::mutate(
        suffix =
          ifelse(
            !suffix %in% REF_SUFFIXES |
              dplyr::row_number() %in% sample(c(1:200), 5),
            suffix,
            NA))) |>
  # Generate and row bind fake hunter data for military
  dplyr::bind_rows(
    purrr::map(1:15, ~fake_military(ap, pp, ip, dtp)) |>
      purrr::list_rbind()) |>
  # Convert state names to 2-letter abbreviations
  dplyr::left_join(
    dplyr::tibble(
      state_name = datasets::state.name,
      state = datasets::state.abb),
    by = "state_name") |>
  dplyr::mutate(state = ifelse(is.na(state), state_name, state)) |>
  dplyr::select(-state_name) |>
  # Make a key
  dplyr::mutate(row_key = paste0("row_", dplyr::row_number()))

# Define some fake PO Boxes
po_box <-
  c("PO BOX 35",
    "BOX 424",
    "P O BOX 1641",
    "POBOX 271",
    "PO BOX 494",
    "PO BOX 742",
    "P.O. BOX 211",
    "P.O.BOX 1017",
    "P. O. BOX 162",
    "PO BOX 428")

# Define some fake email addresses
fake_emails <-
  c("NONE@TPWD.GOV",
    "NA@NA.COM",
    "NONE@TPWD.TEXAS.GOV",
    "NA@GMAIL.COM",
    "NOEMAIL@GMAIL.COM",
    "NONE@GMAIL.COM",
    "NO@GMAIL.COM",
    "NA@YAHOO.COM",
    "NA@GOV.COM",
    "NOMAIL@GMAIL.COM")

# Define some incorrectly formatted email addresses
bad_emails <-
  c(";JJ;@AOL.COM",
    "GBLOO @HOTMAIL.COM",
    "ABC@AOL.COOM",
    "HAN88@GMAIL.CON",
    "TEZ80@GMAI..COM",
    "NM204@GMAIL.ORG",
    "OBA@GMAIL.CM",
    "NTU@GMAI.C.OM",
    "PER@AT&T.NET",
    "y2@gmail.com<")

# Define some Canadian zip codes
canada_zips <-
  c("G0A 1H0",
    "H1A 0B6",
    "M3C 0C3",
    "T1X 0L5",
    "V5H2K3",
    "A0P 1T0",
    "Y0B 0A4",
    "X1A X9A",
    "V5M 1Z7",
    "B3G1N5")

upload_dates <-
  c(paste0("08", stringr::str_pad(c(1:31), 2, pad = "0")),
    paste0("09", stringr::str_pad(c(1:7), 2, pad = "0")))

file_dates <- paste0(REF_CURRENT_SEASON, upload_dates, ".txt")

# messy selections --------------------------------------------------------

# Select some hunters for intentional errors: change to NA, add a hyphen, add a
# space, add a period, add a comma, add a number, change to 1 letter, change to
# 2 initials, etc

# Title
title_errors <- list()
title_errors$NAs <- sample(hunters$row_key, size = 250)

# First names
firstname_errors <- list()
firstname_errors$NAs <- sample(hunters$row_key, size = 60)
firstname_errors$initial <- sample(hunters$row_key, size = 100)
firstname_errors$twoletter <- sample(hunters$row_key, size = 30)
firstname_errors$hyphen <- sample(hunters$row_key, size = 150)
firstname_errors$space <- sample(hunters$row_key, size = 100)
firstname_errors$period <- sample(hunters$row_key, size = 5)
firstname_errors$comma <- sample(hunters$row_key, size = 5)
firstname_errors$number <- sample(hunters$row_key, size = 5)
firstname_errors$test <- sample(hunters$row_key, size = 1)

# Middle initials
middle_errors <- list()
middle_errors$NAs <- sample(hunters$row_key, size = 0.3*nrow(hunters))

# Last names
lastname_errors <- list()
lastname_errors$NAs <- sample(hunters$row_key, size = 70)
lastname_errors$hyphen <- sample(hunters$row_key, size = 150)
lastname_errors$space <- sample(hunters$row_key, size = 300)
lastname_errors$period <- sample(hunters$row_key, size = 50)
lastname_errors$initial <- sample(hunters$row_key, size = 100)
lastname_errors$test <- firstname_errors$test

# Address
address_errors <- list()
address_errors$pobox <- sample(hunters$row_key, size = 0.01*nrow(hunters))

# Zip code
zip_errors <- list()
zip_errors$zip_state_bad <- sample(hunters$row_key, size = 0.1*nrow(hunters))

# Email
email_errors <- list()
email_errors$bademails <- sample(hunters$row_key, size = 30)
email_errors$fakeemails <- sample(hunters$row_key, size = 30)

# HuntY
hunty_errors <- list()
hunty_errors$hunty1 <- sample(hunters$row_key, size = 0.01*nrow(hunters))

# Bag errors
bag_errors <- list()
bag_errors$allzero <- sample(hunters$row_key, size = 5)
bag_errors$duck <- sample(hunters$row_key, size = 10)
bag_errors$goose <- sample(hunters$row_key, size = 10)
bag_errors$dove <- sample(hunters$row_key, size = 10)
bag_errors$woodcock <- sample(hunters$row_key, size = 10)
bag_errors$cs <- sample(hunters$row_key, size = 10)
bag_errors$rg <- sample(hunters$row_key, size = 10)
bag_errors$crane <- sample(hunters$row_key, size = 10)
bag_errors$btpi <- sample(hunters$row_key, size = 10)
bag_errors$brant <- sample(hunters$row_key, size = 10)
bag_errors$seaduck <- sample(hunters$row_key, size = 10)

# International hunters
international <- list()
international$canada <- sample(hunters$row_key, size = 10)

# introduce errors --------------------------------------------------------

# Make the data messy by using the randomly selected fake hunters above for
# intentional error introduction. Use case_when so that a change is not added
# more than once per field

messy_hunters <-
  hunters |>
  # Join in the real state for each zip code (might be some errors, only using
  # the first 3 digits and then reconciling down to 1 state at random)
  dplyr::mutate(zip_key = stringr::str_sub(zip, 1, 3)) |>
  dplyr::left_join(
    migbirdHIP:::REF_ZIP_CODE |>
      dplyr::mutate(zip_key = stringr::str_sub(zipcode, 1, 3)) |>
      dplyr::distinct(zip_key, state) |>
      dplyr::group_by(zip_key) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(actual_state = state, zip_key),
    by = "zip_key"
  ) |>
  # Introduce messy values for: title, firstname, middle, lastname, address,
  # state, zip, and email
  dplyr::mutate(
    title =
      ifelse(row_key %in% title_errors$NAs, NA, title),
    firstname =
      dplyr::case_when(
        row_key %in% firstname_errors$test ~ "TEST",
        row_key %in% firstname_errors$NAs ~ NA_character_,
        row_key %in% firstname_errors$initial ~
          stringr::str_sub(firstname, 1, 1),
        row_key %in% firstname_errors$twoletter ~
          paste(stringr::str_sub(firstname, 1, 1), middle),
        row_key %in% firstname_errors$hyphen[firstname_errors$hyphen !=
                                               paste0("row_", nrow(hunters))] ~
          paste(firstname, dplyr::lead(firstname), sep = "-"),
        row_key %in% firstname_errors$space[firstname_errors$space !=
                                              paste0("row_", nrow(hunters))] ~
          paste(firstname, dplyr::lead(firstname), sep = " "),
        row_key %in% firstname_errors$period ~
          paste(firstname, " ", middle, "."),
        row_key %in% firstname_errors$comma ~
          paste(lastname, firstname, sep = ","),
        row_key %in% firstname_errors$number ~
          paste0(firstname, sample(c(3, 6, 7, 8), dplyr::n(), replace = T)),
        TRUE ~ firstname
      ),
    middle =
      ifelse(row_key %in% middle_errors$NAs, NA, middle),
    lastname =
      dplyr::case_when(
        row_key %in% firstname_errors$test ~ "TEST",
        row_key %in% lastname_errors$NAs ~ NA_character_,
        row_key %in% lastname_errors$hyphen[lastname_errors$hyphen !=
                                              paste0("row_", nrow(hunters))] ~
          paste(lastname, dplyr::lead(lastname), sep = "-"),
        row_key %in% lastname_errors$space[lastname_errors$space !=
                                             paste0("row_", nrow(hunters))] ~
          paste(lastname, dplyr::lead(lastname), sep = " "),
        row_key %in% lastname_errors$period ~
          paste0(lastname, "."),
        row_key %in% lastname_errors$initial ~
          stringr::str_sub(lastname, 1, 1),
        TRUE ~ lastname
      ),
    address =
      ifelse(
        row_key %in% address_errors$pobox,
        sample(po_box, dplyr::n(), replace = T),
        address),
    state =
      dplyr::case_when(
        row_key %in% international$canada ~
          sample(REF_ABBR_CANADA, dplyr::n(), replace = T),
        row_key %in% zip_errors$zip_state_bad ~ state,
        TRUE ~ actual_state),
    zip =
      ifelse(
        row_key %in% international$canada,
        sample(canada_zips, dplyr::n(), replace = T),
        zip),
    email =
      dplyr::case_when(
        row_key %in% email_errors$bademails ~
          sample(bad_emails, dplyr::n(), replace = T),
        row_key %in% email_errors$fakeemails ~
          sample(fake_emails, dplyr::n(), replace = T),
        TRUE ~ email
      )
  ) |>
  dplyr::select(-c("zip_key", "actual_state")) |>
  dplyr::relocate(state, .after = "city") |>
  # Cut field string lengths to FWF limits
  dplyr::mutate(
    firstname = stringr::str_sub(firstname, 1, 15),
    lastname = stringr::str_sub(lastname, 1, 20),
    city = stringr::str_sub(city, 1, 20))

# Randomly arrange hunters so that all of the women are not assigned to the
# first 5 download states
messy_hunters_shuffled <- messy_hunters[sample(1:nrow(messy_hunters)),]

# create HIP response fields ----------------------------------------------

# Define repeated logical expressions
bagLogic1 <-
  rlang::expr(row_key %in% bag_errors$allzero)

bagLogic2 <-
  rlang::expr(dl_state %in% c("WA", "OR") & stringr::str_detect(row_key, "8$"))

# Create dl_state, reformat birth_date, create issue_date, create
# hunt_mig_birds, create bag fields, introduce all-zero errors, create WA and
# OR inline permits, and introduce errors to hunt_mig_birds and bag fields
fake_hip <-
  messy_hunters_shuffled |>
  dplyr::mutate(
    # dl_state: 225 records from all 49 states
    dl_state =
      purrr::map(REF_ABBR_49_STATES, \(x) rep(x, 225)) |> purrr::flatten_chr(),
    birth_date =
      paste0(
        stringr::str_sub(birth_date, 6, 7), "/",
        stringr::str_sub(birth_date, 9, 10), "/",
        stringr::str_sub(birth_date, 1, 4)),
    issue_date =
      purrr::map_chr(
        1:nrow(messy_hunters_shuffled),
        ~fake_issue_dates(
          dtp,
          start = paste0(REF_CURRENT_SEASON, "-01-01"),
          end = paste0(REF_CURRENT_SEASON, "-09-07"))),
    hunt_mig_birds =
      ifelse(row_key %in% hunty_errors$hunty1, "1", "2"),
    ducks_bag =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        !!bagLogic2 ~ "0",
        row_key %in% bag_errors$duck ~ "9",
        TRUE ~ createBagValue("ducks_bag")
      ),
    geese_bag =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        !!bagLogic2 ~ "0",
        row_key %in% bag_errors$goose ~ "9",
        TRUE ~ createBagValue("geese_bag")
      ),
    dove_bag =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        !!bagLogic2 ~ "0",
        row_key %in% bag_errors$dove ~ "9",
        TRUE ~ createBagValue("dove_bag")
      ),
    woodcock_bag =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        !!bagLogic2 ~ "0",
        row_key %in% bag_errors$woodcock ~ "9",
        TRUE ~ createBagValue("woodcock_bag")
      ),
    coots_snipe =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        !!bagLogic2 ~ "0",
        row_key %in% bag_errors$cs ~ "9",
        TRUE ~ createBagValue("coots_snipe")
      ),
    rails_gallinules =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        !!bagLogic2 ~ "0",
        row_key %in% bag_errors$rg ~ "9",
        TRUE ~ createBagValue("rails_gallinules")
      ),
    cranes =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        !!bagLogic2 ~ "0",
        row_key %in% bag_errors$crane ~ "9",
        TRUE ~ createBagValue("cranes")
      ),
    band_tailed_pigeon =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        dl_state %in% c("WA", "OR") &
          stringr::str_detect(firstname, "S|s") &
          stringr::str_detect(row_key, "8$") ~ "2",
        dl_state %in% c("WA", "OR") &
          !stringr::str_detect(firstname, "S|s") &
          stringr::str_detect(row_key, "8$") ~ "0",
        row_key %in% bag_errors$btpi ~ "9",
        TRUE ~ createBagValue("band_tailed_pigeon")
      ),
    brant =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        dl_state %in% c("WA", "OR") &
          stringr::str_detect(lastname, "S|s") &
          stringr::str_detect(row_key, "8$") ~ "2",
        dl_state %in% c("WA", "OR") &
          !stringr::str_detect(lastname, "S|s") &
          stringr::str_detect(row_key, "8$") ~ "0",
        row_key %in% bag_errors$brant ~ "9",
        TRUE ~ createBagValue("brant")
      ),
    seaducks =
      dplyr::case_when(
        !!bagLogic1 ~ "0",
        dl_state %in% c("WA", "OR") &
          stringr::str_detect(address, "S|s") &
          stringr::str_detect(row_key, "8$") ~ "2",
        dl_state %in% c("WA", "OR") &
          !stringr::str_detect(address, "S|s") &
          stringr::str_detect(row_key, "8$") ~ "0",
        row_key %in% bag_errors$seaduck ~ "9",
        TRUE ~ createBagValue("seaducks")
      ),
    registration_yr = REF_CURRENT_SEASON
  ) |>
  dplyr::relocate(email, .after = "registration_yr") |>
  dplyr::group_by(dl_state) |>
  dplyr::mutate(source_file = paste0(dl_state, sample(file_dates, 1))) |>
  dplyr::ungroup() |>
  dplyr::select(-c("row_key", "dl_state"))

# Introduce duplicates
fake_hip_with_duplicates <-
  fake_hip |>
  # Exact duplicates
  dplyr::bind_rows(fake_hip |> dplyr::slice_sample(n = 110)) |>
  # Partial duplicates
  dplyr::bind_rows(
    fake_hip |>
      dplyr::slice_sample(n = 90) |>
      dplyr::mutate(
        seaducks =
          ifelse(
            stringr::str_detect(source_file, "OR|WA") & seaducks == "2",
            "0",
            seaducks),
        geese_bag =
          ifelse(geese_bag == "2", "0", geese_bag),
        ducks_bag =
          ifelse(geese_bag == "2", "1", ducks_bag),
        issue_date =
          ifelse(
            stringr::str_detect(firstname, "S|s"),
            paste(
              stringr::str_sub(lubridate::mdy(issue_date)-1, 6, 7),
              stringr::str_sub(lubridate::mdy(issue_date)-1, 9, 10),
              stringr::str_sub(lubridate::mdy(issue_date)-1, 1, 4), sep = "/"),
            issue_date)
          )
      )

# Split the fake data into a list by dl_state/source file
split_fake_hip <-
  split(
    fake_hip_with_duplicates |> dplyr::select(-source_file),
    f = fake_hip_with_duplicates$source_file)

# write to R package ------------------------------------------------------

# Write 49 files to extdata dir
purrr::walk(
  1:length(split_fake_hip),
  \(x) gdata::write.fwf(
    as.data.frame(split_fake_hip[[x]]),
    file =
      paste0(here::here(), "/inst/extdata/DL0901/", names(split_fake_hip[x])),
    sep = "",
    colnames = F,
    width = c(1, 15, 1, 20, 3, 60, 20, 2, 10, 10, 10,
              1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 100))
)

# Mini test data
DF_TEST_MINI <-
  fake_hip_with_duplicates |>
  dplyr::mutate(
    # Add the download state as a column
    dl_state =
      stringr::str_extract(source_file, "[A-Z]{2}(?=[0-9]{8}\\.txt)"),
    # Add the download date as a column
    dl_date =
      stringr::str_extract(source_file, "(?<=[A-Z]{2})[0-9]{8}(?=\\.txt)"),
    # Add the download cycle as a column
    dl_cycle = "0901") |>
  # Keep some OR records to represent solo permit state
  # Keep some ME records to represent SD-only state
  # Keep some DE records to represent SD and BR state
  # Keep some ND records to represent CR state
  # Keep some UT records to represent BT state
  # Keep some CO records to represent CR and BT state
  # Keep some IA records to represent non-BT, CR, SD, or BR state
  dplyr::filter(dl_state %in% c("OR", "ME", "DE", "ND", "UT", "CO", "IA")) |>
  dplyr::mutate(record_key = paste0("record_", dplyr::row_number())) |>
  dplyr::group_by(dl_date, dl_state) |>
  dplyr::mutate(dl_key = paste0("dl_", dplyr::cur_group_id())) |>
  dplyr::ungroup() |>
  dplyr::relocate(source_file, .after = "dl_date") |>
  dplyr::relocate(dl_key, .after = "dl_cycle")

# Write mini data to extdata directory
usethis::use_data(DF_TEST_MINI, overwrite = T)

# Define season year
yr <- as.numeric(REF_CURRENT_SEASON)

# Tini test data
DF_TEST_TINI_READ <-
  DF_TEST_MINI |>
  dplyr::filter(dl_state == "IA") |>
  dplyr::slice_sample(n = 3) |>
  dplyr::mutate(record_key = paste0("record_", dplyr::row_number()))

# Partially process tini test data
DF_TEST_TINI_CLEANED <- clean(DF_TEST_TINI_READ)
DF_TEST_TINI_CURRENT <- issueCheck(DF_TEST_TINI_CLEANED, yr)
DF_TEST_TINI_DEDUPED <- duplicateFix(DF_TEST_TINI_CURRENT)
DF_TEST_TINI_PROOFED <- proof(DF_TEST_TINI_DEDUPED, yr)
DF_TEST_TINI_CORRECTED <- correct(DF_TEST_TINI_PROOFED, yr)

# Write partially processed tini data to extdata directory
usethis::use_data(DF_TEST_TINI_READ, overwrite = T)
usethis::use_data(DF_TEST_TINI_CLEANED, overwrite = T)
usethis::use_data(DF_TEST_TINI_CURRENT, overwrite = T)
usethis::use_data(DF_TEST_TINI_DEDUPED, overwrite = T)
usethis::use_data(DF_TEST_TINI_PROOFED, overwrite = T)
usethis::use_data(DF_TEST_TINI_CORRECTED, overwrite = T)
