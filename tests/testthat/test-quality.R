# quality -----------------------------------------------------------------

# qualityMessages
# qSummary

# missing data ------------------------------------------------------------

test_that("missingPIIMessage returns a message for missing PII", {
  missing_pii <-
    DF_TEST_TINI_READ |>
    mutate(firstname = NA)

  suppressMessages(
    invisible(
      capture.output(
        expect_message(missingPIIMessage(missing_pii))
      )
    )
  )
})

test_that("missingPIIMessage does not return a message for good PII fields", {
  expect_no_message(missingPIIMessage(DF_TEST_TINI_READ))
})

test_that("missingEmailsMessage returns a message for missing emails", {
  missing_email <-
    # Use DF_TEST_MINI for data with more than 10 registrations per file
    DF_TEST_MINI |>
    filter(source_file == DF_TEST_TINI_READ$source_file[1]) |>
    mutate(email = NA)

  suppressMessages(
    invisible(
      capture.output(
        expect_message(missingEmailsMessage(missing_email))
      )
    )
  )
})

test_that("missingEmailsMessage returns a message for identical emails", {
  repeated_email <-
    # Use DF_TEST_MINI for data with more than 10 registrations per file
    DF_TEST_MINI |>
    filter(source_file == DF_TEST_TINI_READ$source_file[1]) |>
    mutate(email = "identical_email_for_entire_file@gmail.com")

  suppressMessages(
    invisible(
      capture.output(
        expect_message(missingEmailsMessage(repeated_email))
      )
    )
  )
})

test_that("missingEmailsMessage does not return a message for good emails", {
  good_email <-
    DF_TEST_MINI |>
    slice_sample(n = 250) |>
    mutate(
      email =
        rep(c("TonyStark@starkindustries.com",
              "Rhodes@q.com",
              "JARVIS@verizon.net",
              "p3pperp0tts@cox.net",
              "peter_parker@gmail.com",
              "blackwidow@posteo.de",
              "BlackPanther@github.io",
              "shuri@wakanda.gov",
              "thor@comcast.net",
              NA),
            25))

  expect_no_message(missingEmailsMessage(good_email))
})

# test record -------------------------------------------------------------

test_that("testRecordMessage returns a message for test records", {
  test_data <-
    DF_TEST_MINI |>
    dplyr::filter(
      !dplyr::if_any(
        c("firstname", "lastname", "birth_date", "state"), is.na)) |>
    dplyr::slice_head(n = 10) |>
    mutate(
      firstname =
        case_when(
          record_key == .data$record_key[1] ~ "TEST",
          record_key == .data$record_key[2] ~ "TEST",
          record_key == .data$record_key[3] ~ "INAUDIBLE",
          record_key == .data$record_key[4] ~ "BLANK",
          record_key == .data$record_key[5] ~ "USER",
          record_key == .data$record_key[6] ~ "RESIDENT",
          TRUE ~ firstname),
      lastname =
        case_when(
          record_key == .data$record_key[1] ~ "TEST",
          record_key == .data$record_key[7] ~ "INAUDIBLE",
          TRUE ~ lastname))

  suppressMessages(
    invisible(
      capture.output(
        map(
          1:7,
          \(x) expect_message(testRecordMessage(test_data[x,]))
        )
      )
    )
  )
})

test_that("testRecordMessage does not return a message for non-test data", {
  good_data <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 10)

  expect_no_message(testRecordMessage(good_data))
})

# non-resident ------------------------------------------------------------

test_that("nonResidentMessage returns a message for nonresidents", {

  test_data <-
    DF_TEST_MINI |>
    filter(dl_state != .data$state)

  suppressMessages(
    invisible(capture.output(expect_message(nonResidentMessage(test_data)))))
})

test_that("nonResidentMessage does not return a message for good data", {
  good_data <-
    DF_TEST_MINI |>
    mutate(state = .data$dl_state)

  expect_no_message(nonResidentMessage(good_data))
})

# inter-state duplicates --------------------------------------------------

test_that("interStateDuplicatesMessage returns a message for bad data", {

  test_data <-
    bind_rows(
      DF_TEST_MINI |> mutate(dl_state = "SC"),
      DF_TEST_MINI |> mutate(dl_state = "TN"))

  suppressMessages(
    invisible(
      capture.output(
        expect_message(interStateDuplicatesMessage(test_data)))))
})

test_that("interStateDuplicatesMessage doesn't return message for good data", {
  expect_no_message(interStateDuplicatesMessage(DF_TEST_MINI))
})

# quality messages --------------------------------------------------------

test_that("qTitle returns a message for bad titles", {
  bad_titles <-
    DF_TEST_MINI |>
    slice_sample(n = 8) |>
    mutate(
      title = c("*", "%", "#", "!", "3", "10", "Z", "TH"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qTitle(bad_titles))
      )
    )
  )
})

test_that("qTitle does not return a message for good titles", {
  good_titles <-
    DF_TEST_TINI_DEDUPED |>
    mutate(
      title =
        case_when(
          row_number() == 1 ~ "1",
          row_number() == 2 ~ "1",
          row_number() == 3 ~ "2",
          TRUE ~ NA_character_),
      firstname =
        case_when(
          row_number() == 1 ~ "JOHN",
          row_number() == 2 ~ "ALEXANDER",
          row_number() == 3 ~ "JESSICA",
          TRUE ~ NA_character_),
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  expect_no_message(qTitle(good_titles))
})

test_that("qFirstName returns a message for bad first name", {
  bad_first_names <-
    tibble(
      firstname =
        c("C",
          "BOB-",
          "-BOB",
          " BOBBY",
          "BOBBY ",
          "JAMES DEAN ",
          " JAMES DEAN",
          "BOB- ",
          " -BOB",
          "BOB -",
          "- BOB",
          "1",
          "B0B",
          "DAV3",
          "~",
          "M~",
          "`",
          "KU`UIPO",
          "!",
          "FRED!",
          "@",
          "GREG@GMAIL",
          "#",
          "MARK#",
          "$",
          "MARK$",
          "%",
          "90%",
          "AMY%",
          "^",
          "^CAL",
          "&",
          "BILL & PAM",
          "*",
          "A*RON",
          "(",
          ")",
          "GREGORY (GREG)",
          "+",
          "JOHN+DOE",
          "=",
          "PHIL=",
          "-",
          ".",
          "MR.BOB",
          "MR. BOB",
          "''DEON",
          "JO''HN",
          "DAN''",
          "H  AL",
          "  HAL",
          "HAL  ",
          "WALL--E",
          "--JAN",
          "JAN--",
          "MARY-JANE--WILDER",
          "MARY JANE  WILDER",
          "BOB AKA THE GREAT",
          "BOB A.K.A. THE GREAT",
          "BOB F.K.A. THE GREAT",
          ""),
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qFirstName(bad_first_names))
      )
    )
  )
})

test_that("qFirstName does not return a message for good first name", {
  good_first_names <-
    tibble(
      firstname =
        c("JO",
          "ABE",
          "DORT",
          "DAVID",
          "JOHNNY",
          "GABRIEL",
          "CHANDLER",
          "'IWALANI",
          "KU'UIPO",
          "MA'IA'I",
          "HA-YOON",
          "BO-A",
          "JEAN-BAPTISTE",
          "MINH ANH",
          "MARY-ANN LOUISE",
          "JOE BILLY-BOB",
          "JEA-YVES-ANDRE",
          "JOHN PAUL GEORGE"),
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  expect_no_message(qFirstName(good_first_names))
})

test_that("qMiddle returns a message for bad middle initials", {
  bad_middles <-
    tibble(middle = c("*", "%", "#", "!", "3", "10", "TH")) |>
    mutate(
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qMiddle(bad_middles))
      )
    )
  )
})

test_that("qMiddle does not return a message for good middle initials", {
  good_middles <-
    tibble(middle = c(LETTERS, NA)) |>
    mutate(
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  expect_no_message(qMiddle(good_middles))
})

test_that("qLastName returns a message for bad last names", {
  bad_last_names <-
    tibble(
      lastname =
        c("B",
          "SMITH-",
          "-SMITH",
          " JONES",
          "JONES ",
          "SMITH JONES ",
          " SMITH JONES",
          "SMITH- ",
          " -SMITH",
          "SMITH -",
          "- SMITH",
          "SMITH.",
          ".SMITH",
          "SMITH. ",
          " .SMITH",
          "SMITH .",
          ". SMITH",
          "1",
          "B0B",
          "DAV3",
          "~",
          "M~",
          "`",
          "KU`UIPO",
          "!",
          "FRED!",
          "@",
          "GREG@GMAIL",
          "#",
          "MARK#",
          "$",
          "MARK$",
          "%",
          "90%",
          "AMY%",
          "^",
          "^CAL",
          "&",
          "BILL & PAM",
          "*",
          "A*RON",
          "(",
          ")",
          "GREGORY (GREG)",
          "+",
          "JOHN+DOE",
          "=",
          "PHIL=",
          "-",
          ".",
          "..JONES",
          "JONES..",
          "JO..NES",
          "JO ..NES",
          "''GARCIA",
          "DA''VIS",
          "DAVIS''",
          "W  ILLIAMS",
          "  WILLIAMS",
          "WILLIAMS  ",
          "WILLIA--MS",
          "--MOORE",
          "MOORE--",
          "MARY-JANE--WILDER",
          "MARY JANE  WILDER",
          "MARY.JANE  WILDER",
          "MARY..JANE  WILDER",
          "ST. ST. GERMAINE",
          "ST.ST.GERMAINE",
          "S.S. WASHINGTON",
          ""),
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qLastName(bad_last_names))
      )
    )
  )
})

test_that("qLastName does not return a message for good last names", {
  good_last_names <-
    tibble(
      lastname =
        c("LI",
          "LEE",
          "KING",
          "ABBOT",
          "KNIGHT",
          "SHERIFF",
          "THATCHER",
          "UNDERWOOD",
          "O'MALLEY",
          "TO'OTO'O",
          "BOWES-LYON",
          "CAVE-BROWNE-CAVE",
          "ST GERMAINE",
          "ST. GERMAINE",
          "DIT TRANCHEMONTAGNE",
          "VAN DER WAAL",
          "REYES DE LA BARRERA"),
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  expect_no_message(qLastName(good_last_names))
})

test_that("qSuffix returns a message for bad suffixes", {
  bad_suffixes <-
    tibble(
      suffix =
        c(tolower(REF_ROMAN_SUFFIXES), tolower(REF_ORDINAL_SUFFIXES),
          as.character(1:20), "s", "t", "h", "n", "d", "S", "T", "H", "N", "D",
          "ST", "ND", "RD", "TH", "iiii", "IIII", "VV", "VVV"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qSuffix(bad_suffixes))
      )
    )
  )
})

test_that("qSuffix does not return a message for good suffixes", {
  good_suffixes <-
    tibble(
      suffix =
        c(
          NA,
          "I",
          "II",
          "III",
          "IV",
          "V",
          "VI",
          "VII",
          "VIII",
          "IX",
          "X",
          "XI",
          "XII",
          "XIII",
          "XIV",
          "XV",
          "XVI",
          "XVII",
          "XIX",
          "XX",
          "1ST",
          "2ND",
          "3RD",
          "4TH",
          "5TH",
          "6TH",
          "7TH",
          "8TH",
          "9TH",
          "10TH",
          "11TH",
          "12TH",
          "13TH",
          "14TH",
          "15TH",
          "16TH",
          "17TH",
          "18TH",
          "19TH",
          "20TH",
          "JR",
          "SR"
        ),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  expect_no_message(qSuffix(good_suffixes))
})

test_that("qAddress returns a message for bad address", {
  bad_addresses <-
    tibble(
      address = c("80 Fox |Dr", "4 Bear’s Pl"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qAddress(bad_addresses))
      )
    )
  )
})

test_that("qAddress does not return a message for good address", {
  good_addresses <-
    tibble(
      address = c("966 American Holly Ln", "3 Bee Dr", "PO Box 9"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  expect_no_message(qAddress(good_addresses))
})

test_that("qCity returns a message for bad city", {
  bad_city_names <-
    tibble(
      city = c("Wilming$ton",
               "Saint-Louis-du-Ha! Ha!",
               "0maha",
               "St..Louis",
               "Po"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qCity(bad_city_names))
      )
    )
  )
})

test_that("qCity does not return a message for good city", {
  good_city_names <-
    tibble(
      city = c("Los Angeles",
               "Annapolis",
               "St. Petersburg",
               "Coeur d'Alene",
               "Dover-Foxcroft",
               "Roy",
               "La Plata"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  expect_no_message(qCity(good_city_names))
})

test_that("qState returns a message for bad state", {
  bad_states <-
    tibble(
      state = c("la", "De", "MX", "CN", "00", "ZZ", " ", "  ", "_", "!!", "*"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qState(bad_states))
      )
    )
  )
})

test_that("qState does not return a message for good state", {
  good_states <-
    tibble(
      state =
        c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI",
          "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI",
          "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC",
          "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
          "VT", "VA", "WA", "WV", "WI", "WY", "DC", "AS", "GU", "MP", "PR",
          "VI", "UM", "MH", "FM", "PW", "AA", "AE", "AP", "AB", "BC", "MB",
          "NB", "NL", "NS", "NT", "NU", "ON", "PE", "PQ", "QC", "SK", "YT"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  expect_no_message(qState(good_states))
})

test_that("qZIP returns a message for bad zip code", {
  bad_zips <-
    tibble(
      zip =
        c("*",
          "3",
          "21",
          "123",
          "4567",
          "99999",
          "M5H 2N2",
          "ABCDE"),
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qZIP(bad_zips))
        )
    )
  )
})

test_that("qZIP does not return a message for good zip code", {
  good_zips <-
    tibble(
      zip = c("20708",
              "10036",
              "22046",
              "20001",
              "33043",
              "99950",
              "92101",
              "96701",
              "60601",
              "75201",
              "20260-0001"),
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  expect_no_message(qZIP(good_zips))
})

test_that("qBirthDate returns a message for bad birth dates", {
  bad_birthdates <-
    tibble(
      birth_date =
        c("3",
          "*",
          "AA/AA/AAAA",
          "11/11/92",
          "2/2/2022",
          "1/30/2001",
          "12/1/1963",
          "1970",
          "5/4",
          "10/31",
          "8/16",
          "12/5",
          # Future date
          "08/08/2099",
          # Non-existant date
          "02/29/1983"),
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  suppressMessages(
    invisible(
      capture.output(
        expect_message(
          qBirthDate(bad_birthdates, as.numeric(REF_CURRENT_SEASON)))
      )
    )
  )
})

test_that("qBirthDate does not return a message for good birth dates", {
  good_birthdates <-
    tibble(
      birth_date =
        c("01/01/1935",
          "02/29/1988",
          "04/05/1967",
          "10/10/2010",
          "11/30/1999",
          "12/31/2020"),
      source_file = "ZZ20250101.txt",
      file_size = 100
    )

  expect_no_message(qBirthDate(good_birthdates, as.numeric(REF_CURRENT_SEASON)))
})

test_that("qHuntMigBirds returns a message for bad hunt_mig_birds", {
  bad_hunt_mig_birds <-
    tibble(
      hunt_mig_birds = c("3", "*", "A", "11", "22"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qHuntMigBirds(bad_hunt_mig_birds))
      )
    )
  )
})

test_that("qHuntMigBirds does not return a message for good hunt_mig_birds", {
  good_hunt_mig_birds <-
    tibble(
      hunt_mig_birds = c("2", "2", "2"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  expect_no_message(qHuntMigBirds(good_hunt_mig_birds))
})

test_that("qRegistrationYear returns a message for bad registration_yr", {
  bad_reg_yr <-
    tibble(
      registration_yr = c("2020", "2021", "2022", "2023", "2024"),
      source_file = "ZZ20250101.txt",
      file_size = 100)

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qRegistrationYear(bad_reg_yr))
      )
    )
  )
})

test_that("qRegistrationYear does not return a message for good values", {
  good_reg_yr <-
    tibble(registration_yr = rep(REF_CURRENT_SEASON, 10),
           source_file = "ZZ20250101.txt",
           file_size = 100)

  expect_no_message(qRegistrationYear(good_reg_yr))
})

# bags --------------------------------------------------------------------

test_that("qBags returns a message for bad bags", {
  bad_bags <-
    bind_rows(
      DF_TEST_TINI_READ |>
        mutate(across(all_of(REF_FIELDS_BAG), ~"0")),
      DF_TEST_TINI_READ |>
        mutate(across(all_of(REF_FIELDS_BAG), ~NA)),
      DF_TEST_TINI_READ |>
        mutate(across(all_of(REF_FIELDS_BAG), ~"A"))
    )

  suppressMessages(
    invisible(
      capture.output(
        expect_message(qBags(bad_bags))
      )
    )
  )
})

test_that("qBags does not return a message for good bags", {
  expect_no_message(qBags(DF_TEST_TINI_READ))
})

test_that("zeroBagsMessage returns a message for zero bags", {
  bad_bags <-
    DF_TEST_TINI_READ |>
    mutate(across(all_of(REF_FIELDS_BAG), ~"0"))

  suppressMessages(
    invisible(
      capture.output(
        expect_message(zeroBagsMessage(bad_bags))
      )
    )
  )
})

test_that("zeroBagsMessage does not return a message for good bags", {
  expect_no_message(zeroBagsMessage(DF_TEST_TINI_READ))
})

test_that("naBagsMessage returns a message for NA bags", {
  bad_bags <-
    DF_TEST_TINI_READ |>
    mutate(across(all_of(REF_FIELDS_BAG), ~NA))

  suppressMessages(
    invisible(
      capture.output(
        expect_message(naBagsMessage(bad_bags))
      )
    )
  )
})

test_that("naBagsMessage does not return a message for good bags", {
  expect_no_message(naBagsMessage(DF_TEST_TINI_READ))
})

test_that("nonDigitBagsMessage returns a message for non-digit bags", {
  bad_bags <-
    DF_TEST_TINI_READ |>
    mutate(across(all_of(REF_FIELDS_BAG), ~"A"))

  suppressMessages(
    invisible(
      capture.output(
        expect_message(nonDigitBagsMessage(bad_bags))
      )
    )
  )
})

test_that("nonDigitBagsMessage does not return a message for good bags", {
  expect_no_message(nonDigitBagsMessage(DF_TEST_TINI_READ))
})

# permits -----------------------------------------------------------------

# inLinePermitDNHMessage
# permitFileBagsMessage
