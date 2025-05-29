# proof function ----------------------------------------------------------

test_that("errors field created", {

  test_proof <- proof(DF_TEST_TINI_DEDUPED, as.numeric(REF_CURRENT_SEASON))
  expect_false(is.null(test_proof$errors))
})

test_that("errors field contains errors", {

  test_proof <- proof(DF_TEST_TINI_DEDUPED, as.numeric(REF_CURRENT_SEASON))
  expect_false(nrow(filter(test_proof, is.na(errors))) == 0)
})

test_that("proof input and output have the same number of records", {

  test_proof <- proof(DF_TEST_TINI_DEDUPED, as.numeric(REF_CURRENT_SEASON))
  expect_equal(nrow(DF_TEST_TINI_DEDUPED), nrow(test_proof))
})

# test record -------------------------------------------------------------

test_that("test records fail proofing", {

  test_records <-
    tibble(
      firstname =
        c("TEST", "JIM", "INAUDIBLE", "BLANK", "USER", "TEST", "RESIDENT"),
      lastname =
        c("TEST", "INAUDIBLE", "LARSON", "O'HOULIHAN", "YU", "SMITH", "CRUZ")
    )

  good_records <-
    tibble(
      firstname =
        c("JANE", "JUDY", "JESSICA", "JULIA", "JOHN", "JACK", "RINAUDIBLE",
          "BLANKS", "DUSER", "CONTEST", "PRESIDENT"),
      lastname =
        c("DOE", "BLANK", "USER", "RESIDENT", "TEST", "TESTER", "MONROE",
          "SMITH", "PATEL", "PETERSON", "DEAN")
    )

  test_records_filtered <-
    rbind(good_records, test_records) |>
    filter(!!LOGIC_TEST_RECORD)

  expect_equal(test_records, test_records_filtered)
})

# title -------------------------------------------------------------------

test_that("good titles pass proofing", {
  good_titles <- tibble(title = c(NA, "1", "2", "0"))
  good_titles_filtered <- filter(good_titles, !title %in% REF_TITLES)

  expect_equal(nrow(good_titles_filtered), 0)
})

test_that("bad titles fail proofing", {
  bad_titles <- tibble(title = c("*", "%", "#", "!", "3", "10", "Z", "TH"))
  bad_titles_filtered <- filter(bad_titles, !title %in% REF_TITLES)

  expect_equal(nrow(bad_titles), nrow(bad_titles_filtered))
})

# firstname ---------------------------------------------------------------

test_that("good first names pass proofing", {
  good_first_names <-
    tibble(
      first_names =
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
          "JOHN PAUL GEORGE")
    )

  good_first_names_filtered <-
    filter(good_first_names, !str_detect(first_names, REGEX_FIRSTNAME))

  expect_equal(nrow(good_first_names_filtered), 0)
})

test_that("bad first names fail proofing", {
  bad_first_names <-
    tibble(
      first_names =
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
          "")
    )

  bad_first_names_filtered <-
    filter(bad_first_names, !str_detect(first_names, REGEX_FIRSTNAME))

  expect_equal(nrow(bad_first_names), nrow(bad_first_names_filtered))
})

# middle ------------------------------------------------------------------

test_that("good middle initials pass proofing", {
  good_middles <- tibble(middle = c(LETTERS, NA))

  good_middles_filtered <-
    good_middles |>
    filter(!middle %in% c(LETTERS, NA))

  expect_equal(nrow(good_middles_filtered), 0)
})

test_that("bad middle initials fail proofing", {
  bad_middles <- tibble(middle = c("*", "%", "#", "!", "3", "10", "TH"))

  bad_middles_filtered <-
    bad_middles |>
    filter(!middle %in% LETTERS)

  expect_equal(nrow(bad_middles), nrow(bad_middles_filtered))
})

# lastname ----------------------------------------------------------------

test_that("good last names pass proofing", {
  good_last_names <-
    tibble(
      last_names =
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
          "REYES DE LA BARRERA")
    )

  good_last_names_filtered <-
    filter(good_last_names, !str_detect(last_names, REGEX_LASTNAME))

  expect_equal(nrow(good_last_names_filtered), 0)
})

test_that("bad last names fail proofing", {
  bad_last_names <-
    tibble(
      last_names =
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
          "")
    )

  bad_last_names_filtered <-
    filter(bad_last_names, !str_detect(last_names, REGEX_LASTNAME))

  expect_equal(nrow(bad_last_names), nrow(bad_last_names_filtered))
})

# suffix ------------------------------------------------------------------

test_that("good suffixes pass proofing", {
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
        ))

  good_suffixes_filtered <-
    good_suffixes |>
    filter(!suffix %in% c(REF_SUFFIXES, NA))

  expect_equal(nrow(good_suffixes_filtered), 0)
})

test_that("bad suffixes fail proofing", {
  bad_suffixes <-
    tibble(
      suffix =
        c(tolower(REF_ROMAN_SUFFIXES), tolower(REF_ORDINAL_SUFFIXES),
          as.character(1:20), "s", "t", "h", "n", "d", "S", "T", "H", "N", "D",
          "ST", "ND", "RD", "TH", "iiii", "IIII", "VV", "VVV"))

  bad_suffixes_filtered <- filter(bad_suffixes, !suffix %in% REF_SUFFIXES)

  expect_equal(nrow(bad_suffixes), nrow(bad_suffixes_filtered))
})

# address -----------------------------------------------------------------

test_that("bad addresses fail proofing", {
  good_addresses <-
    tibble(
      address = c("966 American Holly Ln", "3 Bee Dr", "PO Box 9"))

  bad_addresses <-
    tibble(
      address = c("80 Fox |Dr", "4 Bear’s Pl"))

  bad_addresses_filtered <-
    rbind(good_addresses, bad_addresses) |>
    filter(str_detect(address, REGEX_BAD_ADDRESS))

  expect_equal(bad_addresses, bad_addresses_filtered)
})

# city --------------------------------------------------------------------

test_that("bad city names fail proofing", {
  good_city_names <-
    tibble(
      city = c("Los Angeles",
               "Annapolis",
               "St. Petersburg",
               "Coeur d'Alene",
               "Dover-Foxcroft",
               "Roy",
               "La Plata"))

  bad_city_names <-
    tibble(
      city = c("Wilming$ton",
               "Saint-Louis-du-Ha! Ha!",
               "0maha",
               "St..Louis",
               "Po"))

  bad_city_names_filtered <-
    rbind(good_city_names, bad_city_names) |>
    filter(!str_detect(city, REGEX_CITY))

  expect_equal(bad_city_names, bad_city_names_filtered)
})

# state -------------------------------------------------------------------

test_that("good states pass proofing", {
  good_states <-
    tibble(
      state =
        c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI",
          "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI",
          "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC",
          "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
          "VT", "VA", "WA", "WV", "WI", "WY", "DC", "AS", "GU", "MP", "PR",
          "VI", "UM", "MH", "FM", "PW", "AA", "AE", "AP", "AB", "BC", "MB",
          "NB", "NL", "NS", "NT", "NU", "ON", "PE", "PQ", "QC", "SK", "YT"))

  good_states_filtered <-
    good_states |>
    filter(!state %in% REF_USA_CANADA)

  expect_equal(nrow(good_states_filtered), 0)
})

test_that("bad states fail proofing", {
  bad_states <-
    tibble(
      state = c("la", "De", "MX", "CN", "00", "ZZ", " ", "  ", "_", "!!", "*"))

  bad_states_filtered <-
    bad_states |>
    filter(!state %in% REF_USA_CANADA)

  expect_equal(nrow(bad_states), nrow(bad_states_filtered))
})

# zip code ----------------------------------------------------------------


# birth_date --------------------------------------------------------------


# hunt_mig_birds ----------------------------------------------------------

test_that("good hunt_mig_birds values pass proofing", {
  good_hunt_mig_birds <- tibble(hunt_mig_birds = c("1", "2", "2", "2", "1"))

  good_hunt_mig_birds_filtered <-
    good_hunt_mig_birds |>
    filter(!hunt_mig_birds %in% REF_HUNT_MIG_BIRDS)

  expect_equal(nrow(good_hunt_mig_birds_filtered), 0)
})

test_that("bad hunt_mig_birds values fail proofing", {
  bad_hunt_mig_birds <- tibble(hunt_mig_birds = c("3", "*", "A", "11", "22"))

  bad_hunt_mig_birds_filtered <-
    bad_hunt_mig_birds |>
    filter(!hunt_mig_birds %in% REF_HUNT_MIG_BIRDS)

  expect_equal(bad_hunt_mig_birds, bad_hunt_mig_birds_filtered)
})

# registration_yr ---------------------------------------------------------


# email -------------------------------------------------------------------

test_that("good email addresses pass proofing", {
  good_emails <-
    tibble(
      # Fake email addresses that are formatted correctly
      email = c(
        "TonyStark@starkindustries.com",
        "p3pperp0tts@cox.net",
        "peter_parker@gmail.com",
        "blackwidow@posteo.de",
        "BlackPanther@github.io",
        "shuri@wakanda.gov",
        "thor@comcast.net",
        "loki@live.com",
        "mobius@comet.com",
        "bucky@GMAIL.COM",
        "Captain-America@us.army.mil",
        "Hulk.Smash.00@hotmail.com",
        "H.A.W.K.E.Y.E@GMAIL.COM",
        "antman@charter.net",
        "wasp@EARTHLINK.NET",
        "nickFury_@protonmail.com",
        "GAMORA@VERIZON.NET",
        "star-lord88@duck.com",
        "YondU@bellsouth.net",
        "drax@attacus.com",
        "nebula@verizonhorizon.edu",
        "mantis@yahoo.com",
        "mantis@yahoo.co.uk",
        "mantis@yahoo.fr",
        "mantis@yahoo.es",
        "mantis@yahoo.ca",
        "mantis@yahoo.de",
        "mantis@hotmail.com",
        "mantis@hotmail.co.uk",
        "mantis@hotmail.fr",
        "mantis@hotmail.es",
        "mantis@hotmail.ca",
        "mantis@hotmail.de",
        "kraglin@ravagers.space",
        "HowardTheDuck@ducks.org",
        "groot+hunting@pm.me",
        "ROCK3T+hip@protonmail.ch",
        "CaptainMarvel90@proton.me",
        "moon.knight@icloud.com",
        "Gambit@att.net",
        "W0-lv3R.ine+xm3n@Xavier.School.org",
        "nick.cage@aol.com",
        "DannyRand@msn.com",
        "JessicaJones@me.com",
        "matt-murdock+law@outlook.com",
        "rEDgUARDIAN@sbcglobal.net",
        "yelena@ymail.com",
        "bob@mac.com"
      ))

  good_emails_filtered <-
    good_emails |>
    proofBadEmails()

  expect_equal(nrow(good_emails_filtered), 0)
})

test_that("bad email addresses fail proofing", {
  bad_emails <-
    tibble(
      # Fake email addresses that are formatted incorrectly
      email = c(
        # Incomplete
        "@",
        "wanda@com",
        "wanda@net",
        "wanda@io",
        "wanda@",
        "wanda",
        "@wanda",
        "@wanda.com",
        "agatha@gmail",
        "agatha@yahoo",
        "agatha@hotmail",
        "agatha@aol",
        "agatha@icloud",
        "agatha@comcast",
        "agatha@outlook",
        "agatha@sbcglobal",
        "agatha@att",
        "agatha@msn",
        "agatha@live",
        "agatha@bellsouth",
        "agatha@charter",
        "agatha@ymail",
        "agatha@me",
        "agatha@verizon",
        "agatha@cox",
        "agatha@earthlink",
        "agatha@protonmail",
        "agatha@pm",
        "agatha@duck",
        "agatha@ducks",
        "agatha@mail",
        "thanos@gmailcom",
        "thanos@comcastnet",
        "thanos@edu",
        "thanos@fwsgov",
        "thanos@ducksorg",
        "thanos@navymil",
        "thanos@usnavymil",
        "thanos@usafmil",
        "thanos@mailmil",
        "thanos@armymil",
        "thanos@usarmymil",
        "thanos@usacearmymil",
        # Too many @s
        "@@",
        "BruceBanner@@gmail.com",
        "BruceBanner@@@gmail.com",
        # Invalid special character
        "Tony`Stark@starkindustries.com",
        "Tony!Stark@starkindustries.com",
        "Tony@Stark@starkindustries.com",
        "Tony#Stark@starkindustries.com",
        "Tony$Stark@starkindustries.com",
        "Tony%Stark@starkindustries.com",
        "Tony^Stark@starkindustries.com",
        "Tony&Stark@starkindustries.com",
        "Tony*Stark@starkindustries.com",
        "Tony(Stark@starkindustries.com",
        "Tony)Stark@starkindustries.com",
        "Tony{Stark@starkindustries.com",
        "Tony}Stark@starkindustries.com",
        "Tony|Stark@starkindustries.com",
        "Tony[Stark@starkindustries.com",
        "Tony]Stark@starkindustries.com",
        "Tony=Stark@starkindustries.com",
        "TonyStark@stark`industries.com",
        "TonyStark@stark!industries.com",
        "TonyStark@stark@industries.com",
        "TonyStark@stark#industries.com",
        "TonyStark@stark$industries.com",
        "TonyStark@stark%industries.com",
        "TonyStark@stark^industries.com",
        "TonyStark@stark&industries.com",
        "TonyStark@stark*industries.com",
        "TonyStark@stark(industries.com",
        "TonyStark@stark)industries.com",
        "TonyStark@stark{industries.com",
        "TonyStark@stark}industries.com",
        "TonyStark@stark|industries.com",
        "TonyStark@stark[industries.com",
        "TonyStark@stark]industries.com",
        "TonyStark@stark=industries.com",
        # Obfuscative addresses
        "none@gmail.com",
        "no@gmail.com",
        "na@gmail.com",
        "not@gmail.com",
        "non@gmail.com",
        "nomail@gmail.com",
        "noemail@gmail.com",
        "noreply@gmail.com",
        "customer@gmail.com",
        "unknown@gmail.com",
        "notprovided@gmail.com",
        "spiderman@none.com",
        "spiderman@no.com",
        "spiderman@na.org",
        "spiderman@tpw.org",
        "spiderman@twp.org",
        "spiderman@example.com",
        "yondu@guerillamail.com",
        # Too long
        "peter______________________________________________________________________________parker@example.com",
        # Consecutive periods
        "Dead..Pool@comcast.net",
        "DeadPool@comc..ast.net",
        # Leading period in local part
        ".thor@comcast.net",
        # Leading period in domain
        "loki@.comcast.net",
        # Ending period in local part
        "star-lord88.@duck.com",
        # Ending period
        "CaptainMarvel90@gmail.com.",
        # Hyphen as first character in domain
        "Hulk.Smash.00@-hotmail.com",
        # Popular domain name with non-matching top-level domain
        "moon.knight@icloud.net",
        "moon.knight@icloud.org",
        "moon.knight@att.com",
        "moon.knight@att.org",
        "moon.knight@comcast.com",
        "moon.knight@comcast.org",
        "moon.knight@gmail.net",
        "moon.knight@gmail.edu",
        "moon.knight@gmail.org",
        "moon.knight@cox.com",
        "moon.knight@yahoo.gov",
        "moon.knight@live.org",
        "moon.knight@hotmail.net",
        "moon.knight@charter.com",
        "moon.knight@EARTHLINK.com",
        "moon.knight@aol.org",
        "moon.knight@msn.biz",
        "moon.knight@me.io",
        "moon.knight@outlook.net",
        "moon.knight@sbcglobal.com",
        "moon.knight@ymail.gov",
        "moon.knight@mac.edu",
        # Domain typo
        "p3pperp0tts@yahooo.com",
        "p3pperp0tts@attt.net",
        "p3pperp0tts@gmaill.com",
        "p3pperp0tts@gmai.com",
        "p3pperp0tts@gamil.com",
        "p3pperp0tts@gmial.com",
        "p3pperp0tts@gmal.com",
        "p3pperp0tts@gmil.com",
        "p3pperp0tts@gail.com",
        "p3pperp0tts@gmali.com",
        "p3pperp0tts@gmall.com",
        "p3pperp0tts@glail.com",
        "p3pperp0tts@gmaim.com",
        "p3pperp0tts@gamil.com",
        "p3pperp0tts@gimal.com",
        "p3pperp0tts@gmai.com",
        "p3pperp0tts@gmaii.com",
        # Top-level domain typo
        "vision@yahoo.com.com",
        "vision@yahoo.comcom",
        "vision@yahoo.con",
        "vision@yahoo.ccom",
        "vision@yahoo.coom",
        "vision@yahoo.comm",
        "vision@yahoo.c0m",
        "vision@yahoo.ocm",
        "vision@yahoo.cm",
        "vision@yahoo.om",
        "vision@yahoo.cim",
        "vision@yahoo.common",
        "vision@protonmail.co",
        "vision@VERIZON.ET",
        "vision@duck.kcom",
        "vision@bellsouth.nnet",
        "vision@pm.no",
        "vision@protonmail.choo",
        "vision@ducks.borg",
        "vision@proton.m3",
        "vision@comcast.ne",
        "vision@gmail.co",
        # Too short/bad top-level domain
        "a@b.c",
        "0@b.c",
        "a@0.c",
        "a@b.0"
      ))

  bad_emails_filtered <-
    bad_emails |>
    proofBadEmails()

  expect_equal(nrow(bad_emails), nrow(bad_emails_filtered))
})
