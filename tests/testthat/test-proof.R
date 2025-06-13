# proof function ----------------------------------------------------------

test_that("errors field created", {

  test_proof <- proof(DF_TEST_TINI_DEDUPED, as.numeric(REF_CURRENT_SEASON))
  expect_false(is.null(test_proof$errors))
})

test_that("errors field contains errors", {

  error_data <-
    DF_TEST_TINI_DEDUPED |>
    mutate(zip = "000000")

  test_proof <- proof(error_data, as.numeric(REF_CURRENT_SEASON))
  expect_true(nrow(filter(test_proof, is.na(errors))) == 0)
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
        "Rhodes@q.com",
        "JARVIS@verizon.net",
        "p3pperp0tts@cox.net",
        "peter_parker@gmail.com",
        "blackwidow@posteo.de",
        "BlackPanther@github.io",
        "shuri@wakanda.gov",
        "thor@comcast.net",
        "loki@live.com",
        "mobius@twc.com",
        "bucky@GMAIL.COM",
        "Captain-America@us.army.mil",
        "falcon@windstream.net",
        "Hulk.Smash.00@hotmail.com",
        "H.A.W.K.E.Y.E@aim.com",
        "antman@charter.net",
        "wasp@EARTHLINK.NET",
        "nickFury_@protonmail.com",
        "GAMORA@VERIZON.NET",
        "star-lord88@duck.com",
        "ego@earthlink.net",
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
        "mantis@hot.dog",
        "kraglin@ravagers.space",
        "HowardTheDuck@ducks.org",
        "groot+hunting@pm.me",
        "ROCK3T+hip@protonmail.ch",
        "rocket@rocketmail.com",
        "CaptainMarvel90@proton.me",
        "MsMarvel@suddenlink.net",
        "moon.knight@icloud.com",
        "Gambit@att.net",
        "W0-lv3R.ine+xm3n@Xavier.School.org",
        "charles@mtsinai.k12.ny.us",
        "luke.cage@aol.com",
        "misty@centurylink.net",
        "DannyRand@msn.com",
        "JessicaJones@me.com",
        "matt-murdock+law@outlook.com",
        "foggy@mail.com",
        "rEDgUARDIAN@sbcglobal.net",
        "yelena@ymail.com",
        "bob@mac.com",
        "USagent@frontiernet.net",
        "ghost@frontier.com",
        "mr.fantastic@embarqmail.com",
        "mrs.fantastic@centurytel.net",
        "Thing@roadrunner.com",
        "FlameOn@juno.com",
        "ShangChi@fakefoo.com"
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
        "none123@gmail.com",
        "noneabc@gmail.com",
        "noen@gmail.com",
        "nnon@gmail.com",
        "onoene@gmail.com",
        "nnnnnne@gmail.com",
        "nonen@gmail.com",
        "nonoe@gmail.com",
        "nnoe@gmail.com",
        "onoen@gmail.com",
        "noneo@gmail.com",
        "nonne@gmail.com",
        "nnone@gmail.com",
        "nnno@gmail.com",
        "onon@gmail.com",
        "onoe@gmail.com",
        "nonn@gmail.com",
        "noeone@gmail.com",
        "nonneee@gmail.com",
        "nonnee@gmail.com",
        "nonoeone@gmail.com",
        "onne@gmail.com",
        "nononon@gmail.com",
        "oonono@gmail.com",
        "oooo@gmail.com",
        "ooooooooooooooooooooooooooooo@gmail.com",
        "nnnn@gmail.com",
        "nnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnn@gmail.com",
        "eeee@gmail.com",
        "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee@gmail.com",
        "none+@gmail.com",
        "none.none@gmail.com",
        "none.12@gmail.com",
        "none.no@gmail.com",
        "noneee@gmail.com",
        "noneeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee@gmail.com",
        "nonenone@gmail.com",
        "nonenonenone@gmail.com",
        "none12@gmail.com",
        "nonenone12@gmail.com",
        "0none@gmail.com",
        "1nonenone@gmail.com",
        "nope@gmail.com",
        "nopenope@gmail.com",
        "nopenopenope@gmail.com",
        "nope12@gmail.com",
        "nopenope12@gmail.com",
        "0nope@gmail.com",
        "1nopenope@gmail.com",
        "null@gmail.com",
        "nullnull@gmail.com",
        "nullnullnull@gmail.com",
        "null12@gmail.com",
        "nullnull12@gmail.com",
        "0null@gmail.com",
        "1nullnull@gmail.com",
        "no@gmail.com",
        "nono@gmail.com",
        "no523@gmail.com",
        "nono4@gmail.com",
        "5no@gmail.com",
        "67nonono@gmail.com",
        "na@gmail.com",
        "nananana@gmail.com",
        "na4234@gmail.com",
        "8na@gmail.com",
        "not@gmail.com",
        "1not@gmail.com",
        "not48576023421908@gmail.com",
        "non@gmail.com",
        "0non@gmail.com",
        "non12345@gmail.com",
        "nomail@gmail.com",
        "7nomail@gmail.com",
        "nomail111@gmail.com",
        "noemai@gmail.com",
        "noemial@gmail.com",
        "nothanks@gmail.com",
        "nothankyou@gmail.com",
        "noemail@gmail.com",
        "555noemail@gmail.com",
        "noemail485@gmail.com",
        "no.email@gmail.com",
        "555no.email@gmail.com",
        "no.email485@gmail.com",
        "no-email@gmail.com",
        "555no-email@gmail.com",
        "no-email485@gmail.com",
        "noreply@gmail.com",
        "13noreply@gmail.com",
        "noreply45@gmail.com",
        "noemailnone@gmail.com",
        "nonemail@gmail.com",
        "noneemail@gmail.com",
        "noneemal@gmail.com",
        "noneeamil@gmail.com",
        "noneemsil@gmail.com",
        "noneya@gmail.com",
        "noneyabusiness1975@yahoo.com",
        "noneyoubznes@gmail.com",
        "nanone@gmail.com",
        "notavailable@gmail.com",
        "notavalable@gmail.com",
        "notavalaible@gmail.com",
        "none_available@gmail.com",
        "noneavailable@gmail.com",
        "notapplicable@gmail.com",
        "notapplic@gmail.com",
        "noneonfile@gmail.com",
        "noneprovide@gmail.com",
        "noneprovided@gmail.com",
        "noneofyourbusiness@gmail.com",
        "nopenone@gmail.com",
        "gotnone@gmail.com",
        "1havenone@gmail.com",
        "nopeaintnone@gmail.com",
        "33nopeaintgotnone44@gmail.com",
        "noneusa@gmail.com",
        "nonetext@gmail.com",
        "nonetexas@gmail.com",
        "none-tpw-texas@gmail.com",
        "none-tpwd.texas@gmail.com",
        "none2tpwd@texas.gov",
        "nonee.tpw@texas.gov",
        "nonetwpd.texas@gmail.com",
        "nonegiven@gmail.com",
        "noneyet@gmail.com",
        "noneatall@gmail.com",
        "nonewalmart@gmail.com",
        "subscribe@gmail.com",
        "customer@gmail.com",
        "done@gmail.com",
        "fu@gmail.com",
        "abc@gmail.com",
        "abcabc@gmail.com",
        "abcabcabc@gmail.com",
        "abc123@gmail.com",
        "123abc@gmail.com",
        "xyz@gmail.com",
        "xyzxyz@gmail.com",
        "xyzxyzxyz@gmail.com",
        "xyz123@gmail.com",
        "123xyz@gmail.com",
        "unknown@gmail.com",
        "48unknown@gmail.com",
        "unknown48576023421908@gmail.com",
        "notprovided@gmail.com",
        "0notprovided@gmail.com",
        "notprovided12@gmail.com",
        "fake@gmail.com",
        "0fake@gmail.com",
        "00fake@gmail.com",
        "123fake@gmail.com",
        "1234fake@gmail.com",
        "12345fake@gmail.com",
        "48576023421908fake@gmail.com",
        "fake0@gmail.com",
        "fake12@gmail.com",
        "fake123@gmail.com",
        "fake1234@gmail.com",
        "fake12345@gmail.com",
        "fake48576023421908@gmail.com",
        "fake.fake@gmail.com",
        "3434fake.fake@gmail.com",
        "fake.fake5656@gmail.com",
        "fakeemail@gmail.com",
        "1fakeemail@gmail.com",
        "fakeemail345@gmail.com",
        "notvalidemail@gmail.com",
        "1notvalidemail@gmail.com",
        "notvalidemail345@gmail.com",
        "donotreply@gmail.com",
        "1donotreply@gmail.com",
        "donotreply345@gmail.com",
        "noemailaddress@gmail.com",
        "00noemailaddress@gmail.com",
        "noemailaddress7234@gmail.com",
        "noone@gmail.com",
        "470noone@gmail.com",
        "noone1236488@gmail.com",
        "www.no@gmail.com",
        "www.none@gmail.com",
        "123@gmail.com",
        "1234@gmail.com",
        "12345@gmail.com",
        "123456@gmail.com",
        "1234567@gmail.com",
        "12345678@gmail.com",
        "123456789@gmail.com",
        "0@gmail.com",
        "00@gmail.com",
        "000@gmail.com",
        "0000@gmail.com",
        "00000@gmail.com",
        "000000@gmail.com",
        "0000000000@gmail.com",
        "00000000000000000000@gmail.com",
        "000000000000000000000000000000@gmail.com",
        "0000000000000000000000000000000000000000@gmail.com",
        "00000000000000000000000000000000000000000000000000@gmail.com",
        "00000000000000000000000000000000000000000000000000000000000@gmail.com",
        "customerrefused@gmail.com",
        "3customerrefused@gmail.com",
        "customerrefused68@gmail.com",
        "refused@gmail.com",
        "1refused@gmail.com",
        "refused333@gmail.com",
        "refusedemail@gmail.com",
        "2refusedemail@gmail.com",
        "refusedemail555@gmail.com",
        "nocustomer@gmail.com",
        "3nocustomer@gmail.com",
        "nocustomer777@gmail.com",
        "www.tpwd@gmail.com",
        "www.none.tpwd@gmail.com",
        "texastpw@gmail.com",
        "texastpwd@gmail.com",
        "tpwtexas@gmail.com",
        "tpwdtexas@gmail.com",
        "tpw.texas@gmail.com",
        "tpwd.texas@gmail.com",
        "tpw.texas.gov@gmail.com",
        "tpwd.texas.gov@gmail.com",
        "tpw.gov@gmail.com",
        "tpwd.gov@gmail.com",
        "notpw@gmail.com",
        "no.tpw@gmail.com",
        "natpw@gmail.com",
        "na.tpw@gmail.com",
        "notpwd@gmail.com",
        "no.tpwd@gmail.com",
        "natpwd@gmail.com",
        "na.tpwd@gmail.com",
        "nontpw@gmail.com",
        "non-tpw@gmail.com",
        "non.tpw@gmail.com",
        "nontpwd@gmail.com",
        "non-tpwd@gmail.com",
        "non.tpwd@gmail.com",
        "nontpwd.texas@gmail.com",
        "nonetpw@gmail.com",
        "none.tpw@gmail.com",
        "none.tpwd@gmail.com",
        "none.tpwd.texas@gmail.com",
        "none.tpw.texas@gmail.com",
        "5nonetpw@gmail.com",
        "nonetpw44@gmail.com",
        "nonetpwd@gmail.com",
        "4nonetpwd@gmail.com",
        "nonetpwd888@gmail.com",
        "non.twd@texas.gov",
        "nonetwp@texas.gov",
        "nontwpd@texas.gov",
        "tpwd@gmail.com",
        "5tpwd@gmail.com",
        "tpwd22222@gmail.com",
        "twd659@gmail.com",
        "twd@gmail.com",
        "none.twd@gmail.com",
        "twd.none@yahoo.com",
        "novalid@gmail.com",
        "6novalid@gmail.com",
        "novalid345@gmail.com",
        "invalid@gmail.com",
        "7invalid@gmail.com",
        "invalid6543@gmail.com",
        "nonvalid@gmail.com",
        "8nonvalid@gmail.com",
        "nonvalid7421@gmail.com",
        "notvalidemail@gmail.com",
        "9notvalidemail@gmail.com",
        "notvalidemail245234578@gmail.com",
        "notreply@gmail.com",
        "dont@gmail.com",
        "name@gmail.com",
        "55name@gmail.com",
        "johndoe@gmail.com",
        "john.doe@walmart.com",
        "janedoe@yahoo.com",
        "jane.doe@msn.com",
        "notme@gmail.com",
        "notyou@gmail.com",
        "notagain@gmail.com",
        "notemail@gmail.com",
        "nopenottoday@gmail.com",
        "nottoday@gmail.com",
        "nottotday@gmail.com",
        "nottoay@gmail.com",
        "notone@gmail.com",
        "nopeemail@gmail.com",
        "nopemail44@gmail.com",
        "notinterested@gmail.com",
        # Obfuscative domain
        "yondu@none.com",
        "yondu@no.com",
        "yondu@na.org",
        "yondu@tpw.org",
        "yondu@twp.org",
        "yondu@twd.org",
        "yondu@twpd.org",
        "yondu@twdp.org",
        "yondu@twd.gov",
        "yondu@twdp.com",
        "yondu@tx.com",
        "yondu@example.com",
        "yondu@test.com",
        "yondu@spambog.com",
        "yondu@guerillamail.com",
        "yondu@fake.com",
        "yondu@email-fake.com",
        "yondu@temp-mail.com",
        "yondu@www.com",
        "yondu@spam.com",
        "yondu@junk.com",
        "yondu@1999gmail.com",
        "yondu@48yahoo.com",
        "yondu@null.com",
        "yondu@dnr.com",
        "yondu@dnr.gov",
        "yondu@that.com",
        "yondu@gov.com",
        "yondu@done.com",
        "yondu@customer.com",
        # Fake emails
        "email@email.com",
        "email@gmail.com",
        "email@yahoo.com",
        "email@aol.com",
        "email@mail.com",
        "email@me.com",
        "email@hotmail.com",
        "bob@bob.com",
        "n@o.no",
        "n@a.com",
        "n@o.com",
        "f@u.com",
        "x@y.com",
        "me@u.com",
        # Repeated letter fake emails
        "gggggggggggggggg@g.com",
        "q@q.com",
        "yyyyyyyyyyyyyyyyyyyyyy@y.com",
        "zz@z.com",
        "gg@gg.com",
        "yyy@yy.com",
        "ii@iii.com",
        "x@xx.com",
        "111@1.com",
        "2@22.com",
        "22@22.com",
        "ggggggggg@g.net",
        "zz@z.edu",
        "q@q.gov",
        "yyyyyyyyyyyyyyyyyyyyyy@y.me",
        "zz@z.io",
        "2@22.mil",
        "22@22.org",
        "1@1.biz",
        "oo@ooooo.us",
        "44@444.co",
        # Fake walmart emails
        "111@walmart.com",
        # Fake tx emails
        "johnsmith@tpwd.texas.gov",
        "nine@tpwd.texas.gov",
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
        "p3pperp0tts@yahoooooooooooo.com",
        "p3pperp0tts@ahoo.com",
        "p3pperp0tts@yhoo.com",
        "p3pperp0tts@yaho.com",
        "p3pperp0tts@yahoh.com",
        "p3pperp0tts@yahohh.com",
        "p3pperp0tts@yyahoo.com",
        "p3pperp0tts@ayahoo.com",
        "p3pperp0tts@attt.net",
        "p3pperp0tts@attttttttttt.net",
        "p3pperp0tts@aatt.net",
        "p3pperp0tts@at.net",
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
        "p3pperp0tts@iclould.com",
        "p3pperp0tts@icoud.com",
        "p3pperp0tts@icould.com",
        "p3pperp0tts@spcglobal.net",
        "p3pperp0tts@sbcgobal.net",
        "p3pperp0tts@sbcglobel.net",
        "p3pperp0tts@sbcgloble.net",
        "p3pperp0tts@sbcgolbal.net",
        "p3pperp0tts@sbcglobe.net",
        "p3pperp0tts@sbcglobl.net",
        "p3pperp0tts@sbcgloabl.net",
        "p3pperp0tts@sbcgloabal.net",
        "p3pperp0tts@sbcgloal.net",
        "p3pperp0tts@sbcgobel.net",
        "p3pperp0tts@sbcglbal.net",
        "p3pperp0tts@sbcglob.net",
        "p3pperp0tts@sbcgoble.net",
        "p3pperp0tts@sbclobal.net",
        "p3pperp0tts@sbc.gobal.net",
        "p3pperp0tts@sbcglabal.net",
        "p3pperp0tts@sbcglibal.net",
        "p3pperp0tts@sbcgllobal.net",
        "p3pperp0tts@sbcgloba.net",
        "p3pperp0tts@sbcglobale.net",
        "p3pperp0tts@sbcglobol.net",
        "p3pperp0tts@sbcglobsl.net",
        "p3pperp0tts@concast.net",
        "p3pperp0tts@commcast.net",
        "p3pperp0tts@comacast.net",
        "p3pperp0tts@cmcast.net",
        "p3pperp0tts@compcast.net",
        "p3pperp0tts@conmcast.net",
        "p3pperp0tts@c0mcast.net",
        "p3pperp0tts@comcst.net",
        "p3pperp0tts@comacst.net",
        # Top-level domain typo
        "vision@yahoo.com.com.com",
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
        "vision@g.mail",
        "vision@hot.mail",
        "vision@ducks.or",
        "vision@comcast.ney",
        "vision@netscape.come",
        "vision@gmail.comb",
        "vision@gmai.coml",
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
