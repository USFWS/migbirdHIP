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


# middle ------------------------------------------------------------------

test_that("good middle initials pass proofing", {
  good_middles <- tibble(middle = LETTERS)

  good_middles_filtered <-
    good_middles |>
    filter(!middle %in% LETTERS)

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


# suffix ------------------------------------------------------------------

test_that("good suffixes pass proofing", {
  good_suffixes <- tibble(suffix = REF_SUFFIXES)
  good_suffixes_filtered <- filter(good_suffixes, !suffix %in% REF_SUFFIXES)

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


# city --------------------------------------------------------------------

test_that("bad city names fail proofing", {
  good_city_names <-
    tibble(
      city = c("Los Angeles", "Annapolis", "St. Petersburg", "Coeur d'Alene",
               "Dover-Foxcroft", "St..Louis"))

  bad_city_names <-
    tibble(
      city = c("Wilming$ton", "Saint-Louis-du-Ha! Ha!", "0maha"))

  bad_city_names_filtered <-
    rbind(good_city_names, bad_city_names) |>
    filter(str_detect(city, REGEX_CITY))

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
        "spiderman@yahoo.com",
        "peter_parker@gmail.com",
        "blackwidow@posteo.de",
        "thor@comcast.net",
        "star-lord88@duck.com",
        "CaptainMarvel90@gmail.com",
        "Hulk.Smash.00@hotmail.com",
        "H.A.W.K.E.Y.E@GMAIL.COM",
        "moon.knight@icloud.com",
        "Gambit@att.net",
        "BlackPanther@github.io",
        "groot+hunting@gmail.com",
        "ROCK3T+hip@icloud.com",
        "_nickFury_@hotmail.com",
        "W0-lv3R.ine+xm3n@Xavier.School.org",
        "bucky@GMAIL.COM",
        "Captain-America@us.army.mil"
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
        "BruceBanner@@gmail.com",
        "BruceBanner@@@gmail.com",
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
        "peter______________________________________________________________________________parker@example.com",
        "Dead..Pool@comcast.net",
        "DeadPool@comc..ast.net",
        ".thor@comcast.net",
        "loki@.comcast.net",
        "star-lord88.@duck.com",
        "CaptainMarvel90@gmail.com.",
        "Hulk.Smash.00@-hotmail.com",
        "yondu@guerillamail.com",
        "moon.knight@icloud.net",
        "moon.knight@icloud.org",
        "moon.knight@att.com",
        "moon.knight@att.org",
        "moon.knight@comcast.com",
        "moon.knight@comcast.org",
        "moon.knight@gmail.net",
        "moon.knight@gmail.edu",
        "moon.knight@gmail.org",
        "moon.knight@gmail.co",
        "wanda@com",
        "wanda@net",
        "wanda@io",
        "wanda@",
        "wanda",
        "@wanda",
        "@wanda.com",
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
        "thanos@usacearmymil"
      ))

  bad_emails_filtered <-
    bad_emails |>
    proofBadEmails()

  expect_equal(nrow(bad_emails), nrow(bad_emails_filtered))
})
