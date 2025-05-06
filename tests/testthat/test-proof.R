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

# middle ------------------------------------------------------------------

test_that("good middle initials pass proofing", {
  good_middles <- tibble(middle = LETTERS)

  good_middles_filtered <-
    good_middles |>
    filter(!str_detect(middle, "^[A-Z]{1}$"))

  expect_equal(nrow(good_middles_filtered), 0)
})

test_that("bad middle initials fail proofing", {
  bad_middles <- tibble(middle = c("*", "%", "#", "!", "3", "10", "TH"))

  bad_middles_filtered <-
    bad_middles |>
    filter(!str_detect(middle, "^[A-Z]{1}$"))

  expect_equal(nrow(bad_middles), nrow(bad_middles_filtered))
})

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
