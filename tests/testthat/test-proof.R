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
