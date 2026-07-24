test_that("bagCheck works", {

  suppressMessages(bags_checked <- bagCheck(DF_TEST_MINI))

  expect_true(nrow(bags_checked) > 1)
})

# The following tests for bagCheck() were generated with AI assistance using
# Claude Opus 4.8 in Perplexity on July 22, 2026.

# If every row is an in-line permit, bagCheck filters them all out and reaches
# the else branch.
test_that("bagCheck messages 'No bag abnormalities detected.' on clean data", {
  inline_only <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(
      dl_state = "OR",
      ducks_bag = "0", geese_bag = "0", dove_bag = "0", woodcock_bag = "0",
      coots_snipe = "0", rails_gallinules = "0", cranes = "0",
      band_tailed_pigeon = "2", brant = "0", seaducks = "0")

  expect_message(bagCheck(inline_only), "No bag abnormalities detected\\.")
})

# Assertions on a known-bad bag value. A single IA record (IA is neither a
# permit-file state nor an in-line permit state) with every bag field set to "9"
# (never an expected value) must flag every species. We assert the specific
# flagged dl_state/spp/bad_bag_value and the summarizeBadBags proportion
# arithmetic: 1 bad of 1 total => "100%".
test_that("bagCheck flags a known bad bag value with correct proportion", {
  bad_one <-
    DF_TEST_MINI |>
    dplyr::slice_head(n = 1) |>
    dplyr::mutate(
      dl_state = "IA",
      dplyr::across(dplyr::all_of(migbirdHIP:::REF_FIELDS_BAG), \(x) "9"))

  out <- suppressMessages(bagCheck(bad_one))

  expect_s3_class(out, "data.frame")
  expect_true(all(
    c("dl_state", "spp", "bad_bag_value", "n", "proportion") %in% names(out)))
  # Every one of the 10 species bag fields is bad.
  expect_equal(nrow(out), length(migbirdHIP:::REF_FIELDS_BAG))

  ducks_row <- out[out$spp == "ducks_bag", ]
  expect_equal(nrow(ducks_row), 1)
  expect_identical(ducks_row$dl_state, "IA")
  expect_identical(ducks_row$bad_bag_value, "9")
  expect_equal(ducks_row$n, 1L)
  # summarizeBadBags: round(1/1, 2) * 100 => "100%"
  expect_identical(ducks_row$proportion, "100%")
})
