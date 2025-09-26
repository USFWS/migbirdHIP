test_that("failProofed works", {
  expect_error(failProofed(DF_TEST_MINI))

  test_failproofed <- DF_TEST_MINI |> mutate(errors = NA)
  expect_no_error(failProofed(test_failproofed))
})

test_that("failState works", {
  expect_error(failState("A"))
  expect_error(failState("MX"))
  expect_error(failState("SMITH"))
  expect_error(failState("XT"))
  expect_error(failState("Arizona"))
  expect_error(failState("2019"))
  expect_error(failState(2019))
  expect_error(failState(2024.5))

  for (i in seq_along(REF_ABBR_49_STATES)) {
    expect_no_error(failState(REF_ABBR_49_STATES[i]))
  }
})

test_that("failTF works", {
  expect_error(failTF("A"))
  expect_error(failTF("2019"))
  expect_error(failTF(2019))
  expect_error(failTF(2024.5))
  expect_no_error(failTF(TRUE))
  expect_no_error(failTF(FALSE))
  expect_no_error(failTF(T))
  expect_no_error(failTF(F))
})

test_that("failYear works", {
  expect_error(failYear("A"))
  expect_error(failYear("2019"))
  expect_error(failYear(2019))
  expect_error(failYear(2024.5))
  expect_error(failYear(2030))
  expect_no_error(failYear(as.numeric(REF_CURRENT_SEASON)))
})

test_that("failBTPI works", {
  good_btpi <-
    tibble::tibble(
      record_type = c("HIP", "HIP", "HIP"),
      ducks_bag = c(0, 0, 0),
      geese_bag = c(0, 0, 0),
      dove_bag = c(2, 1, 2),
      woodcock_bag = c(0, 0, 0),
      coots_snipe = c(0, 0, 0),
      rails_gallinules = c(0, 0, 0),
      cranes = c(0, 0, 0),
      band_tailed_pigeon = c(2, 2, 2),
      brant = c(0, 0, 0),
      seaducks = c(0, 0, 0))

  expect_error(failBTPI(tibble::tibble(record_type = c("PMT", "PMT", "PMT"))))
  expect_error(failBTPI(tibble::tibble(record_type = c("HIP", "PMT", "PMT"))))
  suppressMessages(expect_error(failBTPI(good_btpi |> mutate(dove_bag = 0))))
  suppressMessages(expect_message(failBTPI(good_btpi |> mutate(
    dove_bag = ifelse(dove_bag == 2, 0, dove_bag)
  ))))
  expect_error(failBTPI(good_btpi |> mutate(band_tailed_pigeon = 0)))
  expect_error(failBTPI(good_btpi |> mutate(band_tailed_pigeon = 1)))
  expect_error(failBTPI(good_btpi |> mutate(band_tailed_pigeon = 3)))
  expect_error(failBTPI(good_btpi |> mutate(ducks_bag = 2)))
  expect_error(failBTPI(good_btpi |> mutate(geese_bag = 2)))
  expect_error(failBTPI(good_btpi |> mutate(woodcock_bag = 2)))
  expect_error(failBTPI(good_btpi |> mutate(coots_snipe = 2)))
  expect_error(failBTPI(good_btpi |> mutate(rails_gallinules = 2)))
  expect_error(failBTPI(good_btpi |> mutate(cranes = 2)))
  expect_error(failBTPI(good_btpi |> mutate(brant = 2)))
  expect_error(failBTPI(good_btpi |> mutate(seaducks = 2)))
  expect_no_error(failBTPI(good_btpi))
})

test_that("failCR works", {
  good_cr <-
    tibble::tibble(
      record_type = c("PMT", "PMT", "PMT"),
      ducks_bag = c(0, 0, 0),
      geese_bag = c(0, 0, 0),
      dove_bag = c(0, 0, 0),
      woodcock_bag = c(0, 0, 0),
      coots_snipe = c(0, 0, 0),
      rails_gallinules = c(0, 0, 0),
      cranes = c(2, 2, 2),
      band_tailed_pigeon = c(0, 0, 0),
      brant = c(0, 0, 0),
      seaducks = c(0, 0, 0))

  expect_error(failCR(tibble::tibble(record_type = c("HIP", "HIP", "HIP"))))
  expect_error(failCR(tibble::tibble(record_type = c("HIP", "PMT", "PMT"))))
  expect_error(failCR(good_cr |> mutate(cranes = 0)))
  expect_error(failCR(good_cr |> mutate(cranes = 1)))
  expect_error(failCR(good_cr |> mutate(cranes = 3)))
  expect_error(failCR(good_cr |> mutate(ducks_bag = 2)))
  expect_error(failCR(good_cr |> mutate(geese_bag = 2)))
  expect_error(failCR(good_cr |> mutate(dove_bag = 2)))
  expect_error(failCR(good_cr |> mutate(woodcock_bag = 2)))
  expect_error(failCR(good_cr |> mutate(coots_snipe = 2)))
  expect_error(failCR(good_cr |> mutate(rails_gallinules = 2)))
  expect_error(failCR(good_cr |> mutate(band_tailed_pigeon = 2)))
  expect_error(failCR(good_cr |> mutate(brant = 2)))
  expect_error(failCR(good_cr |> mutate(seaducks = 2)))
  expect_no_error(failCR(good_cr))
})
