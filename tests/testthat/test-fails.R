test_that("failProofed works", {
  expect_error(failProofed(DF_TEST_MINI))

  test_failproofed <- DF_TEST_MINI |> dplyr::mutate(errors = NA)
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
  expect_no_error(failYear(REF_CURRENT_SEASON))
})
