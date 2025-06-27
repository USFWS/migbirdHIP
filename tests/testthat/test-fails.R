test_that("failYear works", {
  expect_error(failYear("A"))
  expect_error(failYear("2019"))
  expect_error(failYear(2019))
  expect_error(failYear(2024.5))
  expect_error(failYear(2030))
  expect_no_error(failYear(2024))
})

test_that("failProofed works", {
  expect_error(failProofed(DF_TEST_MINI))

  test_failproofed <- DF_TEST_MINI |> dplyr::mutate(errors = NA)
  expect_no_error(failProofed(test_failproofed))
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
