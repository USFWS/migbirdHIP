test_that("failyear works", {
  expect_error(failyear("A"))
  expect_error(failyear("2019"))
  expect_error(failyear(2019))
  expect_error(failyear(2024.5))
  expect_error(failyear(2030))
  expect_no_error(failyear(2024))
})
