test_that("bagCheck works", {

  suppressMessages(bags_checked <- bagCheck(DF_TEST_MINI))

  expect_true(nrow(bags_checked) > 1)
})
