test_that("REF_DATES contains current hunting season", {
  expect_equal(as.character(unique(REF_DATES$hunting_season)),
               REF_CURRENT_SEASON)
})

test_that("REF_DATES contains issue_start that precede issue_end", {
  expect_equal(nrow(filter(REF_DATES, issue_start > issue_end)), 0)
})

test_that("REF_DATES contains 48 states", {
  expect_equal(length(REF_DATES$state), 48)
})

test_that("SF_HEXMAP contains 49 states", {
  expect_equal(length(SF_HEXMAP$state), 49)
})

test_that("REF_BAGS contains 49 states", {
  expect_equal(length(unique(REF_BAGS$state)), 49)
})

