# read_hip function -------------------------------------------------------

test_that("read_hip works for single state", {

  suppressMessages(
    invisible(
      capture.output(
        read_data <-
          read_hip(
            paste0(here::here(), "/inst/extdata/DL0901/"),
            state = "RI"))))

  expect_true(nrow(read_data) > 0)
})

test_that("read_hip works for a download", {

  suppressMessages(
    invisible(
      capture.output(
        read_data <-
          read_hip(
            paste0(here::here(), "/inst/extdata/DL0901/")))))

  expect_true(length(unique(read_data$dl_state)) == 49)
})

# test record -------------------------------------------------------------

# see test-proof.R
