# test deduplication ------------------------------------------------------

test_that("exact duplicate removed", {

  # Create an exact duplicate
  duplicated_data <-
    bind_rows(
      DF_TEST_TINI |>
        dplyr::filter(record_key == "record_1") |>
        dplyr::mutate(record_key = paste0("record_", nrow(DF_TEST_TINI) + 1)),
      DF_TEST_TINI
    )

  deduped_data <- duplicateFix(duplicated_data)

  expect_equal(nrow(duplicated_data)-1, nrow(deduped_data))
})

