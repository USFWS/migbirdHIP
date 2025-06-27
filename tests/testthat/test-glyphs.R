test_that("glyphCheck finds non-utf8 characters", {

  glyph_data <-
    DF_TEST_TINI_READ |>
    mutate(
      firstname = ifelse(record_key == "record_1", "caf\xC3", firstname))

  suppressMessages(
    invisible(
      capture.output(
        glyph_checked <- glyphCheck(glyph_data)
      )))

  expect_true(nrow(glyph_checked) == 1)
})
