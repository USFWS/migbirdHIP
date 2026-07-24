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

# The following tests for glyphCheck() were generated with AI assistance using
# Claude Opus 4.8 in Perplexity on July 22, 2026.

test_that("glyphCheck messages 'All characters are UTF-8' on clean input", {
  clean_input <- DF_TEST_MINI |> dplyr::slice_head(n = 5)
  expect_message(glyphCheck(clean_input), "All characters are UTF-8")
})
