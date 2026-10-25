test_that("returns the expected character vector of chapter names", {
  result <- list_tidy_finance_chapters()
  expect_type(result, "character")
  expect_false(anyDuplicated(result) > 0)
  expect_equal(result[1], "setting-up-your-environment")
  expect_true(
    all(c("the-tidyfinance-package", "clean-enhanced-trace") %in% result)
  )
  expect_false(any(c("hex-sticker", "changelog") %in% result))
})
