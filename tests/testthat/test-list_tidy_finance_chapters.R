test_that("returns the expected character vector of chapter names", {
  result <- list_tidy_finance_chapters()
  expect_type(result, "character")
})
