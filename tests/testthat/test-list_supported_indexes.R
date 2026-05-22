test_that("returns a tibble with the expected structure and content", {
  result <- list_supported_indexes()
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("index", "url", "skip"))
  expect_snapshot(result)
})
