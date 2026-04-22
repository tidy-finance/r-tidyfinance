test_that("list_supported_indexes returns a data.frame", {
  result <- list_supported_indexes()
  expect_s3_class(result, "data.frame")
})

test_that("list_supported_indexes has columns index, url, and skip", {
  result <- list_supported_indexes()
  expect_true(all(c("index", "url", "skip") %in% colnames(result)))
})

test_that("list_supported_indexes returns multiple rows", {
  result <- list_supported_indexes()
  expect_gt(nrow(result), 5)
})

test_that("list_supported_indexes index column is character", {
  result <- list_supported_indexes()
  expect_type(result$index, "character")
})

test_that("list_supported_indexes url column is character", {
  result <- list_supported_indexes()
  expect_type(result$url, "character")
})

test_that("list_supported_indexes skip column is numeric", {
  result <- list_supported_indexes()
  expect_type(result$skip, "double")
})

test_that("list_supported_indexes has no NAs in index column", {
  result <- list_supported_indexes()
  expect_true(all(!is.na(result$index)))
})

test_that("list_supported_indexes has no NAs in url column", {
  result <- list_supported_indexes()
  expect_true(all(!is.na(result$url)))
})

test_that("list_supported_indexes has no NAs in skip column", {
  result <- list_supported_indexes()
  expect_true(all(!is.na(result$skip)))
})

test_that("list_supported_indexes includes well-known indexes", {
  result <- list_supported_indexes()
  expect_true("S&P 500" %in% result$index)
  expect_true("DAX" %in% result$index)
  expect_true("MSCI World" %in% result$index)
})

test_that("list_supported_indexes url values start with http", {
  result <- list_supported_indexes()
  expect_true(all(grepl("^https?://", result$url)))
})

test_that("list_supported_indexes index values are unique", {
  result <- list_supported_indexes()
  expect_equal(length(unique(result$index)), nrow(result))
})

test_that("list_supported_indexes skip values are non-negative", {
  result <- list_supported_indexes()
  expect_true(all(result$skip >= 0))
})
