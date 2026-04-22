test_that("list_supported_types returns a data.frame", {
  result <- list_supported_types()
  expect_s3_class(result, "data.frame")
})

test_that("list_supported_types has columns type, dataset_name, and domain", {
  result <- list_supported_types()
  expect_true(all(c("type", "dataset_name", "domain") %in% colnames(result)))
})

test_that("list_supported_types returns multiple rows", {
  result <- list_supported_types()
  expect_gt(nrow(result), 10)
})

test_that("list_supported_types type column contains no NAs", {
  result <- list_supported_types()
  expect_true(all(!is.na(result$type)))
})

test_that("list_supported_types domain column contains no NAs", {
  result <- list_supported_types()
  expect_true(all(!is.na(result$domain)))
})

test_that("list_supported_types filters by domain correctly", {
  result_wrds <- list_supported_types(domain = "WRDS")
  expect_true(all(result_wrds$domain == "WRDS"))
  expect_gt(nrow(result_wrds), 0)
})

test_that("list_supported_types filters by multiple domains", {
  result <- list_supported_types(domain = c("WRDS", "Fama-French"))
  expect_true(all(result$domain %in% c("WRDS", "Fama-French")))
  expect_gt(nrow(result), 0)
})

test_that("list_supported_types returns empty data frame for unknown domain", {
  result <- list_supported_types(domain = "nonexistent_domain_xyz")
  expect_equal(nrow(result), 0)
})

test_that("list_supported_types returns character vector when as_vector = TRUE", {
  result <- list_supported_types(as_vector = TRUE)
  expect_type(result, "character")
  expect_gt(length(result), 0)
})

test_that("list_supported_types as_vector = TRUE returns the type column", {
  result_df <- list_supported_types()
  result_vec <- list_supported_types(as_vector = TRUE)
  expect_equal(result_vec, result_df$type)
})

test_that("list_supported_types includes Fama-French entries", {
  result <- list_supported_types(domain = "Fama-French")
  expect_gt(nrow(result), 0)
  expect_true(any(grepl("factors_ff", result$type)))
})

test_that("list_supported_types includes Global Q entries", {
  result <- list_supported_types(domain = "Global Q")
  expect_gt(nrow(result), 0)
  expect_true(any(grepl("factors_q5", result$type)))
})

test_that("list_supported_types includes Goyal-Welch macro predictor entries", {
  result <- list_supported_types(domain = "Goyal-Welch")
  expect_gt(nrow(result), 0)
  expect_true(any(grepl("macro_predictors", result$type)))
})

test_that("list_supported_types type values are unique", {
  result <- list_supported_types()
  expect_equal(length(unique(result$type)), nrow(result))
})

test_that("list_supported_types domain filter combined with as_vector works", {
  result <- list_supported_types(domain = "WRDS", as_vector = TRUE)
  expect_type(result, "character")
  expect_gt(length(result), 0)
  full_df <- list_supported_types(domain = "WRDS")
  expect_equal(result, full_df$type)
})
