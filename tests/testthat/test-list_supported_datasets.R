test_that("list_supported_datasets_ff returns expected structure", {
  result <- list_supported_datasets_ff()
  expect_named(result, c("type", "dataset_name", "domain"))
  expect_true(all(result$domain == "Fama-French"))
  expect_snapshot(result)
})

test_that("list_supported_datasets_ff_legacy returns expected structure", {
  result <- list_supported_datasets_ff_legacy()
  expect_named(result, c("type", "dataset_name", "domain"))
  expect_true(all(result$domain == "Fama-French"))
  expect_snapshot(result)
})

test_that("list_supported_datasets_q returns expected structure", {
  result <- list_supported_datasets_q()
  expect_named(result, c("type", "dataset_name", "domain"))
  expect_true(all(result$domain == "Global Q"))
  expect_snapshot(result)
})

test_that("list_supported_datasets_macro_predictors returns expected structure", {
  result <- list_supported_datasets_macro_predictors()
  expect_named(result, c("type", "dataset_name", "domain"))
  expect_true(all(result$domain == "Goyal-Welch"))
  expect_snapshot(result)
})

test_that("list_supported_datasets_wrds returns expected structure", {
  result <- list_supported_datasets_wrds()
  expect_named(result, c("type", "dataset_name", "domain"))
  expect_true(all(result$domain == "WRDS"))
  expect_snapshot(result)
})

test_that("list_supported_datasets_other returns expected structure", {
  result <- list_supported_datasets_other()
  expect_named(result, c("type", "dataset_name", "domain"))
  expect_snapshot(result)
})

test_that("default call returns a tibble with all domains combined", {
  result <- list_supported_datasets()
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("type", "dataset_name", "domain"))
  expect_true(all(c("Fama-French", "Global Q", "WRDS") %in% result$domain))
})

test_that("domain filter returns only matching rows", {
  result <- list_supported_datasets(domain = "WRDS")
  expect_true(all(result$domain == "WRDS"))
  expect_equal(nrow(result), nrow(list_supported_datasets_wrds()))
})

test_that("as_vector = TRUE returns a character vector of types", {
  result <- list_supported_datasets(as_vector = TRUE)
  expect_type(result, "character")
  expect_false(is.data.frame(result))
})

test_that("list_supported_types warns about deprecation and delegates", {
  expect_warning(
    result <- list_supported_types(),
    "list_supported_types"
  )
  expect_equal(result, list_supported_datasets())
})
