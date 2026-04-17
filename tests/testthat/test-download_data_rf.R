test_that("download_data_rf returns monthly data with correct columns", {
  skip_if_offline()
  skip_on_cran()
  data <- download_data_rf("2020-01-01", "2020-12-31")
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("date", "risk_free") %in% colnames(data)))
  expect_equal(ncol(data), 2)
  expect_true(all(!is.na(data$risk_free)))
  expect_true(all(data$risk_free >= 0))
})

test_that("download_data_rf returns daily data with correct columns", {
  skip_if_offline()
  skip_on_cran()
  data <- download_data_rf(
    "2020-01-01", "2020-12-31", frequency = "daily"
  )
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("date", "risk_free") %in% colnames(data)))
  expect_equal(ncol(data), 2)
  expect_true(all(!is.na(data$risk_free)))
  expect_true(all(data$risk_free >= 0))
})

test_that("download_data_rf monthly uses TB3MS pre-2001 and DTB4WK post", {
  skip_if_offline()
  skip_on_cran()
  pre <- download_data_rf("1990-01-01", "2000-12-31")
  post <- download_data_rf("2001-01-01", "2020-12-31")
  expect_true(nrow(pre) > 0)
  expect_true(nrow(post) > 0)
})

test_that("download_data_rf uses correct splice date boundary", {
  skip_if_offline()
  skip_on_cran()
  data <- download_data_rf("2000-11-01", "2001-02-28")
  expect_true(nrow(data) >= 4)
  expect_true(all(!is.na(data$risk_free)))
})

test_that("download_data_rf errors on invalid frequency", {
  expect_error(
    download_data_rf("2020-01-01", "2020-12-31", frequency = "weekly"),
    regexp = "frequency"
  )
})

test_that("download_data_rf errors when start_date after end_date", {
  expect_error(
    download_data_rf("2021-12-31", "2020-01-01"),
    regexp = "`start_date` cannot be after `end_date`"
  )
})

test_that("download_data via tidyfinance risk_free routes correctly", {
  skip_if_offline()
  skip_on_cran()
  data <- download_data(
    "tidyfinance", "risk_free", "2020-01-01", "2020-12-31"
  )
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("date", "risk_free") %in% colnames(data)))
})

test_that("risk_free appears in list_supported_types for tidyfinance", {
  types <- list_supported_types()
  tf_types <- types[types$domain == "tidyfinance", ]
  expect_true("risk_free" %in% tf_types$type)
})
