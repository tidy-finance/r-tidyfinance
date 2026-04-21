test_that("download_data_risk_free returns monthly data with correct columns", {
  skip_if_offline()
  skip_on_cran()
  data <- download_data_risk_free("2020-01-01", "2020-12-31")
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("date", "risk_free") %in% colnames(data)))
  expect_equal(ncol(data), 2)
  expect_true(all(!is.na(data$risk_free)))
})

test_that("download_data_risk_free returns daily data with correct columns", {
  skip_if_offline()
  skip_on_cran()
  data <- download_data_risk_free(
    "2020-01-01",
    "2020-12-31",
    frequency = "daily"
  )
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("date", "risk_free") %in% colnames(data)))
  expect_equal(ncol(data), 2)
  expect_true(all(!is.na(data$risk_free)))
})

test_that("download_data_risk_free monthly returns one row per month", {
  skip_if_offline()
  skip_on_cran()
  data <- download_data_risk_free("2010-01-01", "2010-12-31")
  expect_equal(nrow(data), 12)
  expect_s3_class(data$date, "Date")
})

test_that("download_data_risk_free uses correct splice date boundary", {
  skip_if_offline()
  skip_on_cran()
  data <- download_data_risk_free("2000-11-01", "2001-02-28")
  expect_true(nrow(data) >= 4)
  expect_true(all(!is.na(data$risk_free)))
})

test_that("download_data_risk_free errors on invalid frequency", {
  expect_error(
    download_data_risk_free("2020-01-01", "2020-12-31", frequency = "weekly"),
    regexp = "frequency"
  )
})

test_that("download_data_risk_free errors when start_date after end_date", {
  expect_error(
    download_data_risk_free("2021-12-31", "2020-01-01"),
    regexp = "`start_date` cannot be after `end_date`"
  )
})

test_that("download_data_risk_free returns filtered rows for date range", {
  mock_data <- tibble::tibble(
    date = seq.Date(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"),
    risk_free = runif(12, 0, 0.01)
  )
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(mock_data, tmp)

  with_mocked_bindings(
    read_parquet = function(file, ...) arrow::read_parquet(tmp, ...),
    .package = "arrow",
    {
      result <- download_data_risk_free("2020-03-01", "2020-06-30")
      expect_equal(nrow(result), 4)
      expect_true(all(result$date >= as.Date("2020-03-01")))
      expect_true(all(result$date <= as.Date("2020-06-30")))
    }
  )
})

test_that("download_data_risk_free aborts with clear message on failure", {
  with_mocked_bindings(
    read_parquet = function(file, ...) stop("connection refused"),
    .package = "arrow",
    {
      expect_error(
        download_data_risk_free("2020-01-01", "2020-12-31"),
        regexp = "Failed to download risk-free rate data from HuggingFace"
      )
    }
  )
})

test_that("download_data via tidyfinance risk_free routes correctly", {
  skip_if_offline()
  skip_on_cran()
  data <- download_data(
    "tidyfinance",
    "risk_free",
    "2020-01-01",
    "2020-12-31"
  )
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("date", "risk_free") %in% colnames(data)))
})

test_that("risk_free appears in list_supported_types for tidyfinance", {
  types <- list_supported_types()
  tf_types <- types[types$domain == "tidyfinance", ]
  expect_true("risk_free" %in% tf_types$type)
})
