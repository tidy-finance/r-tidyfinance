# Tests for download_data_factors_ff ----------------------------------------

test_that("download_data_factors_ff errors when dataset is NULL", {
  expect_error(
    download_data_factors_ff(dataset = NULL),
    "dataset.*required"
  )
})

test_that("download_data_factors_ff errors for unsupported dataset name", {
  expect_error(
    download_data_factors_ff(dataset = "Not a real FF dataset"),
    "Unsupported"
  )
})

test_that("download_data_factors_ff handles start_date after end_date", {
  expect_error(
    download_data_factors_ff(
      dataset = "Fama/French 3 Factors",
      start_date = "2021-12-31",
      end_date = "2020-01-01"
    ),
    regexp = "`start_date` cannot be after `end_date`"
  )
})

test_that("download_data_factors_ff emits deprecation warning for legacy type argument", {
  expect_warning(
    expect_error(
      download_data_factors_ff(
        type = "factors_ff3_monthly",
        start_date = "2025-01-01",
        end_date = "2020-01-01"
      )
    ),
    class = "lifecycle_warning_deprecated"
  )
})

# Tests for download_data_factors_q ------------------------------------------

test_that("download_data_factors_q gracefully handles broken url", {
  skip_if_offline()
  skip_on_cran()
  expect_message(
    download_data_factors_q(
      dataset = "q5_factors_daily_2024",
      start_date = "2020-01-01",
      end_date = "2022-12-01",
      url = "test"
    ),
    "Returning an empty data set due to download failure."
  )
})

test_that("download_data_factors_q handles start_date after end_date", {
  expect_error(
    download_data_factors_q(
      dataset = "q5_factors_daily_2024",
      start_date = "2021-12-31",
      end_date = "2020-01-01"
    ),
    regexp = "`start_date` cannot be after `end_date`"
  )
})
