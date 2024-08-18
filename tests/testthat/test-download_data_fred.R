
test_that("function returns full data set when no date range is provided", {
  data <- download_data_fred("CPIAUCNS")
  expect_s3_class(data, "tbl_df")
  expect_true(all(c("date", "value", "series") %in% colnames(data)))
  expect_equal(unique(data$series), "CPIAUCNS")
})

test_that("function handles invalid series ID", {
  expect_warning(download_data_fred("INVALID_SERIES"),
                 regexp = "Failed to retrieve data for series INVALID_SERIES with status code 404.")
})

test_that("function handles start_date after end_date", {
  expect_error(download_data_fred("CPIAUCNS", start_date = "2021-12-31", end_date = "2020-01-01"),
               regexp = "`start_date` cannot be after `end_date`")
})
