test_that("download_data_factors_q gracefully handles broken url", {
  skip_if_offline()
  skip_on_cran()
  expect_message(
    download_data_factors_q(
      "factors_q5_annual",
      start_date = "2020-01-01",
      end_date = "2022-12-01",
      url = "test"
    ),
    "Returning an empty data set due to download failure."
  ) |>
    expect_message("The resource may not be available")
})

test_that("download_data_factors_q handles start_date after end_date", {
  expect_error(
    download_data_factors_q(
      "factors_q5_annual",
      start_date = "2021-12-31",
      end_date = "2020-01-01"
    ),
    regexp = "`start_date` cannot be after `end_date`"
  )
})
