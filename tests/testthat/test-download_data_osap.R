test_that("download_data_osap gracefully handles broken url", {
  skip_if_offline()
  expect_message(
    download_data_osap(
      start_date = "2020-01-01", end_date = "2022-12-01", sheet_id = "test"
    ),
    "Returning an empty data set due to download failure."
  )
})

test_that("download_data_osap handles start_date after end_date", {
  expect_message(
    download_data_osap(
      start_date = "2021-01-01", end_date = "2020-12-01", sheet_id = "test"
    ),
    "Returning an empty data set due to download failure."
  )
})
