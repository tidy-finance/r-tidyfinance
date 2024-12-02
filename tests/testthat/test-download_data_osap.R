test_that("download_data_osap gracefully handles broken url", {
  expect_message(
    download_data_osap(
      start_date = "2020-01-01", end_date = "2022-12-01", sheet_id = "test"
    ),
    "Returning an empty data set due to download failure."
  )
})
