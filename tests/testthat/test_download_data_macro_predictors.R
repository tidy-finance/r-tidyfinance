test_that("download_data_macro_predictors gracefully handles broken url", {
  expect_message(
    download_data_macro_predictors("macro_predictors_monthly", start_date = 2020, end_date = 2023, sheet_id = "test"),
    "Returning an empty data set due to download failure."
  )
})
