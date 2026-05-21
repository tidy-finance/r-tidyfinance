test_that("both NULL with use_default_range = FALSE returns NULL list", {
  result <- suppressMessages(validate_dates(NULL, NULL))

  expect_equal(result, list(start_date = NULL, end_date = NULL))
})

test_that("both NULL with use_default_range = TRUE returns two-year default window", {
  result <- suppressMessages(validate_dates(
    NULL,
    NULL,
    use_default_range = TRUE
  ))

  expect_s3_class(result$start_date, "Date")
  expect_s3_class(result$end_date, "Date")
  expect_equal(result$start_date, Sys.Date() %m-% years(2))
  expect_equal(result$end_date, Sys.Date() %m-% years(1))
})

test_that("valid date pair is coerced to Date and returned", {
  result <- validate_dates("2022-01-01", "2022-12-31")

  expect_equal(result$start_date, as.Date("2022-01-01"))
  expect_equal(result$end_date, as.Date("2022-12-31"))
})

test_that("start_date after end_date raises an error", {
  expect_error(validate_dates("2022-12-31", "2022-01-01"))
})
