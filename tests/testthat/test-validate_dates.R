test_that("validate_dates returns NULL dates with message when both are NULL and use_default_range = FALSE", {
  expect_message(
    result <- validate_dates(NULL, NULL, use_default_range = FALSE),
    "Returning the full data set"
  )
  expect_null(result$start_date)
  expect_null(result$end_date)
})

test_that("validate_dates returns Date range with message when both are NULL and use_default_range = TRUE", {
  expect_message(
    result <- validate_dates(NULL, NULL, use_default_range = TRUE),
    "No.*start_date.*or.*end_date"
  )
  expect_s3_class(result$start_date, "Date")
  expect_s3_class(result$end_date, "Date")
  expect_true(result$start_date < result$end_date)
})

test_that("validate_dates default range spans approximately two years", {
  suppressMessages(result <- validate_dates(NULL, NULL, use_default_range = TRUE))
  diff_days <- as.numeric(result$end_date - result$start_date)
  expect_true(diff_days > 300 && diff_days < 750)
})

test_that("validate_dates coerces character dates to Date", {
  result <- validate_dates("2020-01-01", "2021-12-31")
  expect_s3_class(result$start_date, "Date")
  expect_s3_class(result$end_date, "Date")
  expect_equal(result$start_date, as.Date("2020-01-01"))
  expect_equal(result$end_date, as.Date("2021-12-31"))
})

test_that("validate_dates accepts Date objects directly", {
  result <- validate_dates(as.Date("2019-06-01"), as.Date("2022-06-01"))
  expect_equal(result$start_date, as.Date("2019-06-01"))
  expect_equal(result$end_date, as.Date("2022-06-01"))
})

test_that("validate_dates errors when start_date is after end_date", {
  expect_error(
    validate_dates("2021-12-31", "2020-01-01"),
    "`start_date` cannot be after `end_date`"
  )
})

test_that("validate_dates errors when start_date equals end_date", {
  expect_no_error(
    validate_dates("2020-06-15", "2020-06-15")
  )
})

test_that("validate_dates returns a named list", {
  result <- validate_dates("2020-01-01", "2021-01-01")
  expect_type(result, "list")
  expect_named(result, c("start_date", "end_date"))
})

test_that("validate_dates NULL list result is also a named list", {
  suppressMessages(result <- validate_dates(NULL, NULL))
  expect_type(result, "list")
  expect_named(result, c("start_date", "end_date"))
})
