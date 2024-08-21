
test_that("lag_column works correctly with basic input", {
  dates <- as.Date('2023-01-01') + 0:9
  values <- 1:10
  result <- lag_column(values, dates, lag = 1, max_lag = 3)

  expected <- c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  expect_equal(result, expected)
})

test_that("lag_column handles negative lag and max_lag values correctly", {
  dates <- as.Date('2023-01-01') + 0:9
  values <- 1:10

  expect_error(
    lag_column(values, dates, lag = -1, max_lag = 3),
    regexp = "must be non-negative"
  )

  expect_error(
    lag_column(values, dates, lag = 2, max_lag = 1),
    regexp = "must be greater than or equal to"
  )
})

test_that("lag_column returns NA for unmatched dates", {
  dates <- as.Date('2023-01-01') + 0:9
  values <- 1:10
  result <- lag_column(values, dates, lag = 10, max_lag = 20)

  expected <- rep(as.integer(NA), 10)
  expect_equal(result, expected)
})

test_that("lag_column returns correct results when drop_na is FALSE", {
  dates <- as.Date('2023-01-01') + 0:9
  values <- c(NA, 1:9)

  result <- lag_column(values, dates, lag = 1, max_lag = 3, drop_na = FALSE)

  expected <- c(NA, NA, 1, 2, 3, 4, 5, 6, 7, 8)
  expect_equal(result, expected)
})
