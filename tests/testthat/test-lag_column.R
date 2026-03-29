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
    regexp = "must be >= "
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

data <- tibble::tibble(
  permno = rep(1:2, each = 10),
  date = rep(seq.Date(as.Date('2023-01-01'), by = "month", length.out = 10), 2),
  bm = runif(20, 0.5, 1.5),
  size = runif(20, 100, 200)
)

test_that("add_lagged_columns adds lagged columns", {
  result <- add_lagged_columns(
    data,
    cols = c("bm", "size"),
    lag = months(3),
    by = "permno"
  )
  expect_true("bm_lag" %in% colnames(result))
  expect_true("size_lag" %in% colnames(result))
})

test_that("add_lagged_columns returns error for negative lag or max_lag", {
  expect_error(add_lagged_columns(data, cols = c("bm", "size"), lag = -1))
  expect_error(add_lagged_columns(
    data,
    cols = c("bm", "size"),
    lag = months(3),
    max_lag = months(1)
  ))
})

test_that("add_lagged_columns works without grouping", {
  result <- add_lagged_columns(
    data |> filter(permno == 1),
    cols = c("bm", "size"),
    lag = months(3)
  )
  expect_true("bm_lag" %in% colnames(result))
  expect_true("size_lag" %in% colnames(result))
})

test_that("add_lagged_columns returns error without duplicate dates", {
  expect_error(add_lagged_columns(data, cols = c("bm", "size"), lag = 1))
})

test_that("add_lagged_columns preserves original column values", {
  result <- add_lagged_columns(
    data,
    cols = c("bm", "size"),
    lag = months(3),
    by = "permno"
  )
  expect_equal(result$bm, data$bm)
  expect_equal(result$size, data$size)
})
