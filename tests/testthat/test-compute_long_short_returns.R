make_portfolio_returns_data <- function(n_portfolios = 5, n_months = 6, seed = 42) {
  set.seed(seed)
  dates <- seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = n_months)
  data <- expand.grid(portfolio = seq_len(n_portfolios), date = dates)
  data$ret_excess_vw <- rnorm(nrow(data), 0, 0.05)
  data$ret_excess_ew <- rnorm(nrow(data), 0, 0.05)
  data[order(data$date, data$portfolio), ]
}

test_that("compute_long_short_returns returns a data.frame", {
  data <- make_portfolio_returns_data()
  result <- compute_long_short_returns(data)
  expect_s3_class(result, "data.frame")
})

test_that("compute_long_short_returns output has one row per date", {
  data <- make_portfolio_returns_data(n_months = 6)
  result <- compute_long_short_returns(data)
  expect_equal(nrow(result), 6)
})

test_that("compute_long_short_returns includes a date column in the output", {
  data <- make_portfolio_returns_data()
  result <- compute_long_short_returns(data)
  expect_true("date" %in% colnames(result))
})

test_that("compute_long_short_returns output columns match the ret_excess measures in input", {
  data <- make_portfolio_returns_data()
  result <- compute_long_short_returns(data)
  expect_true("ret_excess_vw" %in% colnames(result))
  expect_true("ret_excess_ew" %in% colnames(result))
})

test_that("compute_long_short_returns computes top_minus_bottom correctly", {
  # One date, three portfolios with known returns
  data <- data.frame(
    portfolio = 1:3,
    date = as.Date("2020-01-01"),
    ret_excess_vw = c(0.01, 0.03, 0.05)
  )
  result <- compute_long_short_returns(data, direction = "top_minus_bottom")
  expect_equal(result$ret_excess_vw, 0.05 - 0.01, tolerance = 1e-10)
})

test_that("compute_long_short_returns computes bottom_minus_top correctly", {
  data <- data.frame(
    portfolio = 1:3,
    date = as.Date("2020-01-01"),
    ret_excess_vw = c(0.01, 0.03, 0.05)
  )
  result <- compute_long_short_returns(data, direction = "bottom_minus_top")
  expect_equal(result$ret_excess_vw, 0.01 - 0.05, tolerance = 1e-10)
})

test_that("bottom_minus_top is the negative of top_minus_bottom", {
  data <- make_portfolio_returns_data(n_months = 3)
  result_tmb <- compute_long_short_returns(data, direction = "top_minus_bottom")
  result_bmt <- compute_long_short_returns(data, direction = "bottom_minus_top")
  expect_equal(result_bmt$ret_excess_vw, -result_tmb$ret_excess_vw, tolerance = 1e-10)
  expect_equal(result_bmt$ret_excess_ew, -result_tmb$ret_excess_ew, tolerance = 1e-10)
})

test_that("compute_long_short_returns output dates are sorted", {
  data <- make_portfolio_returns_data(n_months = 12)
  result <- compute_long_short_returns(data)
  expect_equal(result$date, sort(result$date))
})

test_that("compute_long_short_returns works with two portfolios", {
  data <- data.frame(
    portfolio = c(1, 2),
    date = as.Date("2020-01-01"),
    ret_excess_vw = c(0.02, 0.06)
  )
  result <- compute_long_short_returns(data, direction = "top_minus_bottom")
  expect_equal(result$ret_excess_vw, 0.06 - 0.02, tolerance = 1e-10)
})

test_that("compute_long_short_returns uses custom data_options", {
  data <- data.frame(
    port = 1:3,
    month = as.Date("2020-01-01"),
    ret_vw = c(0.01, 0.03, 0.05)
  )
  custom_opts <- data_options(portfolio = "port", date = "month", ret_excess = "ret_vw")
  result <- compute_long_short_returns(data, data_options = custom_opts)
  expect_true("month" %in% colnames(result))
  expect_true("ret_vw" %in% colnames(result))
  expect_equal(result$ret_vw, 0.05 - 0.01, tolerance = 1e-10)
})
