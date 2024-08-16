test_that("Input validation: negative or zero lookback throws error", {
  data <- tibble::tibble(
    date = as.Date("2020-01-01"),
    permno = 1,
    ret_excess = 0,
    mkt_excess = 0
  )
  expect_error(estimate_betas(data, "ret_excess ~ mkt_excess", -1))
  expect_error(estimate_betas(data, "ret_excess ~ mkt_excess", 0))
})

test_that("Input validation: negative or zero min_obs throws error", {
  data <- tibble::tibble(
    date = as.Date("2020-01-01"),
    permno = 1,
    ret_excess = 0,
    mkt_excess = 0
  )
  expect_error(estimate_betas(data, "ret_excess ~ mkt_excess", 3, min_obs = -1))
  expect_error(estimate_betas(data, "ret_excess ~ mkt_excess", 3, min_obs = 0))
})

test_that("Input validation: invalid use_furrr throws error", {
  data <- tibble::tibble(
    date = as.Date("2020-01-01"),
    permno = 1,
    ret_excess = 0,
    mkt_excess = 0
  )
  expect_error(estimate_betas(data, "ret_excess ~ mkt_excess", 3, use_furr = 0))
})

test_that("Output structure: correct column names and number of rows", {
  data <- tibble::tibble(
    date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 2),
    permno = rep(1:2, times = 12),
    ret_excess = rnorm(24, 0, 0.1),
    mkt_excess = rnorm(24, 0, 0.1)
  )

  result <- estimate_betas(data, "ret_excess ~ mkt_excess", months(3))

  expect_true(all(c("date", "beta_mkt_excess") %in% colnames(result)))
  expect_equal(nrow(result), 24)  # Should match the number of unique date-permno pairs
})

test_that("Correctness: Known result test", {
  data <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    permno = c(1, 1, 1),
    ret_excess = c(0.1, 0.2, 0.3),
    mkt_excess = c(0.1, 0.2, 0.3)
  )

  result <- estimate_betas(data, "ret_excess ~ mkt_excess", months(3))

  expect_equal(result$beta_mkt_excess, c(NA, 1, 1))
})

test_that("Performance: Single vs multiple workers give the same result", {
  data <- tibble::tibble(
    date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 2),
    permno = rep(1:2, times = 12),
    ret_excess = rnorm(24, 0, 0.1),
    mkt_excess = rnorm(24, 0, 0.1)
  )

  future::plan(strategy = "multisession")

  result_single <- estimate_betas(data, "ret_excess ~ mkt_excess", months(3))
  result_multi <- estimate_betas(data, "ret_excess ~ mkt_excess", months(3), use_furrr = TRUE)

  expect_equal(result_single, result_multi)
})

test_that("Rolling window behavior: correct handling of boundary dates", {
  data <- tibble::tibble(
    date = seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-06-01"), by = "month"),
    permno = 1,
    ret_excess = rnorm(6, 0, 0.1),
    mkt_excess = rnorm(6, 0, 0.1)
  )

  result <- estimate_betas(data, "ret_excess ~ mkt_excess", months(3))

  expect_equal(nrow(result), 6)
  # Check if the first couple of rows have NA values where the window size is not sufficient
  expect_true(all(is.na(result$beta_mkt_excess[1])))
})

test_that("Daily data test: correctly handles daily data grouped into months", {
  data <- tibble::tibble(
    date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-31"), by = "day"), each = 2),
    permno = rep(1:2, times = 366),
    ret_excess = rnorm(732, 0, 0.02),
    mkt_excess = rnorm(732, 0, 0.02)
  )

  data <- data %>%
    mutate(date = lubridate::floor_date(date, "month"))

  result <- estimate_betas(data, "ret_excess ~ mkt_excess", lookback = months(6))

  expect_equal(nrow(result), length(unique(data$date)) * 2)  # Check if rows match the expected number of date-permno combinations
  expect_true(all(c("date", "beta_mkt_excess") %in% colnames(result)))
})

test_that("Edge case: single permno", {
  data <- tibble::tibble(
    date = seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"),
    permno = 1,
    ret_excess = rnorm(12, 0, 0.1),
    mkt_excess = rnorm(12, 0, 0.1)
  )

  result <- estimate_betas(data, "ret_excess ~ mkt_excess", months(3))

  expect_equal(nrow(result), 12)
  expect_true(all(c("date", "beta_mkt_excess") %in% colnames(result)))
})
