# A simple breakpoint function that returns quantile-based breakpoints.
# This avoids depending on the real compute_breakpoints / breakpoint_options.
mock_breakpoint_function <- function(
  data,
  sorting_variable,
  bp_options = NULL,
  data_options = NULL
) {
  n <- if (!is.null(bp_options) && !is.null(bp_options$n_portfolios)) {
    bp_options$n_portfolios
  } else {
    5
  }
  probs <- seq(0, 1, length.out = n + 1)
  quantile(data[[sorting_variable]], probs = probs, na.rm = TRUE, names = FALSE)
}

# A breakpoint function that uses explicit percentiles (like breakpoint_options(percentiles = ...))
mock_breakpoint_percentiles <- function(
  data,
  sorting_variable,
  bp_options = NULL,
  data_options = NULL
) {
  percentiles <- if (!is.null(bp_options) && !is.null(bp_options$percentiles)) {
    bp_options$percentiles
  } else {
    c(0.2, 0.4, 0.6, 0.8)
  }
  probs <- c(0, percentiles, 1)
  quantile(data[[sorting_variable]], probs = probs, na.rm = TRUE, names = FALSE)
}

test_that("assign_portfolio returns a vector the same length as nrow(data)", {
  data <- data.frame(id = 1:100, value = runif(100))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 5),
    breakpoint_function = mock_breakpoint_function
  )
  expect_length(result, nrow(data))
})

test_that("assign_portfolio returns integer portfolio indices", {
  data <- data.frame(id = 1:100, value = runif(100))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 5),
    breakpoint_function = mock_breakpoint_function
  )
  expect_type(result, "integer")
})

test_that("portfolio indices are within expected range for 5 portfolios", {
  set.seed(42)
  data <- data.frame(id = 1:200, value = runif(200))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 5),
    breakpoint_function = mock_breakpoint_function
  )
  expect_true(all(result >= 1 & result <= 5))
})

test_that("portfolio indices are within expected range for 10 portfolios", {
  set.seed(42)
  data <- data.frame(id = 1:1000, value = runif(1000))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 10),
    breakpoint_function = mock_breakpoint_function
  )
  expect_true(all(result >= 1 & result <= 10))
})

test_that("all portfolio buckets are populated with sufficient distinct data", {
  set.seed(123)
  data <- data.frame(id = 1:1000, value = seq_len(1000))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 5),
    breakpoint_function = mock_breakpoint_function
  )
  expect_equal(sort(unique(result)), 1:5)
})

test_that("higher sorting variable values get higher or equal portfolio indices", {
  data <- data.frame(id = 1:500, value = seq_len(500))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 5),
    breakpoint_function = mock_breakpoint_function
  )
  # For strictly increasing values, portfolio assignments must be non-decreasing
  expect_true(all(diff(result) >= 0))
})

test_that("rows with the same sorting variable value get the same portfolio", {
  data <- data.frame(id = 1:10, value = c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 3),
    breakpoint_function = mock_breakpoint_function
  )
  # Identical values must map to identical portfolios
  expect_equal(result[1], result[2])
  expect_equal(result[3], result[4])
  expect_equal(result[5], result[6])
  expect_equal(result[7], result[8])
  expect_equal(result[9], result[10])
})

test_that("constant sorting variable returns all 1s with a warning", {
  data <- data.frame(id = 1:50, value = rep(42, 50))
  expect_warning(
    result <- assign_portfolio(
      data,
      "value",
      breakpoint_options = list(n_portfolios = 5),
      breakpoint_function = mock_breakpoint_function
    ),
    "sorting variable is constant"
  )
  expect_equal(result, rep(1, 50))
})

test_that("constant sorting variable returns vector of correct length", {
  data <- data.frame(id = 1:10, value = rep(0, 10))
  expect_warning(
    result <- assign_portfolio(
      data,
      "value",
      breakpoint_function = mock_breakpoint_function
    ),
    "constant"
  )
  expect_length(result, 10)
})

test_that("two distinct values with 2 portfolios produces two groups", {
  data <- data.frame(id = 1:100, value = rep(c(1, 2), each = 50))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 2),
    breakpoint_function = mock_breakpoint_function
  )
  expect_true(all(result %in% c(1, 2)))
  # Lower values should be in portfolio 1, higher in portfolio 2 (or equal due to all.inside)
  expect_true(all(result[data$value == 1] <= result[data$value == 2]))
})

test_that("single-row data frame with constant variable triggers warning", {
  data <- data.frame(id = 1, value = 5)
  expect_warning(
    result <- assign_portfolio(
      data,
      "value",
      breakpoint_options = list(n_portfolios = 3),
      breakpoint_function = mock_breakpoint_function
    ),
    "constant"
  )
  expect_equal(result, 1)
})


test_that("warning is issued when clusters reduce the number of effective portfolios", {
  # Data heavily clustered: only 2 distinct values but requesting 5 portfolios
  data <- data.frame(id = 1:100, value = rep(c(1, 100), each = 50))

  # Mock that returns 6 breakpoints (for 5 portfolios) but clustering collapses them
  mock_bp_5 <- function(data, sorting_variable, bp_options, data_options) {
    c(1, 20, 40, 60, 80, 100)
  }

  expect_warning(
    result <- assign_portfolio(data, "value", breakpoint_function = mock_bp_5),
    "number of portfolios differs"
  )
})

test_that("no cluster warning when portfolios match expected count", {
  set.seed(99)
  data <- data.frame(id = 1:1000, value = seq_len(1000))
  expect_no_warning(
    result <- assign_portfolio(
      data,
      "value",
      breakpoint_options = list(n_portfolios = 5),
      breakpoint_function = mock_breakpoint_function
    )
  )
})

test_that("custom breakpoint function is used correctly", {
  # A breakpoint function that always splits at the median (2 portfolios)
  median_bp <- function(data, sorting_variable, bp_options, data_options) {
    med <- median(data[[sorting_variable]], na.rm = TRUE)
    c(min(data[[sorting_variable]]), med, max(data[[sorting_variable]]))
  }

  data <- data.frame(id = 1:100, value = seq_len(100))
  result <- assign_portfolio(data, "value", breakpoint_function = median_bp)

  expect_true(all(result %in% c(1, 2)))
  # Values below median should be portfolio 1
  expect_true(all(result[data$value <= 50] == 1))
})

test_that("breakpoint_options are forwarded to the breakpoint function", {
  # Breakpoint function that reads a custom parameter
  custom_bp <- function(data, sorting_variable, bp_options, data_options) {
    n <- bp_options$custom_n
    probs <- seq(0, 1, length.out = n + 1)
    quantile(
      data[[sorting_variable]],
      probs = probs,
      na.rm = TRUE,
      names = FALSE
    )
  }

  data <- data.frame(id = 1:200, value = seq_len(200))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(custom_n = 4),
    breakpoint_function = custom_bp
  )

  expect_true(all(result >= 1 & result <= 4))
})

test_that("data_options are forwarded to the breakpoint function", {
  # Breakpoint function that checks data_options are received
  bp_with_opts <- function(data, sorting_variable, bp_options, data_options) {
    # Verify data_options is passed through
    stopifnot(!is.null(data_options))
    stopifnot(data_options$date == "my_date")
    probs <- seq(0, 1, length.out = 4)
    quantile(
      data[[sorting_variable]],
      probs = probs,
      na.rm = TRUE,
      names = FALSE
    )
  }

  data <- data.frame(id = 1:100, my_date = Sys.Date(), value = seq_len(100))
  # Should not error — data_options are correctly forwarded

  expect_no_error(
    assign_portfolio(
      data,
      "value",
      breakpoint_function = bp_with_opts,
      data_options = list(date = "my_date")
    )
  )
})

test_that("NULL breakpoint_options works without error", {
  bp_fn <- function(data, sorting_variable, bp_options, data_options) {
    probs <- seq(0, 1, length.out = 4)
    quantile(
      data[[sorting_variable]],
      probs = probs,
      na.rm = TRUE,
      names = FALSE
    )
  }
  data <- data.frame(id = 1:100, value = seq_len(100))
  expect_no_error(
    assign_portfolio(
      data,
      "value",
      breakpoint_options = NULL,
      breakpoint_function = bp_fn
    )
  )
})

test_that("NULL data_options works without error", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  expect_no_error(
    assign_portfolio(
      data,
      "value",
      breakpoint_options = list(n_portfolios = 3),
      breakpoint_function = mock_breakpoint_function,
      data_options = NULL
    )
  )
})

test_that("negative values are handled correctly", {
  data <- data.frame(id = 1:100, value = seq(-50, 49))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 5),
    breakpoint_function = mock_breakpoint_function
  )
  expect_length(result, 100)
  expect_true(all(result >= 1 & result <= 5))
  # Monotonicity: sorted input should give non-decreasing portfolios
  expect_true(all(diff(result) >= 0))
})

test_that("decimal / floating point values are handled correctly", {
  set.seed(7)
  data <- data.frame(id = 1:200, value = rnorm(200))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 5),
    breakpoint_function = mock_breakpoint_function
  )
  expect_length(result, 200)
  expect_true(all(result >= 1 & result <= 5))
})

test_that("function works with large datasets", {
  set.seed(1)
  n <- 100000
  data <- data.frame(id = seq_len(n), value = rnorm(n))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 10),
    breakpoint_function = mock_breakpoint_function
  )
  expect_length(result, n)
  expect_true(all(result >= 1 & result <= 10))
  expect_equal(sort(unique(result)), 1:10)
})

test_that("extreme values (min and max) are assigned to boundary portfolios", {
  # With all.inside = TRUE, values at or beyond breakpoints are clamped
  data <- data.frame(id = 1:5, value = c(0, 25, 50, 75, 100))
  bp_fn <- function(data, sorting_variable, bp_options, data_options) {
    c(0, 25, 50, 75, 100)
  }
  result <- assign_portfolio(data, "value", breakpoint_function = bp_fn)
  # all.inside = TRUE means min maps to 1 and max maps to length(breakpoints)-1
  expect_equal(result[1], 1L) # value = 0 (at left boundary)
  expect_equal(result[5], 4L) # value = 100 (at right boundary, clamped inside)
})

test_that("NA values in sorting variable produce NA portfolio assignments", {
  data <- data.frame(id = 1:10, value = c(1:8, NA, NA))

  bp_fn <- function(data, sorting_variable, bp_options, data_options) {
    vals <- data[[sorting_variable]]
    probs <- seq(0, 1, length.out = 4)
    quantile(vals, probs = probs, na.rm = TRUE, names = FALSE)
  }

  # findInterval returns NA for NA inputs
  result <- assign_portfolio(data, "value", breakpoint_function = bp_fn)
  expect_true(is.na(result[9]))
  expect_true(is.na(result[10]))
  # Non-NA values should still be assigned
  expect_true(all(!is.na(result[1:8])))
})

test_that("return type is integer vector (from findInterval)", {
  data <- data.frame(id = 1:50, value = seq_len(50))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 5),
    breakpoint_function = mock_breakpoint_function
  )
  expect_true(is.integer(result))
})

test_that("constant variable early return is numeric (rep(1, n))", {
  data <- data.frame(id = 1:10, value = rep(5, 10))
  expect_warning(
    result <- assign_portfolio(
      data,
      "value",
      breakpoint_function = mock_breakpoint_function
    ),
    "constant"
  )
  # rep(1, n) returns numeric, not integer — capture this behavior
  expect_equal(result, rep(1, 10))
})

test_that("function works with different column names for sorting variable", {
  data <- data.frame(company_id = 1:100, market_cap = runif(100, 1e6, 1e9))
  result <- assign_portfolio(
    data,
    "market_cap",
    breakpoint_options = list(n_portfolios = 5),
    breakpoint_function = mock_breakpoint_function
  )
  expect_length(result, 100)
  expect_true(all(result >= 1 & result <= 5))
})

test_that("percentile-based breakpoints produce correct number of groups", {
  set.seed(42)
  data <- data.frame(id = 1:500, value = seq_len(500))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(percentiles = c(0.2, 0.4, 0.6, 0.8)),
    breakpoint_function = mock_breakpoint_percentiles
  )
  expect_true(all(result >= 1 & result <= 5))
  expect_equal(sort(unique(result)), 1:5)
})

test_that("two portfolios split data roughly in half", {
  set.seed(10)
  data <- data.frame(id = 1:1000, value = rnorm(1000))
  result <- assign_portfolio(
    data,
    "value",
    breakpoint_options = list(n_portfolios = 2),
    breakpoint_function = mock_breakpoint_function
  )
  expect_true(all(result %in% c(1, 2)))
  # Each group should have roughly 500 members (within tolerance)
  counts <- table(result)
  expect_true(all(counts > 400))
})

test_that("breakpoint function receives all four arguments", {
  received_args <- list()

  spy_bp <- function(data, sorting_variable, bp_options, data_options) {
    received_args$data <<- data
    received_args$sorting_variable <<- sorting_variable
    received_args$bp_options <<- bp_options
    received_args$data_options <<- data_options
    c(0, 0.5, 1)
  }

  data <- data.frame(id = 1:10, value = seq(0, 1, length.out = 10))
  my_bp_opts <- list(n_portfolios = 2)
  my_data_opts <- list(date = "date_col")

  assign_portfolio(
    data,
    "value",
    breakpoint_options = my_bp_opts,
    breakpoint_function = spy_bp,
    data_options = my_data_opts
  )

  expect_identical(received_args$data, data)
  expect_equal(received_args$sorting_variable, "value")
  expect_equal(received_args$bp_options, my_bp_opts)
  expect_equal(received_args$data_options, my_data_opts)
})
