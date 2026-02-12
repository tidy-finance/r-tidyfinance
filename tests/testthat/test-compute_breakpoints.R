test_that("error if breakpoint_options is not a list", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  expect_error(
    compute_breakpoints(data, "value", "not_a_list"),
    "named list"
  )
})

test_that("error if both n_portfolios and percentiles are provided", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  expect_error(
    compute_breakpoints(
      data,
      "value",
      breakpoint_options(n_portfolios = 5, percentiles = c(0.5))
    ),
    "not both"
  )
})

test_that("error if neither n_portfolios nor percentiles are provided", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  expect_error(
    compute_breakpoints(data, "value", breakpoint_options()),
    "must provide"
  )
})

test_that("error if n_portfolios is 1 or less", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  expect_error(
    compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 1)),
  )
  expect_error(
    compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 0)),
  )
})

test_that("error if breakpoint_exchanges column is missing from data", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  expect_error(
    compute_breakpoints(
      data,
      "value",
      breakpoint_options(n_portfolios = 5, breakpoint_exchanges = c("NYSE"))
    ),
    "exchange"
  )
})

test_that("returns n_portfolios + 1 breakpoints", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  for (n in c(2, 5, 10)) {
    bp <- compute_breakpoints(
      data,
      "value",
      breakpoint_options(n_portfolios = n)
    )
    expect_length(bp, n + 1)
  }
})

test_that("breakpoints are in ascending order", {
  data <- data.frame(id = 1:1000, value = rnorm(1000))
  bp <- compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 5))
  expect_true(all(diff(bp) >= 0))
})

test_that("breakpoints are numeric", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  bp <- compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 5))
  expect_type(bp, "double")
})

test_that("first breakpoint equals min, last approximately equals max", {
  data <- data.frame(id = 1:1000, value = seq_len(1000))
  bp <- compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 5))
  expect_equal(bp[1], min(data$value))
  # Last breakpoint = max + tiny epsilon (1e-20)
  expect_equal(bp[6], max(data$value) + 1e-20, tolerance = 1e-15)
})

test_that("n_portfolios = 2 gives 3 breakpoints (min, median-ish, max)", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  bp <- compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 2))
  expect_length(bp, 3)
})

test_that("percentiles produce correct number of breakpoints", {
  data <- data.frame(id = 1:1000, value = seq_len(1000))
  pcts <- c(0.2, 0.4, 0.6, 0.8)
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(percentiles = pcts)
  )
  # breakpoints = c(0, percentiles, 1) → length = length(percentiles) + 2
  expect_length(bp, length(pcts) + 2)
})

test_that("single percentile produces 3 breakpoints (median split)", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(percentiles = c(0.5))
  )
  expect_length(bp, 3)
})

test_that("percentile breakpoints are ascending", {
  set.seed(42)
  data <- data.frame(id = 1:500, value = rnorm(500))
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(percentiles = c(0.1, 0.3, 0.5, 0.7, 0.9))
  )
  expect_true(all(diff(bp) >= 0))
})

test_that("n_portfolios = 5 and percentiles = c(0.2,0.4,0.6,0.8) give same breakpoints", {
  data <- data.frame(id = 1:1000, value = seq_len(1000))
  bp_n <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5)
  )
  bp_p <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(percentiles = c(0.2, 0.4, 0.6, 0.8))
  )
  expect_equal(bp_n, bp_p)
})

test_that("n_portfolios = 10 and percentiles at deciles give same breakpoints", {
  set.seed(7)
  data <- data.frame(id = 1:5000, value = rnorm(5000))
  bp_n <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 10)
  )
  bp_p <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(percentiles = seq(0.1, 0.9, by = 0.1))
  )
  expect_equal(bp_n, bp_p)
})

test_that("breakpoint_exchanges filters data before computing breakpoints", {
  set.seed(1)
  data <- data.frame(
    id = 1:200,
    exchange = rep(c("NYSE", "NASDAQ"), each = 100),
    value = c(rnorm(100, mean = 10), rnorm(100, mean = 50))
  )

  bp_all <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5)
  )
  bp_nyse <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, breakpoint_exchanges = c("NYSE"))
  )

  # NYSE only has values around 10, so breakpoints should differ
  expect_false(isTRUE(all.equal(bp_all, bp_nyse)))
  # NYSE breakpoints should be clustered around 10
  expect_true(all(bp_nyse < 20))
})

test_that("multiple exchanges can be specified", {
  data <- data.frame(
    id = 1:300,
    exchange = rep(c("NYSE", "NASDAQ", "AMEX"), each = 100),
    value = c(1:100, 201:300, 401:500)
  )
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(
      n_portfolios = 5,
      breakpoint_exchanges = c("NYSE", "AMEX")
    )
  )
  # Breakpoints should span NYSE (1-100) and AMEX (401-500), not NASDAQ
  expect_equal(bp[1], 1)
  expect_true(bp[length(bp)] > 400)
})

test_that("custom exchange column name via data_options", {
  data <- data.frame(
    id = 1:100,
    exch = rep(c("NYSE", "NASDAQ"), each = 50),
    value = seq_len(100)
  )
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, breakpoint_exchanges = c("NYSE")),
    data_options = data_options(exchange = "exch")
  )
  # NYSE is rows 1-50, so breakpoints should be within [1, 50]
  expect_true(bp[1] >= 1)
  expect_true(bp[length(bp)] <= 50 + 1e-15)
})

test_that("error when custom exchange column doesn't exist", {
  data <- data.frame(id = 1:100, value = seq_len(100))
  expect_error(
    compute_breakpoints(
      data,
      "value",
      breakpoint_options(n_portfolios = 5, breakpoint_exchanges = c("NYSE")),
      data_options = data_options(exchange = "nonexistent")
    ),
    "nonexistent"
  )
})

test_that("NULL data_options uses defaults without error", {
  data <- data.frame(id = 1:100, exchange = "NYSE", value = seq_len(100))
  expect_no_error(
    compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 5))
  )
})


test_that("interior breakpoints are slightly larger than raw quantiles", {
  data <- data.frame(id = 1:1000, value = seq_len(1000))
  bp <- compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 5))
  raw_q <- quantile(
    data$value,
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE,
    names = FALSE
  )

  # First breakpoint is untouched
  expect_equal(bp[1], raw_q[1])
  # Interior breakpoints (indices 2 through n+1) have epsilon added

  for (i in 2:6) {
    expect_equal(bp[i], raw_q[i] + 1e-20, tolerance = 1e-25)
  }
})

test_that("smooth_bunching handles clustering on both edges", {
  # Many zeros and many max-values, some spread in between
  data <- data.frame(
    id = 1:500,
    value = c(rep(0, 200), seq(1, 99, length.out = 100), rep(100, 200))
  )
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, smooth_bunching = TRUE)
  )
  expect_length(bp, 6)
  # First breakpoint should be 0, last should be >= 100
  expect_equal(bp[1], 0)
  expect_true(bp[6] >= 100)
  # Interior breakpoints should be strictly between 0 and 100
  expect_true(all(bp[2:5] > 0))
  expect_true(all(bp[2:5] <= 100 + 1e-10))
})

test_that("smooth_bunching with both edges and percentiles emits warning", {
  data <- data.frame(
    id = 1:500,
    value = c(rep(0, 200), seq(1, 99, length.out = 100), rep(100, 200))
  )
  expect_warning(
    compute_breakpoints(
      data,
      "value",
      breakpoint_options(
        percentiles = c(0.2, 0.4, 0.6, 0.8),
        smooth_bunching = TRUE
      )
    ),
    "smooth_bunching"
  )
})

test_that("smooth_bunching handles clustering only on lower edge", {
  data <- data.frame(
    id = 1:500,
    value = c(rep(0, 200), seq(1, 100, length.out = 300))
  )
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, smooth_bunching = TRUE)
  )
  expect_length(bp, 6)
  expect_equal(bp[1], 0)
  # After smoothing, breakpoints 2+ should be > 0
  expect_true(all(bp[2:6] > 0))
})

test_that("lower edge bunching with percentiles emits warning", {
  data <- data.frame(
    id = 1:500,
    value = c(rep(0, 200), seq(1, 100, length.out = 300))
  )
  expect_warning(
    compute_breakpoints(
      data,
      "value",
      breakpoint_options(
        percentiles = c(0.2, 0.4, 0.6, 0.8),
        smooth_bunching = TRUE
      )
    ),
    "smooth_bunching"
  )
})

test_that("smooth_bunching handles clustering only on upper edge", {
  data <- data.frame(
    id = 1:500,
    value = c(seq(0, 99, length.out = 300), rep(100, 200))
  )
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, smooth_bunching = TRUE)
  )
  expect_length(bp, 6)
  expect_true(bp[6] >= 100)
  # Interior breakpoints should be < 100
  expect_true(all(bp[1:5] < 100))
})

test_that("upper edge bunching with percentiles emits warning", {
  data <- data.frame(
    id = 1:500,
    value = c(seq(0, 99, length.out = 300), rep(100, 200))
  )
  expect_warning(
    compute_breakpoints(
      data,
      "value",
      breakpoint_options(
        percentiles = c(0.2, 0.4, 0.6, 0.8),
        smooth_bunching = TRUE
      )
    ),
    "smooth_bunching"
  )
})

test_that("smooth_bunching = FALSE does not alter clustered breakpoints", {
  data <- data.frame(
    id = 1:500,
    value = c(rep(0, 200), seq(1, 99, length.out = 100), rep(100, 200))
  )
  bp_no_smooth <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, smooth_bunching = FALSE)
  )
  raw_q <- quantile(
    data$value,
    probs = seq(0, 1, length.out = 6),
    na.rm = TRUE,
    names = FALSE
  )
  # Without smoothing, breakpoints should just be raw quantiles + epsilon
  expect_equal(bp_no_smooth[1], raw_q[1])
  for (i in 2:6) {
    expect_equal(bp_no_smooth[i], raw_q[i] + 1e-20, tolerance = 1e-25)
  }
})

test_that("smooth_bunching = NULL triggers error", {
  data <- data.frame(
    id = 1:500,
    value = c(rep(0, 200), seq(1, 99, length.out = 100), rep(100, 200))
  )
  expect_error(compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, smooth_bunching = NULL)
  ))
})


test_that("smooth_bunching = TRUE with no clustering gives same result as FALSE", {
  data <- data.frame(id = 1:1000, value = seq_len(1000))
  bp_smooth <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, smooth_bunching = TRUE)
  )
  bp_plain <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, smooth_bunching = FALSE)
  )
  expect_equal(bp_smooth, bp_plain)
})

test_that("NA values in sorting variable are ignored (na.rm = TRUE)", {
  data_clean <- data.frame(id = 1:100, value = seq_len(100))
  data_na <- data.frame(id = 1:120, value = c(seq_len(100), rep(NA, 20)))

  bp_clean <- compute_breakpoints(
    data_clean,
    "value",
    breakpoint_options(n_portfolios = 5)
  )
  bp_na <- compute_breakpoints(
    data_na,
    "value",
    breakpoint_options(n_portfolios = 5)
  )
  expect_equal(bp_clean, bp_na)
})


test_that("works with arbitrary column names", {
  data <- data.frame(company_id = 1:200, market_cap = runif(200, 1e6, 1e9))
  bp <- compute_breakpoints(
    data,
    "market_cap",
    breakpoint_options(n_portfolios = 5)
  )
  expect_length(bp, 6)
  expect_true(all(diff(bp) >= 0))
})

test_that("works with very small data (n = 2)", {
  data <- data.frame(id = 1:2, value = c(1, 10))
  bp <- compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 2))
  expect_length(bp, 3)
  expect_equal(bp[1], 1)
})

test_that("works with n = 3 and 3 portfolios (one obs per portfolio)", {
  data <- data.frame(id = 1:3, value = c(1, 5, 10))
  bp <- compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 3))
  expect_length(bp, 4)
})

test_that("non-uniform percentiles produce correct breakpoints", {
  data <- data.frame(id = 1:1000, value = seq_len(1000))
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(percentiles = c(0.1, 0.5, 0.9))
  )
  expect_length(bp, 5)
  # The 50th percentile breakpoint should be near 500
  expect_true(abs(bp[3] - 500) < 5)
})

test_that("100 portfolios work with sufficient data", {
  set.seed(99)
  data <- data.frame(id = 1:100000, value = rnorm(100000))
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 100)
  )
  expect_length(bp, 101)
  expect_true(all(diff(bp) >= 0))
})

test_that("breakpoints produce valid portfolio assignments via findInterval", {
  set.seed(42)
  data <- data.frame(id = 1:1000, value = rnorm(1000))
  bp <- compute_breakpoints(data, "value", breakpoint_options(n_portfolios = 5))
  portfolios <- findInterval(data$value, bp, all.inside = TRUE)

  expect_true(all(portfolios >= 1 & portfolios <= 5))
  expect_equal(sort(unique(portfolios)), 1:5)
})

test_that("exchange-filtered breakpoints still produce valid assignments on full data", {
  set.seed(1)
  data <- data.frame(
    id = 1:500,
    exchange = rep(c("NYSE", "NASDAQ"), each = 250),
    value = rnorm(500)
  )
  bp <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, breakpoint_exchanges = c("NYSE"))
  )
  portfolios <- findInterval(data$value, bp, all.inside = TRUE)

  expect_true(all(portfolios >= 1 & portfolios <= 5))
  expect_length(portfolios, 500)
})

test_that("smooth bunching produces more distinct interior values than raw quantiles", {
  data <- data.frame(
    id = 1:1000,
    value = c(rep(0, 400), seq(1, 99, length.out = 200), rep(100, 400))
  )

  bp_smooth <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, smooth_bunching = TRUE)
  )
  bp_raw <- compute_breakpoints(
    data,
    "value",
    breakpoint_options(n_portfolios = 5, smooth_bunching = FALSE)
  )

  # Smoothed version should have more distinct breakpoint values
  n_distinct_smooth <- length(unique(round(bp_smooth, 10)))
  n_distinct_raw <- length(unique(round(bp_raw, 10)))
  expect_gte(n_distinct_smooth, n_distinct_raw)
})

test_that("function is deterministic (same input → same output)", {
  set.seed(1)
  data <- data.frame(id = 1:500, value = rnorm(500))
  opts <- breakpoint_options(n_portfolios = 5)

  bp1 <- compute_breakpoints(data, "value", opts)
  bp2 <- compute_breakpoints(data, "value", opts)
  expect_identical(bp1, bp2)
})
