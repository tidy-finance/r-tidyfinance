make_data <- function(
  n_stocks = 30L,
  n_months = 6L,
  start = "2020-01-01",
  seed = 42L,
  with_mktcap = TRUE
) {
  set.seed(seed)
  dates <- seq.Date(
    from = as.Date(start),
    by = "month",
    length.out = n_months
  )
  df <- expand.grid(
    permno = seq_len(n_stocks),
    date = dates
  )
  n <- nrow(df)
  df$ret_excess <- rnorm(n)
  df$size <- runif(n, 50, 150)
  df$bm <- runif(n, 0.5, 2)
  if (with_mktcap) {
    df$mktcap_lag <- runif(n, 100, 1000)
  }
  df
}

bp3 <- breakpoint_options(n_portfolios = 3L)
bp2 <- breakpoint_options(n_portfolios = 2L)

# Argument validation -----------------------------------------------------

test_that("quiet must be a single logical", {
  expect_error(
    compute_portfolio_returns(
      make_data(),
      "size",
      "univariate",
      breakpoint_options_main = bp3,
      quiet = "yes"
    ),
    "quiet"
  )
})

test_that("sorting_variables cannot be empty", {
  expect_error(
    compute_portfolio_returns(
      make_data(),
      character(0L),
      "univariate",
      breakpoint_options_main = bp3
    ),
    "sorting variable"
  )
})

test_that("invalid sorting_method raises error", {
  expect_error(
    compute_portfolio_returns(
      make_data(),
      "size",
      "bad-method",
      breakpoint_options_main = bp3
    ),
    "sorting method"
  )
})

test_that("bivariate without secondary breakpoints errors", {
  expect_warning(
    try(
      compute_portfolio_returns(
        make_data(),
        c("size", "bm"),
        "bivariate-dependent",
        breakpoint_options_main = bp3,
        quiet = TRUE
      ),
      silent = TRUE
    ),
    "breakpoint_options_secondary"
  )
})

test_that("cap_weight outside [0, 1] raises error", {
  expect_error(
    compute_portfolio_returns(
      make_data(),
      "size",
      "univariate",
      breakpoint_options_main = bp3,
      cap_weight = 2
    ),
    "cap_weight"
  )
})

test_that("missing column raises error", {
  expect_error(
    compute_portfolio_returns(
      make_data(),
      "no_such_col",
      "univariate",
      breakpoint_options_main = bp3
    ),
    "Missing columns"
  )
})

test_that("rebalancing_month outside 1-12 raises error", {
  expect_error(
    compute_portfolio_returns(
      make_data(),
      "size",
      "univariate",
      rebalancing_month = 13L,
      breakpoint_options_main = bp3
    ),
    "rebalancing_month"
  )
})

# Univariate – periodic ---------------------------------------------------

test_that("univariate periodic without mktcap returns ew only", {
  result <- compute_portfolio_returns(
    make_data(with_mktcap = FALSE),
    "size",
    "univariate",
    breakpoint_options_main = bp3,
    quiet = TRUE
  )
  expect_named(result, c("portfolio", "date", "ret_excess_ew"))
  expect_equal(nrow(result), 3L * 6L)
})

test_that("univariate periodic with mktcap returns all ret cols", {
  result <- compute_portfolio_returns(
    make_data(),
    "size",
    "univariate",
    breakpoint_options_main = bp3,
    quiet = TRUE
  )
  expect_named(
    result,
    c(
      "portfolio",
      "date",
      "ret_excess_vw",
      "ret_excess_ew",
      "ret_excess_vw_capped"
    )
  )
  expect_equal(nrow(result), 3L * 6L)
})

test_that("all-NA mktcap_lag gives NA value-weighted returns", {
  # Exercises sum(w_col) == 0 branch in summarise_portfolio_returns
  data <- make_data()
  data$mktcap_lag <- NA_real_
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = bp3,
    quiet = TRUE
  )
  expect_true(all(is.na(result$ret_excess_vw)))
})

test_that("all-NA sorting var emits message and returns 0 rows", {
  # Exercises early-exit empty-panel path (quiet = FALSE branch)
  data <- make_data()
  data$size <- NA_real_
  expect_message(
    result <- compute_portfolio_returns(
      data,
      "size",
      "univariate",
      breakpoint_options_main = bp3
    ),
    "empty panel"
  )
  expect_equal(nrow(result), 0L)
})

test_that("univariate sort with two variables raises error", {
  expect_error(
    compute_portfolio_returns(
      make_data(),
      c("size", "bm"),
      "univariate",
      breakpoint_options_main = bp3
    ),
    "one sorting variable"
  )
})

# Univariate – annual -----------------------------------------------------

test_that("univariate annual: no data in month raises early error", {
  # Jan-Jun data only; rebalancing month = December
  expect_error(
    compute_portfolio_returns(
      make_data(n_months = 6L, start = "2020-01-01"),
      "size",
      "univariate",
      rebalancing_month = 12L,
      breakpoint_options_main = bp3
    ),
    "No observations match"
  )
})

test_that("univariate annual rebalancing produces valid output", {
  # 14 months from July -> two July rebalancing dates
  result <- compute_portfolio_returns(
    make_data(n_months = 14L, start = "2019-07-01"),
    "size",
    "univariate",
    rebalancing_month = 7L,
    breakpoint_options_main = bp3,
    quiet = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})

test_that("min_portfolio_size produces NAs and emits message", {
  # Exercises quiet = FALSE && n_missing > 0 branch
  expect_message(
    result <- compute_portfolio_returns(
      make_data(n_stocks = 5L),
      "size",
      "univariate",
      breakpoint_options_main = bp3,
      min_portfolio_size = 10L
    ),
    "missing value"
  )
  expect_true(any(is.na(result$ret_excess_ew)))
})

# Bivariate-dependent -----------------------------------------------------

test_that("bivariate-dependent with one variable raises error", {
  expect_error(
    compute_portfolio_returns(
      make_data(),
      "size",
      "bivariate-dependent",
      breakpoint_options_main = bp3,
      breakpoint_options_secondary = bp2
    ),
    "two sorting variables"
  )
})

test_that("bivariate-dependent periodic produces valid output", {
  result <- compute_portfolio_returns(
    make_data(),
    c("size", "bm"),
    "bivariate-dependent",
    breakpoint_options_main = bp3,
    breakpoint_options_secondary = bp2,
    quiet = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})

test_that("bivariate-dependent annual produces valid output", {
  result <- compute_portfolio_returns(
    make_data(n_months = 14L, start = "2019-07-01"),
    c("size", "bm"),
    "bivariate-dependent",
    rebalancing_month = 7L,
    breakpoint_options_main = bp3,
    breakpoint_options_secondary = bp2,
    quiet = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})

test_that("bivariate-dependent annual no-match hits bottom error", {
  conds <- list()

  tryCatch(
    withCallingHandlers(
      compute_portfolio_returns(
        make_data(n_months = 6L, start = "2020-01-01"),
        c("size", "bm"),
        "bivariate-dependent",
        rebalancing_month = 12L,
        breakpoint_options_main = bp3,
        breakpoint_options_secondary = bp2,
        quiet = TRUE
      ),
      warning = function(w) {
        conds[[length(conds) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      conds[[length(conds) + 1L]] <<- e
    }
  )

  # Assert a warning was raised
  warnings <- Filter(function(c) inherits(c, "warning"), conds)
  expect_gt(length(warnings), 0L)

  # Assert an error with the right message was raised
  errors <- Filter(function(c) inherits(c, "error"), conds)
  expect_gt(length(errors), 0L)
  expect_match(conditionMessage(errors[[1L]]), "data match")
})

test_that("all-NA mktcap in bivariate coerces NaN vw to NA", {
  # Exercises is.nan(x) branch in aggregate_bivariate_returns:
  # every cell vw is NA (sum of weights == 0), so mean(NA, na.rm=TRUE)
  # produces NaN, which is then replaced with NA_real_.
  data <- make_data()
  data$mktcap_lag <- NA_real_
  result <- compute_portfolio_returns(
    data,
    c("size", "bm"),
    "bivariate-dependent",
    breakpoint_options_main = bp3,
    breakpoint_options_secondary = bp2,
    quiet = TRUE
  )
  expect_true(all(is.na(result$ret_excess_vw)))
})

# Bivariate-independent ---------------------------------------------------

test_that("bivariate-independent with one variable raises error", {
  expect_error(
    compute_portfolio_returns(
      make_data(),
      "size",
      "bivariate-independent",
      breakpoint_options_main = bp3,
      breakpoint_options_secondary = bp2
    ),
    "two sorting variables"
  )
})

test_that("bivariate-independent periodic produces valid output", {
  result <- compute_portfolio_returns(
    make_data(),
    c("size", "bm"),
    "bivariate-independent",
    breakpoint_options_main = bp3,
    breakpoint_options_secondary = bp2,
    quiet = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})

test_that("bivariate-independent annual produces valid output", {
  result <- compute_portfolio_returns(
    make_data(n_months = 14L, start = "2019-07-01"),
    c("size", "bm"),
    "bivariate-independent",
    rebalancing_month = 7L,
    breakpoint_options_main = bp3,
    breakpoint_options_secondary = bp2,
    quiet = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_gt(nrow(result), 0L)
})
