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

# Capped value weights --------------------------------------------------------

cap_panel <- function() {
  # One date, two portfolios. NYSE stocks are small, NASDAQ stocks are large,
  # so the NYSE and the all-stock percentiles differ.
  tibble::tibble(
    permno = 1:10,
    date = as.Date("2020-01-01"),
    size = 1:10,
    exchange = rep(c("NYSE", "NASDAQ"), each = 5),
    mktcap_lag = c(1, 2, 3, 4, 5, 60, 70, 80, 90, 100),
    ret_excess = c(rep(0.01, 5), 0.02, 0.03, 0.04, 0.05, 0.06)
  )
}

expected_capped <- function(data, cap) {
  w <- pmin(data$mktcap_lag, cap)
  top <- data$size > 5
  c(
    stats::weighted.mean(data$ret_excess[!top], w[!top]),
    stats::weighted.mean(data$ret_excess[top], w[top])
  )
}

test_that("capped weights use the NYSE percentile by default", {
  data <- cap_panel()
  out <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 2),
    quiet = TRUE
  )
  nyse_cap <- stats::quantile(1:5, 0.8, names = FALSE)
  expect_equal(out$ret_excess_vw_capped, expected_capped(data, nyse_cap))
})

test_that("cap_weight_exchanges = NULL caps at the percentile of all stocks", {
  data <- cap_panel()
  out <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 2),
    cap_weight_exchanges = NULL,
    quiet = TRUE
  )
  all_cap <- stats::quantile(data$mktcap_lag, 0.8, names = FALSE)
  expect_equal(out$ret_excess_vw_capped, expected_capped(data, all_cap))
})

test_that("capping falls back to all stocks without an exchange column", {
  data <- dplyr::select(cap_panel(), -"exchange")
  rlang::local_options(rlib_message_verbosity = "verbose")
  expect_message(
    out <- compute_portfolio_returns(
      data,
      "size",
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 2)
    ),
    "not found"
  )
  all_cap <- stats::quantile(data$mktcap_lag, 0.8, names = FALSE)
  expect_equal(out$ret_excess_vw_capped, expected_capped(data, all_cap))
})

test_that("capping falls back to all stocks on dates without NYSE stocks", {
  data <- cap_panel() |>
    dplyr::mutate(exchange = "NASDAQ")
  expect_message(
    out <- compute_portfolio_returns(
      data,
      "size",
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 2)
    ),
    "on 1 date"
  )
  all_cap <- stats::quantile(data$mktcap_lag, 0.8, names = FALSE)
  expect_equal(out$ret_excess_vw_capped, expected_capped(data, all_cap))
})

test_that("cap_weight_exchanges is validated", {
  expect_error(
    compute_portfolio_returns(
      cap_panel(),
      "size",
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 2),
      cap_weight_exchanges = 1
    ),
    "cap_weight_exchanges"
  )
})

# Dependent sorts with a minimum size threshold -------------------------------

test_that("dependent sorts compute the size cutoff once per date", {
  # Secondary sort splits at the median size. The main breakpoints exclude
  # stocks below the 25th size percentile of the whole date (25.75), not of
  # each size half. In the small half, the main median is then 38 (stocks 26
  # to 50), so stock 35 is in the low portfolio. A cutoff within the small
  # half (13.25) would set the median at 32 and put stock 35 in the high one.
  data <- tibble::tibble(
    permno = 1:100,
    date = as.Date("2020-01-01"),
    mktcap_lag = as.numeric(1:100),
    x = as.numeric(1:100),
    ret_excess = 0.01
  )
  out <- compute_portfolio_returns(
    data,
    c("x", "mktcap_lag"),
    "bivariate-dependent",
    breakpoint_options_main = breakpoint_options(
      n_portfolios = 2,
      breakpoints_min_size_threshold = 0.25
    ),
    breakpoint_options_secondary = breakpoint_options(n_portfolios = 2),
    cap_weight_exchanges = NULL,
    quiet = TRUE
  )
  expect_equal(nrow(out), 2L)

  assigned <- data |>
    dplyr::mutate(
      portfolio_secondary = as.integer(mktcap_lag > 50) + 1L
    )
  portfolio_main <- assign_portfolio_dependent(
    assigned,
    sorting_variable = "x",
    breakpoint_options = breakpoint_options(
      n_portfolios = 2,
      breakpoints_min_size_threshold = 0.25
    ),
    breakpoint_function = compute_breakpoints,
    data_options = data_options()
  )
  expect_equal(portfolio_main[35], 1L)
  expect_equal(portfolio_main[40], 2L)
  # The large half is unaffected: all its stocks are above the cutoff.
  expect_equal(portfolio_main[75], 1L)
  expect_equal(portfolio_main[76], 2L)
})
