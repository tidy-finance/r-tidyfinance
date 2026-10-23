muffle_constant_warning <- function(code) {
  withCallingHandlers(
    code,
    warning = function(w) {
      if (grepl("constant", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Balanced panel: every stock in every month, so a staggered sort with K
# cohorts equals the average of K monthly sorts on the signal lagged by
# 0, ..., K - 1 months.
staggered_panel <- function(n_stocks = 40L, n_months = 12L) {
  set.seed(324)
  tidyr::expand_grid(
    permno = seq_len(n_stocks),
    date = seq(as.Date("2020-01-01"), by = "month", length.out = n_months)
  ) |>
    dplyr::mutate(
      signal = stats::rnorm(dplyr::n()),
      size = stats::runif(dplyr::n(), 1, 100),
      mktcap_lag = stats::runif(dplyr::n(), 10, 1000),
      ret_excess = stats::rnorm(dplyr::n(), 0.01, 0.05)
    ) |>
    # Missing market caps must not enter the cap percentile as zeros (#337)
    dplyr::mutate(
      mktcap_lag = dplyr::if_else(
        dplyr::row_number() %% 9L == 0L, NA_real_, .data$mktcap_lag
      )
    )
}

lagged_sort_average <- function(data, holding_period, skip = 0L, ...) {
  lags <- skip + seq_len(holding_period) - 1L
  purrr::map(lags, \(k) {
    data |>
      dplyr::group_by(.data$permno) |>
      dplyr::mutate(
        dplyr::across(c("signal", "size"), \(x) dplyr::lag(x, k))
      ) |>
      dplyr::ungroup() |>
      compute_portfolio_returns(..., cap_weight_exchanges = NULL, quiet = TRUE)
  }) |>
    dplyr::bind_rows() |>
    dplyr::group_by(.data$portfolio, .data$date) |>
    dplyr::summarise(
      dplyr::across(dplyr::starts_with("ret_excess"), mean),
      .groups = "drop"
    )
}

test_that("staggered univariate sort averages K lagged monthly sorts", {
  data <- staggered_panel()
  out <- compute_portfolio_returns(
    data,
    "signal",
    "univariate",
    holding_period = 3L,
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    cap_weight_exchanges = NULL,
    quiet = TRUE
  )
  expected <- lagged_sort_average(
    data,
    3L,
    sorting_variables = "signal",
    sorting_method = "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  ) |>
    dplyr::filter(date >= as.Date("2020-03-01"))

  expect_equal(min(out$date), as.Date("2020-03-01"))
  expect_equal(nrow(out), 5L * 10L)
  out <- dplyr::arrange(out, portfolio, date)
  expected <- dplyr::arrange(expected, portfolio, date)
  expect_equal(out$ret_excess_ew, expected$ret_excess_ew)
  expect_equal(out$ret_excess_vw, expected$ret_excess_vw)
  expect_equal(out$ret_excess_vw_capped, expected$ret_excess_vw_capped)
})

test_that("staggered sort with skip holds cohorts from later periods", {
  data <- staggered_panel()
  out <- compute_portfolio_returns(
    data,
    "signal",
    "univariate",
    holding_period = 2L,
    skip = 1L,
    breakpoint_options_main = breakpoint_options(n_portfolios = 2),
    cap_weight_exchanges = NULL,
    quiet = TRUE
  ) |>
    dplyr::arrange(portfolio, date)
  expected <- lagged_sort_average(
    data,
    2L,
    skip = 1L,
    sorting_variables = "signal",
    sorting_method = "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 2)
  ) |>
    dplyr::filter(date >= as.Date("2020-03-01")) |>
    dplyr::arrange(portfolio, date)

  expect_equal(min(out$date), as.Date("2020-03-01"))
  expect_equal(out$ret_excess_ew, expected$ret_excess_ew)
})

test_that("staggered bivariate sorts average K lagged monthly sorts", {
  data <- staggered_panel(n_stocks = 60L)
  for (method in c("bivariate-dependent", "bivariate-independent")) {
    args <- list(
      sorting_variables = c("signal", "size"),
      sorting_method = method,
      breakpoint_options_main = breakpoint_options(n_portfolios = 3),
      breakpoint_options_secondary = breakpoint_options(n_portfolios = 2)
    )
    out <- do.call(
      compute_portfolio_returns,
      c(
        list(data, holding_period = 2L, cap_weight_exchanges = NULL),
        args,
        quiet = TRUE
      )
    ) |>
      dplyr::arrange(portfolio, date)
    expected <- do.call(lagged_sort_average, c(list(data, 2L), args)) |>
      dplyr::filter(date >= as.Date("2020-02-01")) |>
      dplyr::arrange(portfolio, date)

    expect_equal(out$ret_excess_ew, expected$ret_excess_ew, info = method)
    expect_equal(out$ret_excess_vw, expected$ret_excess_vw, info = method)
  }
})

test_that("held stocks stay in their cohort without a sorting variable", {
  # Two stocks, two portfolios. Stock 2 has no signal in February, but stays
  # in the high portfolio of the January cohort.
  data <- tibble::tibble(
    permno = rep(1:2, times = 2),
    date = rep(as.Date(c("2020-01-01", "2020-02-01")), each = 2),
    signal = c(1, 2, 1, NA),
    mktcap_lag = 1,
    ret_excess = c(0, 0, 0.1, 0.2)
  )
  out <- muffle_constant_warning(compute_portfolio_returns(
    data,
    "signal",
    "univariate",
    holding_period = 2L,
    breakpoint_options_main = breakpoint_options(n_portfolios = 2),
    cap_weight_exchanges = NULL,
    quiet = TRUE
  ))
  feb <- out[out$date == as.Date("2020-02-01"), ]
  # Low: January cohort (stock 1) and February cohort (stock 1) -> 0.1.
  # High: only the January cohort holds stock 2 -> 0.2.
  expect_equal(feb$ret_excess_ew[feb$portfolio == 1], 0.1)
  expect_equal(feb$ret_excess_ew[feb$portfolio == 2], 0.2)
  expect_false(any(out$date == as.Date("2020-01-01")))
})

test_that("holding_period and skip are validated", {
  data <- staggered_panel(n_stocks = 4L, n_months = 3L)
  args <- list(
    data,
    "signal",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 2),
    quiet = TRUE
  )
  expect_error(
    do.call(compute_portfolio_returns, c(args, holding_period = 0)),
    "holding_period"
  )
  expect_error(
    do.call(compute_portfolio_returns, c(args, holding_period = 1.5)),
    "holding_period"
  )
  expect_error(
    do.call(compute_portfolio_returns, c(args, skip = -1)),
    "skip"
  )
  expect_error(
    do.call(
      compute_portfolio_returns,
      c(args, holding_period = 3, rebalancing_month = 7)
    ),
    "rebalancing_month"
  )
})

test_that("holding_period = 1 and skip = 0 reproduce monthly sorts", {
  data <- staggered_panel()
  args <- list(
    data,
    "signal",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    cap_weight_exchanges = NULL,
    quiet = TRUE
  )
  expect_identical(
    do.call(compute_portfolio_returns, c(args, holding_period = 1L, skip = 0L)),
    do.call(compute_portfolio_returns, args)
  )
})

test_that("staggered sorts screen only at formation", {
  # Stock 1 passes the price screen in January but not in February. With
  # formation screens, it stays in the January cohort in February (-0.5),
  # while the February cohort's low portfolio holds stock 2 (0.1), so the
  # February low return is (-0.5 + 0.1) / 2. With screens in every period,
  # stock 1 also leaves the January cohort, so only the February cohort
  # remains (0.1).
  data <- tibble::tibble(
    permno = rep(1:2, times = 2),
    date = rep(as.Date(c("2020-01-01", "2020-02-01")), each = 2),
    signal = c(1, 2, 1, 2),
    prc_lag = c(10, 10, 1, 10),
    mktcap_lag = 1,
    ret_excess = c(0, 0, -0.5, 0.1)
  )
  options <- portfolio_sort_options(
    filter_options = filter_options(min_stock_price = 5),
    breakpoint_options_main = breakpoint_options(n_portfolios = 2)
  )
  out <- muffle_constant_warning(implement_portfolio_sort(
    data,
    "signal",
    "univariate",
    holding_period = 2L,
    portfolio_sort_options = options,
    cap_weight_exchanges = NULL,
    quiet = TRUE
  ))
  feb_low <- out$ret_excess_ew[
    out$portfolio == 1 & out$date == as.Date("2020-02-01")
  ]
  expect_equal(feb_low, (-0.5 + 0.1) / 2)

  options_ex_post <- portfolio_sort_options(
    filter_options = filter_options(
      min_stock_price = 5,
      timing = "every_period"
    ),
    breakpoint_options_main = breakpoint_options(n_portfolios = 2)
  )
  rlang::local_options(rlib_warning_verbosity = "verbose")
  expect_warning(
    out_ex_post <- muffle_constant_warning(implement_portfolio_sort(
      data,
      "signal",
      "univariate",
      holding_period = 2L,
      portfolio_sort_options = options_ex_post,
      cap_weight_exchanges = NULL,
      quiet = TRUE
    )),
    "look-ahead"
  )
  feb_low_ex_post <- out_ex_post$ret_excess_ew[
    out_ex_post$portfolio == 1 &
      out_ex_post$date == as.Date("2020-02-01")
  ]
  expect_equal(feb_low_ex_post, 0.1)
})

test_that("a holding period longer than the sample returns an empty panel", {
  data <- staggered_panel(n_stocks = 10L, n_months = 3L)
  out <- compute_portfolio_returns(
    data,
    "signal",
    "univariate",
    holding_period = 6L,
    breakpoint_options_main = breakpoint_options(n_portfolios = 2),
    cap_weight_exchanges = NULL,
    quiet = TRUE
  )
  expect_equal(nrow(out), 0L)
})

test_that("held stocks are capped on dates without formation rows", {
  # No signals in March: no cohort is formed, but the February cohort is
  # still held. Its capped weights use the cap of all stocks in March.
  data <- staggered_panel(n_stocks = 20L, n_months = 3L) |>
    dplyr::mutate(
      signal = dplyr::if_else(date == as.Date("2020-03-01"), NA, signal)
    )
  out <- compute_portfolio_returns(
    data,
    "signal",
    "univariate",
    holding_period = 2L,
    breakpoint_options_main = breakpoint_options(n_portfolios = 2),
    cap_weight_exchanges = NULL,
    quiet = TRUE
  )
  march <- out[out$date == as.Date("2020-03-01"), ]
  expect_equal(nrow(march), 2L)
  expect_false(anyNA(march$ret_excess_vw_capped))
})
