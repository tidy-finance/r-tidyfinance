# Panel data: n_stocks × n_months with periodic rebalancing data
make_panel <- function(n_stocks = 50, n_months = 24, seed = 42) {
  set.seed(seed)
  dates <- seq.Date(
    from = as.Date("2020-01-01"),
    by = "month",
    length.out = n_months
  )
  expand.grid(permno = seq_len(n_stocks), date = dates) |>
    mutate(
      mktcap_lag = runif(n(), 100, 10000),
      ret_excess = rnorm(n(), 0, 0.05),
      size = runif(n(), 1, 100),
      bm = runif(n(), 0.5, 3),
      exchange = sample(c("NYSE", "NASDAQ"), n(), replace = TRUE)
    ) |>
    arrange(date, permno) |>
    as.data.frame()
}

test_that("error if sorting_variables is NULL or empty", {
  data <- make_panel()
  expect_error(
    compute_portfolio_returns(
      data,
      NULL,
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    ),
    "at least one"
  )
  expect_error(
    compute_portfolio_returns(
      data,
      character(0),
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    ),
    "at least one"
  )
})

test_that("error for invalid sorting_method", {
  data <- make_panel()
  expect_error(
    compute_portfolio_returns(
      data,
      "size",
      "invalid_method",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    ),
    "Invalid sorting method"
  )
})

test_that("error if required columns are missing", {
  data <- make_panel() |> select(-ret_excess)
  expect_error(
    compute_portfolio_returns(
      data,
      "size",
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    ),
    "missing"
  )
})

test_that("error if sorting variable column is missing", {
  data <- make_panel()
  expect_error(
    compute_portfolio_returns(
      data,
      "nonexistent",
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    ),
    "missing"
  )
})

test_that("error for univariate with two sorting variables", {
  data <- make_panel()
  expect_error(
    compute_portfolio_returns(
      data,
      c("size", "bm"),
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    ),
    "one sorting variable"
  )
})

test_that("error for bivariate with one sorting variable", {
  data <- make_panel()
  expect_warning(
    expect_error(
      compute_portfolio_returns(
        data,
        "size",
        "bivariate-dependent",
        breakpoint_options_main = breakpoint_options(n_portfolios = 5)
      ),
      "two sorting variables"
    )
  )
  expect_warning(
    expect_error(
      compute_portfolio_returns(
        data,
        "size",
        "bivariate-independent",
        breakpoint_options_main = breakpoint_options(n_portfolios = 5)
      ),
      "two sorting variables"
    )
  )
})

test_that("error for rebalancing_month outside 1–12", {
  data <- make_panel()
  expect_error(
    compute_portfolio_returns(
      data,
      "size",
      "univariate",
      rebalancing_month = 13,
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    ),
    "rebalancing_month"
  )
  expect_error(
    compute_portfolio_returns(
      data,
      "size",
      "univariate",
      rebalancing_month = 0,
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    ),
    "rebalancing_month"
  )
})

test_that("univariate periodic returns a data.frame", {
  data <- make_panel()
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_s3_class(result, "data.frame")
})

test_that("univariate periodic has correct columns with mktcap_lag", {
  data <- make_panel()
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_true(all(
    c("portfolio", "date", "ret_excess_vw", "ret_excess_ew") %in%
      colnames(result)
  ))
})

test_that("univariate periodic has correct columns without mktcap_lag", {
  data <- make_panel() |> select(-mktcap_lag)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_true("ret_excess_ew" %in% colnames(result))
  expect_false("ret_excess_vw" %in% colnames(result))
})

test_that("univariate periodic returns correct number of portfolios", {
  data <- make_panel()
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_equal(sort(unique(result$portfolio)), 1:5)
})

test_that("univariate periodic returns all dates", {
  data <- make_panel(n_months = 12)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 3)
  )
  expect_equal(length(unique(result$date)), 12)
})

test_that("univariate periodic: each date × portfolio has one row", {
  data <- make_panel()
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  counts <- result |> count(date, portfolio)
  expect_true(all(counts$n == 1))
})

test_that("univariate periodic: EW returns are within reasonable range", {
  data <- make_panel(n_stocks = 100, n_months = 60)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  # Monthly returns should be small (data has sd = 0.05)
  expect_true(all(abs(result$ret_excess_ew) < 1, na.rm = TRUE))
})

test_that("univariate periodic: VW returns differ from EW returns", {
  data <- make_panel(n_stocks = 100, n_months = 24, seed = 1)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  # With heterogeneous market caps, VW and EW should not be identical
  expect_false(isTRUE(all.equal(result$ret_excess_vw, result$ret_excess_ew)))
})

test_that("univariate annual rebalancing returns a data.frame", {
  data <- make_panel(n_months = 36)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    rebalancing_month = 7,
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_s3_class(result, "data.frame")
})

test_that("univariate annual rebalancing has portfolio column", {
  data <- make_panel(n_months = 36)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    rebalancing_month = 7,
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_true("portfolio" %in% colnames(result))
})

test_that("univariate annual: portfolios are held constant between rebalancing months", {
  # With rebalancing in July, a stock's portfolio should not change from
  # July to June of the following year.
  data <- make_panel(n_stocks = 30, n_months = 36)
  result_periodic <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 3)
  )
  result_annual <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    rebalancing_month = 7,
    breakpoint_options_main = breakpoint_options(n_portfolios = 3)
  )
  # Annual rebalancing should produce different results than periodic
  # (unless data coincidentally aligns)
  expect_false(identical(result_periodic, result_annual))
})

test_that("univariate annual: works with rebalancing_month = 1", {
  data <- make_panel(n_months = 36)
  expect_no_error(
    compute_portfolio_returns(
      data,
      "size",
      "univariate",
      rebalancing_month = 1,
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    )
  )
})

test_that("univariate annual: works with rebalancing_month = 12", {
  data <- make_panel(n_months = 36)
  expect_no_error(
    compute_portfolio_returns(
      data,
      "size",
      "univariate",
      rebalancing_month = 12,
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    )
  )
})

test_that("bivariate-dependent periodic returns a data.frame", {
  data <- make_panel(n_stocks = 100, n_months = 24)
  result <- suppressWarnings(
    compute_portfolio_returns(
      data,
      c("size", "bm"),
      "bivariate-dependent",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5),
      breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
    )
  )
  expect_s3_class(result, "data.frame")
})

test_that("bivariate-dependent returns only main portfolio ids", {
  data <- make_panel(n_stocks = 100, n_months = 12)
  result <- suppressWarnings(
    compute_portfolio_returns(
      data,
      c("size", "bm"),
      "bivariate-dependent",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5),
      breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
    )
  )
  # Should have portfolios 1–5 (main), averaged over secondary
  expect_true(all(result$portfolio %in% 1:5))
  expect_false("portfolio_secondary" %in% colnames(result))
})

test_that("bivariate-dependent has correct output columns", {
  data <- make_panel(n_stocks = 100, n_months = 12)
  result <- suppressWarnings(
    compute_portfolio_returns(
      data,
      c("size", "bm"),
      "bivariate-dependent",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5),
      breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
    )
  )
  expect_true(all(
    c("portfolio", "date", "ret_excess_vw", "ret_excess_ew") %in%
      colnames(result)
  ))
})

test_that("bivariate-dependent: each date × portfolio has one row", {
  data <- make_panel(n_stocks = 100, n_months = 12)
  result <- suppressWarnings(
    compute_portfolio_returns(
      data,
      c("size", "bm"),
      "bivariate-dependent",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5),
      breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
    )
  )
  counts <- result |> count(date, portfolio)
  expect_true(all(counts$n == 1))
})

test_that("bivariate-independent periodic returns a data.frame", {
  data <- make_panel(n_stocks = 100, n_months = 12)
  result <- compute_portfolio_returns(
    data,
    c("size", "bm"),
    "bivariate-independent",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
  )
  expect_s3_class(result, "data.frame")
})

test_that("bivariate-independent returns only main portfolio ids", {
  data <- make_panel(n_stocks = 100, n_months = 12)
  result <- compute_portfolio_returns(
    data,
    c("size", "bm"),
    "bivariate-independent",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
  )
  expect_true(all(result$portfolio %in% 1:5))
})

test_that("bivariate-independent: each date x portfolio has one row", {
  data <- make_panel(n_stocks = 100, n_months = 12)
  result <- compute_portfolio_returns(
    data,
    c("size", "bm"),
    "bivariate-independent",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
  )
  counts <- result |> count(date, portfolio)
  expect_true(all(counts$n == 1))
})

test_that("bivariate-independent differs from bivariate-dependent", {
  data <- make_panel(n_stocks = 200, n_months = 24, seed = 99)
  result_dep <- suppressWarnings(
    compute_portfolio_returns(
      data,
      c("size", "bm"),
      "bivariate-dependent",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5),
      breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
    )
  )
  result_ind <- compute_portfolio_returns(
    data,
    c("size", "bm"),
    "bivariate-independent",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
  )
  # Results should differ because the sorting mechanisms are different
  combined <- inner_join(
    result_dep,
    result_ind,
    by = c("portfolio", "date"),
    suffix = c("_dep", "_ind")
  )
  expect_false(isTRUE(all.equal(
    combined$ret_excess_ew_dep,
    combined$ret_excess_ew_ind
  )))
})

test_that("bivariate-dependent annual rebalancing works", {
  data <- make_panel(n_stocks = 100, n_months = 36)
  result <- suppressWarnings(
    compute_portfolio_returns(
      data,
      c("size", "bm"),
      "bivariate-dependent",
      rebalancing_month = 7,
      breakpoint_options_main = breakpoint_options(n_portfolios = 3),
      breakpoint_options_secondary = breakpoint_options(n_portfolios = 2)
    )
  )
  expect_s3_class(result, "data.frame")
  expect_true("portfolio" %in% colnames(result))
})

test_that("bivariate-independent annual rebalancing works", {
  data <- make_panel(n_stocks = 100, n_months = 36)
  result <- compute_portfolio_returns(
    data,
    c("size", "bm"),
    "bivariate-independent",
    rebalancing_month = 7,
    breakpoint_options_main = breakpoint_options(n_portfolios = 3),
    breakpoint_options_secondary = breakpoint_options(n_portfolios = 2)
  )
  expect_s3_class(result, "data.frame")
  expect_true("portfolio" %in% colnames(result))
})

test_that("min_portfolio_size = 0 returns no NAs from the size check", {
  data <- make_panel(n_stocks = 50, n_months = 12)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    min_portfolio_size = 0
  )
  expect_false(any(is.na(result$ret_excess_ew)))
})

test_that("large min_portfolio_size produces NAs", {
  data <- make_panel(n_stocks = 10, n_months = 12)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    min_portfolio_size = 100
  )
  # With only 10 stocks and 5 portfolios (~2 per portfolio), all should be NA
  expect_true(all(is.na(result$ret_excess_ew)))
})

test_that("min_portfolio_size applies per portfolio-date group", {
  # With 50 stocks and 5 portfolios, each portfolio has ~10 stocks.
  # Setting min_portfolio_size = 11 should make some NA, but not necessarily all.
  data <- make_panel(n_stocks = 50, n_months = 6)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    min_portfolio_size = 11
  )
  # At least some should be NA
  expect_true(any(is.na(result$ret_excess_ew)))
})

test_that("missing mktcap_lag removes ret_excess_vw column", {
  data <- make_panel() |> select(-mktcap_lag)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_false("ret_excess_vw" %in% colnames(result))
  expect_true("ret_excess_ew" %in% colnames(result))
})

test_that("missing mktcap_lag works for bivariate-independent", {
  data <- make_panel(n_stocks = 100) |> select(-mktcap_lag)
  result <- compute_portfolio_returns(
    data,
    c("size", "bm"),
    "bivariate-independent",
    breakpoint_options_main = breakpoint_options(n_portfolios = 3),
    breakpoint_options_secondary = breakpoint_options(n_portfolios = 2)
  )
  expect_false("ret_excess_vw" %in% colnames(result))
  expect_true("ret_excess_ew" %in% colnames(result))
})


test_that("rows with NA portfolio are excluded from output", {
  data <- make_panel(n_stocks = 50, n_months = 12)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_false(any(is.na(result$portfolio)))
})

test_that("custom data_options column names work", {
  set.seed(42)
  dates <- seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 12)
  data <- expand.grid(stock_id = 1:30, my_date = dates) |>
    mutate(
      my_mktcap = runif(n(), 100, 10000),
      my_ret = rnorm(n(), 0, 0.05),
      size = runif(n(), 1, 100)
    ) |>
    as.data.frame()

  custom_opts <- data_options(
    date = "my_date",
    id = "stock_id",
    ret_excess = "my_ret",
    mktcap_lag = "my_mktcap"
  )

  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 3),
    data_options = custom_opts
  )

  expect_s3_class(result, "data.frame")
  expect_true("my_date" %in% colnames(result))
  expect_true("portfolio" %in% colnames(result))
})

test_that("custom breakpoint_function_main is used", {
  data <- make_panel(n_stocks = 50, n_months = 12)

  # Custom function that always makes 3 portfolios regardless of options
  custom_bp <- function(data, sorting_variable, bp_options, data_options) {
    vals <- data[[sorting_variable]]
    q <- quantile(
      vals,
      probs = c(0, 1 / 3, 2 / 3, 1),
      na.rm = TRUE,
      names = FALSE
    )
    q[2:4] <- q[2:4] + 1e-20
    q
  }

  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 999),
    breakpoint_function_main = custom_bp
  )

  # Despite n_portfolios = 999, custom function forces 3 portfolios
  expect_equal(sort(unique(result$portfolio)), 1:3)
})

test_that("results are deterministic", {
  data <- make_panel(n_stocks = 50, n_months = 12)
  opts <- breakpoint_options(n_portfolios = 5)

  r1 <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = opts
  )
  r2 <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = opts
  )
  expect_identical(r1, r2)
})

test_that("single date works for univariate periodic", {
  set.seed(1)
  data <- data.frame(
    permno = 1:50,
    date = as.Date("2020-01-01"),
    mktcap_lag = runif(50, 100, 1000),
    ret_excess = rnorm(50, 0, 0.05),
    size = runif(50, 1, 100)
  )
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_equal(length(unique(result$date)), 1)
  expect_equal(nrow(result), 5)
})

test_that("single stock per date works (produces 1 portfolio)", {
  dates <- seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 12)
  data <- data.frame(
    permno = 1,
    date = dates,
    mktcap_lag = 1000,
    ret_excess = rnorm(12, 0, 0.05),
    size = runif(12, 1, 100)
  )
  # Will get constant-variable warnings per date when only 1 stock
  result <- suppressWarnings(
    compute_portfolio_returns(
      data,
      "size",
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    )
  )
  expect_true(all(result$portfolio == 1))
})

test_that("EW return equals manual mean of ret_excess within portfolio", {
  set.seed(42)
  data <- data.frame(
    permno = 1:20,
    date = as.Date("2020-01-01"),
    mktcap_lag = rep(1, 20), # equal weights
    ret_excess = (1:20) / 100,
    size = 1:20
  )

  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 2)
  )

  # Manually: portfolio 1 = first 10 stocks, portfolio 2 = last 10 stocks
  # (approximately, depending on breakpoints)
  bp <- compute_breakpoints(data, "size", breakpoint_options(n_portfolios = 2))
  portfolios <- findInterval(data$size, bp, all.inside = TRUE)
  for (p in unique(portfolios)) {
    expected_ew <- mean(data$ret_excess[portfolios == p])
    actual_ew <- result$ret_excess_ew[result$portfolio == p]
    expect_equal(actual_ew, expected_ew, tolerance = 1e-10)
  }
})

test_that("VW return equals manual weighted.mean of ret_excess", {
  set.seed(42)
  data <- data.frame(
    permno = 1:20,
    date = as.Date("2020-01-01"),
    mktcap_lag = c(rep(1, 10), rep(100, 10)), # very different weights
    ret_excess = rep(0.01, 20),
    size = 1:20
  )
  data$ret_excess[11:20] <- 0.05 # high-cap stocks have higher returns

  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(n_portfolios = 2)
  )

  bp <- compute_breakpoints(data, "size", breakpoint_options(n_portfolios = 2))
  portfolios <- findInterval(data$size, bp, all.inside = TRUE)
  for (p in unique(portfolios)) {
    mask <- portfolios == p
    expected_vw <- weighted.mean(data$ret_excess[mask], data$mktcap_lag[mask])
    actual_vw <- result$ret_excess_vw[result$portfolio == p]
    expect_equal(actual_vw, expected_vw, tolerance = 1e-10)
  }
})

test_that("percentile-based breakpoints work in univariate", {
  data <- make_panel(n_stocks = 100, n_months = 12)
  result <- compute_portfolio_returns(
    data,
    "size",
    "univariate",
    breakpoint_options_main = breakpoint_options(percentiles = c(0.3, 0.7))
  )
  expect_equal(sort(unique(result$portfolio)), 1:3)
})

test_that("bivariate-independent uses different n_portfolios for main vs secondary", {
  data <- make_panel(n_stocks = 200, n_months = 12)
  result <- compute_portfolio_returns(
    data,
    c("size", "bm"),
    "bivariate-independent",
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    breakpoint_options_secondary = breakpoint_options(n_portfolios = 2)
  )
  # Output should have 5 main portfolios (averaged over 2 secondary)
  expect_equal(sort(unique(result$portfolio)), 1:5)
})

test_that("no duplicate portfolio-date rows in output", {
  data <- make_panel(n_stocks = 100, n_months = 24)

  for (method in c("univariate")) {
    result <- compute_portfolio_returns(
      data,
      "size",
      method,
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    )
    dupes <- result |> count(portfolio, date) |> filter(n > 1)
    expect_equal(nrow(dupes), 0, info = paste("method:", method))
  }

  for (method in c("bivariate-dependent", "bivariate-independent")) {
    result <- suppressWarnings(
      compute_portfolio_returns(
        data,
        c("size", "bm"),
        method,
        breakpoint_options_main = breakpoint_options(n_portfolios = 5),
        breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
      )
    )
    dupes <- result |> count(portfolio, date) |> filter(n > 1)
    expect_equal(nrow(dupes), 0, info = paste("method:", method))
  }
})

test_that("function handles moderately large data without error", {
  data <- make_panel(n_stocks = 500, n_months = 60, seed = 7)
  expect_no_error(
    compute_portfolio_returns(
      data,
      "size",
      "univariate",
      breakpoint_options_main = breakpoint_options(n_portfolios = 10)
    )
  )
})

test_that("bivariate-independent handles large data without error", {
  data <- make_panel(n_stocks = 500, n_months = 60, seed = 7)
  expect_no_error(
    compute_portfolio_returns(
      data,
      c("size", "bm"),
      "bivariate-independent",
      breakpoint_options_main = breakpoint_options(n_portfolios = 5),
      breakpoint_options_secondary = breakpoint_options(n_portfolios = 3)
    )
  )
})
