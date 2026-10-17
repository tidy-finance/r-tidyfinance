make_engine_data <- function(
  n_stocks = 40L,
  n_months = 10L,
  start = "2020-01-01",
  seed = 7L,
  with_mktcap = TRUE
) {
  set.seed(seed)
  dates <- seq.Date(
    from = as.Date(start),
    by = "month",
    length.out = n_months
  )
  df <- expand.grid(permno = seq_len(n_stocks), date = dates)
  n <- nrow(df)
  df$ret_excess <- rnorm(n)
  df$size <- runif(n, 50, 150)
  df$bm <- runif(n, 0.5, 2)
  df$exchange <- rep(c("NYSE", "NASDAQ", "AMEX"), length.out = n)
  if (with_mktcap) {
    df$mktcap_lag <- runif(n, 100, 1000)
  }
  df
}

bp_main <- breakpoint_options(n_portfolios = 3L)
bp_secondary <- breakpoint_options(n_portfolios = 2L)

# Comparing the two engines is only meaningful when the compiled one is
# actually reached, so assert that before comparing: a gate that silently
# refused everything would make every expectation below compare the baseline
# with itself and pass.
expect_engines_identical <- function(..., accelerated = TRUE) {
  args <- list(...)
  options <- args$data_options %||% data_options()
  reason <- cpp11_engine_reject_reason(
    data = args$data,
    sorting_variables = args$sorting_variables,
    sorting_method = args$sorting_method,
    rebalancing_month = args$rebalancing_month,
    breakpoint_options_main = args$breakpoint_options_main,
    breakpoint_options_secondary = args$breakpoint_options_secondary,
    breakpoint_function_main = compute_breakpoints,
    breakpoint_function_secondary = compute_breakpoints,
    min_portfolio_size = args$min_portfolio_size %||% 1L,
    cap_weight = args$cap_weight %||% 0.8,
    data_options = options,
    quiet = TRUE,
    all_dates = unique(args$data[[options$date]])
  )
  if (accelerated) {
    expect_null(reason)
  } else {
    expect_type(reason, "character")
  }
  from_r <- suppressWarnings(suppressMessages(
    do.call(compute_portfolio_returns, c(args, list(engine = "r")))
  ))
  from_cpp11 <- suppressWarnings(suppressMessages(
    do.call(compute_portfolio_returns, c(args, list(engine = "cpp11")))
  ))
  expect_identical(from_cpp11, from_r)
}

# Equivalence --------------------------------------------------------------

test_that("the engines agree on univariate sorts", {
  for (n_portfolios in c(2L, 3L, 5L)) {
    expect_engines_identical(
      data = make_engine_data(),
      sorting_variables = "size",
      sorting_method = "univariate",
      breakpoint_options_main = breakpoint_options(
        n_portfolios = n_portfolios
      ),
      quiet = TRUE
    )
  }
})

test_that("the engines agree on percentile breakpoints", {
  expect_engines_identical(
    data = make_engine_data(),
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = breakpoint_options(
      percentiles = c(0.2, 0.4, 0.6, 0.8)
    ),
    quiet = TRUE
  )
})

test_that("the engines agree on both bivariate sorts", {
  for (method in c("bivariate-dependent", "bivariate-independent")) {
    expect_engines_identical(
      data = make_engine_data(),
      sorting_variables = c("size", "bm"),
      sorting_method = method,
      breakpoint_options_main = bp_main,
      breakpoint_options_secondary = bp_secondary,
      quiet = TRUE
    )
  }
})

test_that("the engines agree with annual rebalancing", {
  for (method in c("univariate", "bivariate-dependent")) {
    variables <- if (method == "univariate") "size" else c("size", "bm")
    secondary <- if (method == "univariate") NULL else bp_secondary
    expect_engines_identical(
      data = make_engine_data(n_months = 30L),
      sorting_variables = variables,
      sorting_method = method,
      rebalancing_month = 7L,
      breakpoint_options_main = bp_main,
      breakpoint_options_secondary = secondary,
      quiet = TRUE
    )
  }
})

test_that("the engines agree on breakpoint exchanges and size thresholds", {
  expect_engines_identical(
    data = make_engine_data(),
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = breakpoint_options(
      n_portfolios = 3L,
      breakpoints_exchanges = "NYSE"
    ),
    quiet = TRUE
  )
  expect_engines_identical(
    data = make_engine_data(),
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = breakpoint_options(
      n_portfolios = 3L,
      breakpoints_exchanges = "NYSE",
      breakpoints_min_size_threshold = 0.2
    ),
    quiet = TRUE
  )
})

test_that("the engines agree with smooth bunching", {
  expect_engines_identical(
    data = make_engine_data(),
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = breakpoint_options(
      n_portfolios = 5L,
      smooth_bunching = TRUE
    ),
    quiet = TRUE
  )
})

test_that("the engines agree on the weighting arguments", {
  for (cap_weight in c(0, 0.5, 1)) {
    expect_engines_identical(
      data = make_engine_data(),
      sorting_variables = "size",
      sorting_method = "univariate",
      breakpoint_options_main = bp_main,
      cap_weight = cap_weight,
      quiet = TRUE
    )
  }
  for (min_portfolio_size in c(0L, 2L)) {
    expect_engines_identical(
      data = make_engine_data(),
      sorting_variables = "size",
      sorting_method = "univariate",
      breakpoint_options_main = bp_main,
      min_portfolio_size = min_portfolio_size,
      quiet = TRUE
    )
  }
})

test_that("the engines agree without market capitalization", {
  expect_engines_identical(
    data = make_engine_data(with_mktcap = FALSE),
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = bp_main,
    quiet = TRUE
  )
})

test_that("the engines agree with missing returns", {
  data <- make_engine_data()
  data$ret_excess[c(1L, 17L, 200L)] <- NA_real_
  expect_engines_identical(
    data = data,
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = bp_main,
    quiet = TRUE
  )
})

test_that("the engines agree with a constant sorting variable", {
  data <- make_engine_data()
  data$size <- 1
  expect_engines_identical(
    data = data,
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = bp_main,
    quiet = TRUE
  )
})

test_that("implement_portfolio_sort() agrees across engines", {
  data <- make_engine_data()
  options <- portfolio_sort_options(breakpoint_options_main = bp_main)
  from_r <- suppressWarnings(suppressMessages(implement_portfolio_sort(
    data, "size", "univariate",
    portfolio_sort_options = options, quiet = TRUE, engine = "r"
  )))
  from_cpp11 <- suppressWarnings(suppressMessages(implement_portfolio_sort(
    data, "size", "univariate",
    portfolio_sort_options = options, quiet = TRUE, engine = "cpp11"
  )))
  expect_identical(from_cpp11, from_r)
})

# The gate -----------------------------------------------------------------

test_that("a plain list of breakpoint options keeps the R path", {
  # compute_portfolio_returns() accepts a plain list, which has not been
  # through breakpoint_options()'s validation, so the compiled path must
  # refuse it rather than hand unchecked values to the kernels.
  expect_engines_identical(
    data = make_engine_data(),
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = list(n_portfolios = 3L),
    quiet = TRUE,
    accelerated = FALSE
  )
})

test_that("an integer return column keeps the R path", {
  # mean() on an integer vector takes R's single-pass path, which the kernel's
  # two-pass refinement does not reproduce bit for bit.
  data <- make_engine_data()
  data$ret_excess <- as.integer(round(data$ret_excess * 100))
  expect_engines_identical(
    data = data,
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = bp_main,
    quiet = TRUE,
    accelerated = FALSE
  )
})

test_that("duplicated column names keep the R path", {
  data <- make_engine_data()
  data$extra <- data$size
  colnames(data)[colnames(data) == "extra"] <- "size"
  reason <- cpp11_engine_reject_reason(
    data = data,
    sorting_variables = "size",
    sorting_method = "univariate",
    rebalancing_month = NULL,
    breakpoint_options_main = bp_main,
    breakpoint_options_secondary = NULL,
    breakpoint_function_main = compute_breakpoints,
    breakpoint_function_secondary = compute_breakpoints,
    min_portfolio_size = 1L,
    cap_weight = 0.8,
    data_options = data_options(),
    quiet = TRUE,
    all_dates = unique(data$date)
  )
  expect_identical(reason, "`data` has duplicated column names")
})

test_that("a missing date keeps the R path", {
  data <- make_engine_data()
  data$date[1L] <- NA
  reason <- cpp11_engine_reject_reason(
    data = data,
    sorting_variables = "size",
    sorting_method = "univariate",
    rebalancing_month = NULL,
    breakpoint_options_main = bp_main,
    breakpoint_options_secondary = NULL,
    breakpoint_function_main = compute_breakpoints,
    breakpoint_function_secondary = compute_breakpoints,
    min_portfolio_size = 1L,
    cap_weight = 0.8,
    data_options = data_options(),
    quiet = TRUE,
    all_dates = unique(data$date)
  )
  expect_identical(reason, "the date column has missing values")
})

test_that("a custom breakpoint function keeps the R path", {
  wrapper <- function(...) compute_breakpoints(...)
  reason <- cpp11_engine_reject_reason(
    data = make_engine_data(),
    sorting_variables = "size",
    sorting_method = "univariate",
    rebalancing_month = NULL,
    breakpoint_options_main = bp_main,
    breakpoint_options_secondary = NULL,
    breakpoint_function_main = wrapper,
    breakpoint_function_secondary = compute_breakpoints,
    min_portfolio_size = 1L,
    cap_weight = 0.8,
    data_options = data_options(),
    quiet = TRUE,
    all_dates = unique(make_engine_data()$date)
  )
  expect_identical(
    reason,
    "`breakpoint_function_main` is not `compute_breakpoints()`"
  )
})

test_that("a refused call says why unless quiet", {
  expect_message(
    compute_portfolio_returns(
      make_engine_data(),
      "size",
      "univariate",
      breakpoint_options_main = list(n_portfolios = 3L),
      quiet = FALSE,
      engine = "cpp11"
    ),
    "engine"
  )
})

test_that("engine is validated", {
  expect_error(
    compute_portfolio_returns(
      make_engine_data(),
      "size",
      "univariate",
      breakpoint_options_main = bp_main,
      engine = "rcpp"
    )
  )
})
