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

# The engine is refused outright where R accumulates in double while the
# kernels would accumulate wider, which is CRAN's noLD flavour. That is a
# property of the build rather than a defect, so skip there instead of failing.
skip_without_engine <- function() {
  testthat::skip_if(
    !isTRUE(capabilities("long.double")) && long_double_is_wider_cpp11(),
    "this R accumulates in double while the kernels use long double"
  )
}

# Comparing the two engines is only meaningful when the compiled one is
# actually reached, so assert that before comparing: a gate that silently
# refused everything would make every expectation below compare the baseline
# with itself and pass.
expect_engines_identical <- function(..., accelerated = TRUE) {
  skip_without_engine()
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
    testthat::expect_null(reason)
  } else {
    testthat::expect_type(reason, "character")
  }
  from_r <- suppressWarnings(suppressMessages(
    do.call(compute_portfolio_returns, c(args, list(engine = "r")))
  ))
  from_cpp11 <- suppressWarnings(suppressMessages(
    do.call(compute_portfolio_returns, c(args, list(engine = "cpp11")))
  ))
  testthat::expect_identical(from_cpp11, from_r)
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
  skip_without_engine()
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
  skip_without_engine()
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
  skip_without_engine()
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

# Every reject branch ------------------------------------------------------

# The gate names a reason for each kind of input it will not accelerate. These
# cover the branches the equivalence tests above do not reach, which is most of
# them: a reject is only observable through the reason string.
test_that("the gate names a reason for every input it refuses", {
  skip_without_engine()

  base <- list(
    data = make_engine_data(),
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
    all_dates = unique(make_engine_data()$date)
  )
  # Plain assignment, not utils::modifyList(): a data frame is a list, so
  # modifyList() merges `data` column by column instead of replacing it, which
  # silently keeps columns a fixture was built to remove.
  reason_for <- function(changes) {
    args <- base
    for (name in names(changes)) {
      args[[name]] <- changes[[name]]
    }
    do.call(cpp11_engine_reject_reason, args)
  }
  # the unmodified call must be accepted, or nothing below means anything
  expect_null(reason_for(list()))

  tamper <- function(options, field, value) {
    options[[field]] <- value
    options
  }
  no_exchange <- make_engine_data()
  no_exchange$exchange <- NULL
  no_mktcap <- make_engine_data(with_mktcap = FALSE)
  dup_names <- make_engine_data()
  dup_names$dup <- dup_names$size
  colnames(dup_names)[colnames(dup_names) == "dup"] <- "size"
  int_ret <- make_engine_data()
  int_ret$ret_excess <- 1L
  matrix_col <- make_engine_data()
  matrix_col$size <- matrix(matrix_col$size, ncol = 1L)
  chr_date <- make_engine_data()
  chr_date$date <- as.character(chr_date$date)
  na_date <- make_engine_data()
  na_date$date[1L] <- NA
  no_ret <- make_engine_data()
  no_ret$ret_excess <- NULL
  reserved <- make_engine_data(n_months = 30L)
  reserved$portfolio <- 1L

  cases <- list(
    list(list(data = 1L), "`data` is not a data frame"),
    list(
      list(data_options = list()),
      "`data_options` is not a `tidyfinance_data_options` object"
    ),
    list(list(quiet = "yes"), "`quiet` is not a single logical"),
    list(list(sorting_method = 1L), "`sorting_method` is not a single string"),
    list(
      list(sorting_method = "trivariate"),
      "`sorting_method` is not a supported sorting method"
    ),
    list(
      list(sorting_variables = c("size", "bm")),
      "`sorting_variables` does not match `sorting_method`"
    ),
    list(
      list(cap_weight = 1.5),
      "`cap_weight` is not a single finite number in [0, 1]"
    ),
    list(
      list(min_portfolio_size = NA_real_),
      "`min_portfolio_size` is not a single finite number"
    ),
    list(
      list(rebalancing_month = 13L),
      "`rebalancing_month` is not a single month between 1 and 12"
    ),
    list(list(data = dup_names), "`data` has duplicated column names"),
    list(list(data = no_ret), "`data` is missing a required column"),
    list(
      list(data = int_ret),
      "a sorting variable, the return, or the weight is not a double"
    ),
    list(
      list(data = matrix_col),
      "a column the kernels read is a matrix or an array"
    ),
    list(list(data = chr_date), "the date column is not a `Date`"),
    list(list(data = na_date), "the date column has missing values"),
    list(
      list(breakpoint_function_main = function(...) NULL),
      "`breakpoint_function_main` is not `compute_breakpoints()`"
    ),
    list(
      list(breakpoint_options_main = list(n_portfolios = 3L)),
      paste(
        "`breakpoint_options_main` is not a",
        "`tidyfinance_breakpoint_options` object"
      )
    ),
    list(
      list(breakpoint_options_main = tamper(bp_main, "n_portfolios", NULL)),
      paste(
        "`breakpoint_options_main` does not set exactly one of",
        "`n_portfolios` and `percentiles`"
      )
    ),
    list(
      list(breakpoint_options_main = tamper(bp_main, "n_portfolios", 1L)),
      "`breakpoint_options_main` has an unsupported `n_portfolios`"
    ),
    list(
      list(breakpoint_options_main = tamper(
        tamper(bp_main, "n_portfolios", NULL), "percentiles", c(0.5, 1.5)
      )),
      "`breakpoint_options_main` has an unsupported `percentiles`"
    ),
    list(
      list(breakpoint_options_main = tamper(bp_main, "smooth_bunching", NA)),
      "`breakpoint_options_main` has a non-logical `smooth_bunching`"
    ),
    list(
      list(breakpoint_options_main = tamper(
        bp_main, "breakpoints_exchanges", character(0L)
      )),
      "`breakpoint_options_main` has an unsupported `breakpoints_exchanges`"
    ),
    list(
      list(
        data = no_exchange,
        breakpoint_options_main = breakpoint_options(
          n_portfolios = 3L, breakpoints_exchanges = "NYSE"
        )
      ),
      "`breakpoint_options_main` needs an exchange column that `data` lacks"
    ),
    list(
      list(breakpoint_options_main = tamper(
        bp_main, "breakpoints_min_size_threshold", 1.5
      )),
      paste(
        "`breakpoint_options_main` has an unsupported",
        "`breakpoints_min_size_threshold`"
      )
    ),
    list(
      list(
        data = no_mktcap,
        breakpoint_options_main = breakpoint_options(
          n_portfolios = 3L, breakpoints_min_size_threshold = 0.2
        )
      ),
      paste(
        "`breakpoint_options_main` needs a market capitalization column",
        "that `data` lacks"
      )
    ),
    list(
      list(data = reserved, rebalancing_month = 7L),
      "`data` carries a column name the annual join reserves"
    )
  )

  for (case in cases) {
    expect_identical(reason_for(case[[1L]]), case[[2L]])
  }
})

test_that("a bivariate sort checks its secondary options too", {
  skip_without_engine()
  reason <- cpp11_engine_reject_reason(
    data = make_engine_data(),
    sorting_variables = c("size", "bm"),
    sorting_method = "bivariate-dependent",
    rebalancing_month = NULL,
    breakpoint_options_main = bp_main,
    breakpoint_options_secondary = list(n_portfolios = 2L),
    breakpoint_function_main = compute_breakpoints,
    breakpoint_function_secondary = compute_breakpoints,
    min_portfolio_size = 1L,
    cap_weight = 0.8,
    data_options = data_options(),
    quiet = TRUE,
    all_dates = unique(make_engine_data()$date)
  )
  expect_identical(
    reason,
    paste(
      "`breakpoint_options_secondary` is not a",
      "`tidyfinance_breakpoint_options` object"
    )
  )
})

test_that("an oversized portfolio-date grid keeps the R path", {
  skip_without_engine()
  data <- make_engine_data()
  reason <- cpp11_engine_reject_reason(
    data = data,
    sorting_variables = "size",
    sorting_method = "univariate",
    rebalancing_month = NULL,
    breakpoint_options_main = breakpoint_options(n_portfolios = 6e6),
    breakpoint_options_secondary = NULL,
    breakpoint_function_main = compute_breakpoints,
    breakpoint_function_secondary = compute_breakpoints,
    min_portfolio_size = 1L,
    cap_weight = 0.8,
    data_options = data_options(),
    quiet = TRUE,
    all_dates = unique(data$date)
  )
  expect_identical(
    reason,
    "the portfolio-date grid is too large for the compiled engine"
  )
})

# The kernels' own guards --------------------------------------------------

# These call the registered entry points directly, which is how a user can
# reach them (cpp11::cpp_register() exports them as tidyfinance:::*_cpp11).
# Each guard below replaced a memory-safety defect: before them, a stray index
# was an out-of-bounds read or write rather than an R error, so these are the
# regression tests for that hardening as much as they are coverage.
test_that("cap_weights_cpp11() validates its arguments", {
  w <- c(1, 2, 3, 4, 5, 6)
  g <- c(1L, 1L, 2L, 2L, 3L, 3L)
  expect_type(tidyfinance:::cap_weights_cpp11(w, g, 3L, 0.8), "double")

  expect_error(tidyfinance:::cap_weights_cpp11(w, g[1:2], 3L, 0.8), "shorter")
  expect_error(
    tidyfinance:::cap_weights_cpp11(w, replace(g, 1L, 9L), 3L, 0.8),
    "out of range"
  )
  expect_error(
    tidyfinance:::cap_weights_cpp11(w, replace(g, 1L, 0L), 3L, 0.8),
    "out of range"
  )
  expect_error(
    tidyfinance:::cap_weights_cpp11(w, g, 0L, 0.8), "positive count"
  )
  expect_error(
    tidyfinance:::cap_weights_cpp11(w, g, -2L, 0.8), "positive count"
  )
})

test_that("cap_weights_cpp11() clamps a cap outside [0, 1]", {
  # quantile7() used to index past either end of the sorted vector for a prob
  # outside [0, 1], silently returning a different answer rather than erroring.
  w <- c(1, 2, 3, 4, 5, 6)
  g <- c(1L, 1L, 1L, 2L, 2L, 2L)
  at_one <- tidyfinance:::cap_weights_cpp11(w, g, 2L, 1)
  at_zero <- tidyfinance:::cap_weights_cpp11(w, g, 2L, 0)
  expect_identical(tidyfinance:::cap_weights_cpp11(w, g, 2L, 1.5), at_one)
  expect_identical(tidyfinance:::cap_weights_cpp11(w, g, 2L, -0.5), at_zero)
})

test_that("carry_forward_cpp11() validates the ids it indexes with", {
  id <- c(1L, 2L, 3L)
  date <- as.double(as.Date(rep("2020-02-01", 3L)))
  rebal_id <- c(1L, 2L, 3L)
  rebal_date <- as.double(as.Date(rep("2020-01-01", 3L)))
  rebal_upper <- rebal_date + 365

  expect_identical(
    tidyfinance:::carry_forward_cpp11(
      id, date, rebal_id, rebal_date, rebal_upper, 3L
    ),
    c(1L, 2L, 3L)
  )
  # `id` was the one argument the hardening pass missed: it indexes the same
  # buckets as `rebal_id` but was unchecked, so a stray value segfaulted R.
  for (bad in list(c(1L, 2L, 4L), c(1L, 2L, 0L), c(1L, 2L, -7L))) {
    expect_error(
      tidyfinance:::carry_forward_cpp11(
        bad, date, rebal_id, rebal_date, rebal_upper, 3L
      ),
      "out of range"
    )
  }
  expect_error(
    tidyfinance:::carry_forward_cpp11(
      id, date[1:2], rebal_id, rebal_date, rebal_upper, 3L
    ),
    "shorter"
  )
  expect_error(
    tidyfinance:::carry_forward_cpp11(
      id, date, rebal_id, rebal_date, rebal_upper, 0L
    ),
    "positive count"
  )
})

test_that("the aggregation kernels validate their arguments", {
  portfolio <- c(1L, 1L, 2L, 2L)
  dates <- c(1L, 2L, 1L, 2L)
  ret <- c(0.1, 0.2, 0.3, 0.4)
  w <- c(1, 1, 1, 1)

  out <- tidyfinance:::aggregate_univariate_cpp11(
    portfolio, dates, 2L, 2L, ret, w, w, 0
  )
  expect_named(
    out, c("n", "ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped")
  )

  expect_error(
    tidyfinance:::aggregate_univariate_cpp11(
      portfolio, dates[1:2], 2L, 2L, ret, w, w, 0
    ),
    "shorter"
  )
  expect_error(
    tidyfinance:::aggregate_univariate_cpp11(
      portfolio, dates, 0L, 2L, ret, w, w, 0
    ),
    "positive count"
  )
  # a portfolio id outside 1..n_portfolios used to be a heap write
  expect_error(
    tidyfinance:::aggregate_univariate_cpp11(
      replace(portfolio, 1L, 9L), dates, 2L, 2L, ret, w, w, 0
    ),
    "out of range"
  )

  secondary <- c(1L, 1L, 2L, 2L)
  biv <- tidyfinance:::aggregate_bivariate_cpp11(
    portfolio, secondary, dates, 2L, 2L, 2L, ret, w, w, 0
  )
  expect_named(
    biv, c("n", "ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped")
  )
  expect_error(
    tidyfinance:::aggregate_bivariate_cpp11(
      portfolio, secondary[1:2], dates, 2L, 2L, 2L, ret, w, w, 0
    ),
    "shorter"
  )
  expect_error(
    tidyfinance:::aggregate_bivariate_cpp11(
      portfolio, secondary, dates, 2L, 0L, 2L, ret, w, w, 0
    ),
    "positive count"
  )
})

test_that("assign_portfolios_cpp11() rejects probabilities outside [0, 1]", {
  x <- c(1, 2, 3, 4, 5, 6)
  g <- c(1L, 1L, 1L, 2L, 2L, 2L)
  empty <- numeric(0L)
  ok <- tidyfinance:::assign_portfolios_cpp11(
    x, g, 2L, logical(0L), numeric(0L), 0, c(0, 0.5, 1),
    FALSE, FALSE, empty, empty
  )
  expect_named(ok, c("portfolio", "warnings"))

  expect_error(
    tidyfinance:::assign_portfolios_cpp11(
      x, g, 2L, logical(0L), numeric(0L), 0, c(0, 1.5),
      FALSE, FALSE, empty, empty
    ),
    "outside"
  )
  expect_error(
    tidyfinance:::assign_portfolios_cpp11(
      x, g[1:2], 2L, logical(0L), numeric(0L), 0, c(0, 0.5, 1),
      FALSE, FALSE, empty, empty
    ),
    "shorter"
  )
})

test_that("long_double_is_wider_cpp11() reports the build", {
  wider <- tidyfinance:::long_double_is_wider_cpp11()
  expect_type(wider, "logical")
  expect_length(wider, 1L)
  expect_false(is.na(wider))
  # R cannot accumulate more precisely than the compiler can
  if (isTRUE(capabilities("long.double"))) {
    expect_true(wider)
  }
})

# Overflow and non-finite arithmetic ---------------------------------------

# The kernels accumulate in long double and only then narrow to double, the
# way R's summary.c does, so they have branches for a sum that overflows
# double and for a mean that is not finite. Those are reachable from R and
# must agree with mean() and weighted.mean(), which is what these check.
test_that("the aggregation kernel matches mean() when the sum overflows", {
  # Two values whose long double sum is finite but whose double sum is not:
  # r_mean() takes its second pass, dividing before accumulating.
  ret <- c(1e308, 1e308)
  portfolio <- c(1L, 1L)
  dates <- c(1L, 1L)
  w <- c(1, 1)
  out <- tidyfinance:::aggregate_univariate_cpp11(
    portfolio, dates, 1L, 1L, ret, w, w, 0
  )
  expect_identical(out$ret_excess_ew, mean(ret))
  expect_identical(out$ret_excess_vw, stats::weighted.mean(ret, w))
})

test_that("the aggregation kernel matches mean() for infinite returns", {
  w <- c(1, 1)
  for (ret in list(c(Inf, 1), c(-Inf, 1), c(Inf, -Inf), c(Inf, Inf))) {
    out <- tidyfinance:::aggregate_univariate_cpp11(
      c(1L, 1L), c(1L, 1L), 1L, 1L, ret, w, w, 0
    )
    expect_identical(out$ret_excess_ew, mean(ret))
    expect_identical(out$ret_excess_vw, stats::weighted.mean(ret, w))
  }
})

test_that("the weighted mean matches R when the weights overflow double", {
  # sum(w) is finite in long double but infinite in double, so the narrowing
  # branch of ld_to_double() decides the result.
  ret <- c(1, 2, 3)
  w <- c(1e308, 1e308, 1e308)
  out <- tidyfinance:::aggregate_univariate_cpp11(
    c(1L, 1L, 1L), c(1L, 1L, 1L), 1L, 1L, ret, w, w, 0
  )
  expect_identical(out$ret_excess_vw, stats::weighted.mean(ret, w))
})

test_that("a zero weight sum gives NA, as weighted.mean does not", {
  # weighted.mean() returns NaN for sum(w) == 0; the kernels return NA, which
  # is what summarise_portfolio_returns() does with the NaN afterwards.
  out <- tidyfinance:::aggregate_univariate_cpp11(
    c(1L, 1L), c(1L, 1L), 1L, 1L, c(1, 2), c(0, 0), c(0, 0), 0
  )
  expect_true(is.na(out$ret_excess_vw))
})

test_that("the bivariate kernel maps an empty cell mean to NA", {
  # mean_na_rm() over a cell whose values are all missing is NaN, which the
  # kernel maps to NA_real_ rather than leaving as NaN.
  out <- tidyfinance:::aggregate_bivariate_cpp11(
    c(1L, 1L), c(1L, 1L), c(1L, 1L), 1L, 1L, 1L,
    c(NA_real_, NA_real_), c(1, 1), c(1, 1), 0
  )
  expect_true(is.na(out$ret_excess_ew))
  expect_false(is.nan(out$ret_excess_ew))
})

test_that("carry_forward_cpp11() ignores a rebalancing row with no bound", {
  # An NA upper bound means the row cannot carry forward, so the result is NA
  # rather than the row's index.
  date <- as.double(as.Date("2020-06-01"))
  rebal_date <- as.double(as.Date("2020-01-01"))
  expect_identical(
    tidyfinance:::carry_forward_cpp11(
      1L, date, 1L, rebal_date, NA_real_, 1L
    ),
    NA_integer_
  )
  expect_identical(
    tidyfinance:::carry_forward_cpp11(
      1L, date, 1L, rebal_date, rebal_date + 365, 1L
    ),
    1L
  )
  # a date before any rebalancing date carries nothing forward
  expect_identical(
    tidyfinance:::carry_forward_cpp11(
      1L, as.double(as.Date("2019-01-01")), 1L, rebal_date,
      rebal_date + 365, 1L
    ),
    NA_integer_
  )
})

test_that("an NA date or weight is skipped as the baseline skips it", {
  out <- tidyfinance:::aggregate_univariate_cpp11(
    c(1L, 1L, 1L), c(1L, NA_integer_, 1L), 1L, 1L,
    c(1, 99, 3), c(1, 1, 1), c(1, 1, 1), 0
  )
  # the NA-date row contributes nothing, so this is mean(c(1, 3))
  expect_identical(out$ret_excess_ew, mean(c(1, 3)))
  expect_identical(out$n, 2L)
})

# Smooth bunching, with breakpoints that actually bunch --------------------

# smooth_bunching only does anything when an edge quantile coincides with its
# neighbour, i.e. when the sorting variable has a mass point at the bottom or
# the top. A continuous fixture never triggers it, so the whole recomputation
# branch goes unexercised. These fixtures put a mass point where it matters.
bunched_data <- function(edge, n_stocks = 60L, n_months = 8L, seed = 3L) {
  set.seed(seed)
  dates <- seq.Date(as.Date("2020-01-01"), by = "month",
                    length.out = n_months)
  df <- expand.grid(permno = seq_len(n_stocks), date = dates)
  n <- nrow(df)
  df$ret_excess <- rnorm(n)
  df$mktcap_lag <- runif(n, 100, 1000)
  df$exchange <- "NYSE"
  # a third of the cross-section sits exactly on the floor, a third exactly on
  # the ceiling, so the 0th and 1st, or the last two, quantiles coincide
  value <- runif(n, 2, 9)
  low <- df$permno <= n_stocks %/% 3L
  high <- df$permno > n_stocks - n_stocks %/% 3L
  if (edge %in% c("lower", "both")) value[low] <- 1
  if (edge %in% c("upper", "both")) value[high] <- 10
  df$size <- value
  df
}

test_that("an edge mass point really does bunch the breakpoints", {
  # guard the fixture itself: if these are not equal, the tests below are
  # exercising the ordinary path and prove nothing about smooth bunching
  for (edge in c("lower", "upper", "both")) {
    d <- bunched_data(edge)
    first_date <- d[d$date == min(d$date), ]
    bp <- stats::quantile(
      first_date$size, probs = seq(0, 1, length.out = 6L), type = 7
    )
    if (edge %in% c("lower", "both")) {
      expect_identical(unname(bp[1L]), unname(bp[2L]))
    }
    if (edge %in% c("upper", "both")) {
      expect_identical(unname(bp[5L]), unname(bp[6L]))
    }
  }
})

test_that("the engines agree when the breakpoints bunch", {
  for (edge in c("lower", "upper", "both")) {
    expect_engines_identical(
      data = bunched_data(edge),
      sorting_variables = "size",
      sorting_method = "univariate",
      breakpoint_options_main = breakpoint_options(
        n_portfolios = 5L, smooth_bunching = TRUE
      ),
      quiet = TRUE
    )
  }
})

test_that("the engines agree when bunching meets percentiles", {
  # the percentile route additionally raises the smooth_percentiles warning
  expect_engines_identical(
    data = bunched_data("both"),
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = breakpoint_options(
      percentiles = c(0.2, 0.4, 0.6, 0.8), smooth_bunching = TRUE
    ),
    quiet = TRUE
  )
})

test_that("the engines agree when bunching meets a size threshold", {
  expect_engines_identical(
    data = bunched_data("both"),
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = breakpoint_options(
      n_portfolios = 5L,
      smooth_bunching = TRUE,
      breakpoints_exchanges = "NYSE",
      breakpoints_min_size_threshold = 0.2
    ),
    quiet = TRUE
  )
})

test_that("the engines agree when bunching meets a bivariate sort", {
  expect_engines_identical(
    data = bunched_data("both"),
    sorting_variables = c("size", "mktcap_lag"),
    sorting_method = "bivariate-dependent",
    breakpoint_options_main = breakpoint_options(
      n_portfolios = 5L, smooth_bunching = TRUE
    ),
    breakpoint_options_secondary = bp_secondary,
    quiet = TRUE
  )
})

# Cell and weight edge cases in the kernels --------------------------------

test_that("a cell below min_size is all NA", {
  out <- tidyfinance:::aggregate_univariate_cpp11(
    c(1L, 1L), c(1L, 1L), 1L, 1L, c(0.1, 0.2), c(1, 1), c(1, 1), 5
  )
  expect_identical(out$n, 2L)
  expect_true(is.na(out$ret_excess_ew))
  expect_true(is.na(out$ret_excess_vw))
  expect_true(is.na(out$ret_excess_vw_capped))
})

test_that("cap_weights_cpp11() handles missing weights", {
  # a missing weight becomes zero, as the baseline zero-fills it
  out <- tidyfinance:::cap_weights_cpp11(
    c(1, NA, 3, 4), c(1L, 1L, 1L, 1L), 1L, 0.8
  )
  expect_identical(out[[2L]], 0)
  # When every weight in a group is missing, the cap is NA too, because its
  # sample holds only the non-missing weights. The missing-weight branch is
  # checked first though, so each weight still becomes zero rather than NA -
  # which also means the NA-cap branch in cap_weights() cannot be reached: it
  # needs a present weight alongside an absent cap, and the cap is absent only
  # when no weight is present.
  all_na <- tidyfinance:::cap_weights_cpp11(
    c(NA_real_, NA_real_), c(1L, 1L), 1L, 0.8
  )
  expect_identical(all_na, c(0, 0))
})

test_that("the engines agree when a breakpoint exchange holds no stock", {
  # the reference sample is empty on one date, so no breakpoints exist there
  d <- bunched_data("lower")
  d$exchange[d$date == min(d$date)] <- "NASDAQ"
  expect_engines_identical(
    data = d,
    sorting_variables = "size",
    sorting_method = "univariate",
    breakpoint_options_main = breakpoint_options(
      n_portfolios = 5L, breakpoints_exchanges = "NYSE"
    ),
    quiet = TRUE
  )
})
