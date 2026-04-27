set.seed(42)

# Minimal stock-return panel used throughout these tests
panel <- tibble::tibble(
  permno = rep(1:20, each = 6),
  date = rep(
    seq.Date(as.Date("2020-01-01"), by = "month", length.out = 6),
    20
  ),
  ret_excess = rnorm(120),
  mktcap_lag = runif(120, 100, 1000),
  bm = rnorm(120)
)

test_that("check_new_col does not warn for genuinely new column names", {
  expect_no_warning(check_new_col(panel, "bm_lag"))
})

test_that("check_new_col does not warn when cols is empty", {
  expect_no_warning(check_new_col(panel, character(0)))
})

test_that("check_new_col warns when ret_excess would be overwritten", {
  expect_warning(
    check_new_col(panel, "ret_excess"),
    "[Oo]verwrit"
  )
})

test_that(
  paste0(
    "check_new_col warns when multiple ",
    "portfolio columns would be overwritten"
  ),
  {
    data_with_portfolios <- tibble::add_column(
      panel,
      portfolio_main = NA_integer_,
      portfolio_secondary = NA_integer_
    )
    expect_warning(
      check_new_col(
        data_with_portfolios,
        c("portfolio_main", "portfolio_secondary")
      ),
      "[Oo]verwrit"
    )
  }
)

test_that("check_new_col warning message contains the offending column name", {
  expect_warning(
    check_new_col(panel, "mktcap_lag"),
    "mktcap_lag"
  )
})

test_that(
  paste0(
    "check_new_col uses the 'Overwriting' ",
    "branch when caller has no name"
  ),
  {
    expect_warning(
      (function() check_new_col(panel, "ret_excess"))(),
      "Overwriting"
    )
  }
)

test_that(
  paste0(
    "check_new_col emits exactly one warning ",
    "even for multiple overlapping cols"
  ),
  {
    data_with_both <- tibble::add_column(
      panel,
      bm_lag = NA_real_,
      ret_excess_lag = NA_real_
    )
    w <- capture_warnings(
      check_new_col(data_with_both, c("bm_lag", "ret_excess_lag"))
    )
    expect_length(w, 1)
  }
)

test_that(
  paste0(
    "check_new_col warns only for existing ",
    "columns, not new ones in the same call"
  ),
  {
    w <- capture_warnings(check_new_col(panel, c("bm", "bm_lag")))
    expect_length(w, 1)
  }
)

test_that("check_new_col returns the input data unchanged when no overlap", {
  result <- check_new_col(panel, "bm_lag")
  expect_identical(result, panel)
})

test_that(
  paste0(
    "check_new_col returns the input data ",
    "unchanged when overlap triggers a warning"
  ),
  {
    result <- suppressWarnings(check_new_col(panel, "ret_excess"))
    expect_identical(result, panel)
  }
)

test_that("add_lagged_columns warns when bm_lag already exists in data", {
  data_with_lag <- tibble::add_column(panel, bm_lag = NA_real_)
  expect_warning(
    add_lagged_columns(
      data_with_lag,
      cols = "bm",
      lag = months(3),
      by = "permno"
    ),
    "overwrites"
  )
})

test_that(
  paste0(
    "add_lagged_columns warns when .upper ",
    "already exists and max_lag is used"
  ),
  {
    data_with_upper <- tibble::add_column(panel, .upper = NA)
    expect_warning(
      add_lagged_columns(
        data_with_upper,
        cols = "bm",
        lag = months(1),
        max_lag = months(3),
        by = "permno"
      ),
      "overwrites"
    )
  }
)

test_that(
  paste0(
    "compute_long_short_returns warns when ",
    "..date already exists in data"
  ),
  {
    portfolio_returns <- tibble::tibble(
      date = rep(
        seq.Date(as.Date("2020-01-01"), by = "month", length.out = 6),
        each = 5
      ),
      portfolio = rep(1L:5L, 6),
      ret_excess = rnorm(30)
    )
    portfolio_returns[["..date"]] <- NA
    expect_warning(
      compute_long_short_returns(portfolio_returns),
      "overwrites"
    )
  }
)

test_that(
  paste0(
    "filter_sorting_data warns when ",
    ".size_cutoff already exists in data"
  ),
  {
    data_with_cutoff <- tibble::add_column(panel, .size_cutoff = NA_real_)
    expect_warning(
      filter_sorting_data(
        data_with_cutoff,
        filter_options = filter_options(min_size_quantile = 0.2),
        quiet = TRUE
      ),
      "overwrites"
    )
  }
)

test_that(
  paste0(
    "join_lagged_values warns when new_data ",
    "already has .year and ff_adjustment is TRUE"
  ),
  {
    original <- dplyr::select(panel, permno, date, ret_excess, mktcap_lag)
    new <- dplyr::mutate(
      dplyr::select(panel, permno, date, bm),
      .year = NA_real_
    )

    expect_warning(
      join_lagged_values(
        original_data = original,
        new_data = new,
        id_keys = "permno",
        min_lag = months(1),
        max_lag = months(3),
        ff_adjustment = TRUE
      ),
      "overwrites"
    )
  }
)

test_that(
  paste0(
    "compute_portfolio_returns warns ",
    "when portfolio already exists in data"
  ),
  {
    data_with_portfolio <- tibble::add_column(panel, portfolio = NA_integer_)
    expect_warning(
      compute_portfolio_returns(
        data_with_portfolio,
        sorting_variable = "bm",
        sorting_method = "univariate",
        breakpoint_options_main = breakpoint_options(n_portfolios = 3),
        data_options = data_options(id = "permno", date = "date")
      ),
      "overwrites"
    )
  }
)
