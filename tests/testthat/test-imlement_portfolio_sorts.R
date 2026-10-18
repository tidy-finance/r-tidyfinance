valid_pso <- function() {
  structure(
    list(
      filter_options = NULL,
      breakpoint_options_main = NULL,
      breakpoint_options_secondary = NULL
    ),
    class = "tidyfinance_portfolio_sort_options"
  )
}

test_that("errors when quiet is not logical", {
  expect_error(
    implement_portfolio_sort(
      data = data.frame(),
      sorting_variables = "x",
      sorting_method = "univariate",
      portfolio_sort_options = valid_pso(),
      quiet = "yes"
    ),
    regexp = "quiet"
  )
})

test_that("errors when quiet has length greater than 1", {
  expect_error(
    implement_portfolio_sort(
      data = data.frame(),
      sorting_variables = "x",
      sorting_method = "univariate",
      portfolio_sort_options = valid_pso(),
      quiet = c(TRUE, FALSE)
    ),
    regexp = "quiet"
  )
})

test_that("errors when quiet is NA", {
  expect_error(
    implement_portfolio_sort(
      data = data.frame(),
      sorting_variables = "x",
      sorting_method = "univariate",
      portfolio_sort_options = valid_pso(),
      quiet = NA
    ),
    regexp = "quiet"
  )
})

test_that("errors when data_options is non-NULL with wrong class", {
  expect_error(
    implement_portfolio_sort(
      data = data.frame(),
      sorting_variables = "x",
      sorting_method = "univariate",
      portfolio_sort_options = valid_pso(),
      data_options = list(id = "permno"),
      quiet = TRUE
    ),
    regexp = "data_options"
  )
})

test_that("errors when portfolio_sort_options has wrong class", {
  expect_error(
    implement_portfolio_sort(
      data = data.frame(),
      sorting_variables = "x",
      sorting_method = "univariate",
      portfolio_sort_options = list(),
      quiet = TRUE
    ),
    regexp = "portfolio_sort_options"
  )
})

test_that("delegates to filter and compute on valid inputs", {
  dummy_input <- data.frame(x = 1)
  dummy_output <- data.frame(portfolio = 1L, ret = 0.1)

  local_mocked_bindings(
    filter_sorting_data = function(...) dummy_input,
    compute_portfolio_returns = function(...) dummy_output,
    .package = "tidyfinance"
  )

  result <- implement_portfolio_sort(
    data = dummy_input,
    sorting_variables = "x",
    sorting_method = "univariate",
    portfolio_sort_options = valid_pso(),
    quiet = TRUE
  )

  expect_identical(result, dummy_output)
})

test_that("implement_portfolio_sort screens at formation when annual", {
  set.seed(1)
  dates <- seq(as.Date("2020-07-01"), as.Date("2021-06-01"), by = "month")
  data <- tidyr::expand_grid(permno = 1:20, date = dates) |>
    dplyr::mutate(
      ret_excess = 0.01,
      mktcap_lag = 100,
      size = rep(1:20, each = length(dates)),
      prc_lag = 10
    )
  # Stock 1 falls below the price screen after formation and loses 50% in
  # August 2020.
  is_stock1_aug <- data$permno == 1 & data$date == as.Date("2020-08-01")
  data$prc_lag[data$permno == 1 & data$date > as.Date("2020-07-01")] <- 1
  data$ret_excess[is_stock1_aug] <- -0.5

  options <- portfolio_sort_options(
    filter_options = filter_options(min_stock_price = 5),
    breakpoint_options_main = breakpoint_options(n_portfolios = 2)
  )

  formation <- implement_portfolio_sort(
    data,
    sorting_variables = "size",
    sorting_method = "univariate",
    rebalancing_month = 7,
    portfolio_sort_options = options,
    quiet = TRUE
  )
  aug_formation <- formation$ret_excess_ew[
    formation$portfolio == 1 & formation$date == as.Date("2020-08-01")
  ]
  expect_equal(aug_formation, (9 * 0.01 - 0.5) / 10)

  options_ex_post <- portfolio_sort_options(
    filter_options = filter_options(
      min_stock_price = 5,
      timing = "every_period"
    ),
    breakpoint_options_main = breakpoint_options(n_portfolios = 2)
  )
  rlang::local_options(rlib_warning_verbosity = "verbose")
  expect_warning(
    ex_post <- implement_portfolio_sort(
      data,
      sorting_variables = "size",
      sorting_method = "univariate",
      rebalancing_month = 7,
      portfolio_sort_options = options_ex_post,
      quiet = TRUE
    ),
    "look-ahead"
  )
  aug_ex_post <- ex_post$ret_excess_ew[
    ex_post$portfolio == 1 & ex_post$date == as.Date("2020-08-01")
  ]
  expect_equal(aug_ex_post, 0.01)
})
