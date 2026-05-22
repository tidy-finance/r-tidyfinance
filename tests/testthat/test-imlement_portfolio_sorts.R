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
