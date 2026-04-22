test_that(
  "portfolio_sort_options returns object of class tidyfinance_portfolio_sort_options",
  {
  opts <- portfolio_sort_options(
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_s3_class(opts, "tidyfinance_portfolio_sort_options")
})

test_that(
  "portfolio_sort_options returns a list",
  {
  opts <- portfolio_sort_options(
    breakpoint_options_main = breakpoint_options(n_portfolios = 5)
  )
  expect_type(opts, "list")
})

test_that(
  "portfolio_sort_options stores all provided components", 
  {
  fo <- filter_options(exclude_financials = TRUE)
  bpm <- breakpoint_options(n_portfolios = 10)
  bps <- breakpoint_options(percentiles = c(0.3, 0.7))

  opts <- portfolio_sort_options(
    filter_options = fo,
    breakpoint_options_main = bpm,
    breakpoint_options_secondary = bps
  )

  expect_identical(opts$filter_options, fo)
  expect_identical(opts$breakpoint_options_main, bpm)
  expect_identical(opts$breakpoint_options_secondary, bps)
})

test_that(
  "portfolio_sort_options accepts NULL filter_options", 
  {
  expect_no_error(
    portfolio_sort_options(
      filter_options = NULL,
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    )
  )
})

test_that(
  "portfolio_sort_options accepts NULL breakpoint_options_secondary", 
  {
  opts <- portfolio_sort_options(
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    breakpoint_options_secondary = NULL
  )
  expect_null(opts$breakpoint_options_secondary)
})

test_that("portfolio_sort_options stores extra arguments via ...", {
  opts <- portfolio_sort_options(
    breakpoint_options_main = breakpoint_options(n_portfolios = 5),
    custom_option = "value"
  )
  expect_equal(opts$custom_option, "value")
})

test_that(
  "portfolio_sort_options errors when filter_options has wrong class", 
  {
  expect_error(
    portfolio_sort_options(
      filter_options = list(exclude_financials = TRUE),
      breakpoint_options_main = breakpoint_options(n_portfolios = 5)
    ),
    "tidyfinance_filter_options"
  )
})

test_that(
  "portfolio_sort_options errors when breakpoint_options_main has wrong class", 
  {
  expect_error(
    portfolio_sort_options(
      breakpoint_options_main = list(n_portfolios = 5)
    ),
    "tidyfinance_breakpoint_options"
  )
})

test_that(
  "portfolio_sort_options errors when breakpoint_options_main is missing", 
  {
  expect_error(
    portfolio_sort_options(),
    "breakpoint_options_main"
  )
})

test_that(
  "portfolio_sort_options errors when breakpoint_options_secondary has wrong class", 
  {
  expect_error(
    portfolio_sort_options(
      breakpoint_options_main = breakpoint_options(n_portfolios = 5),
      breakpoint_options_secondary = list(n_portfolios = 3)
    ),
    "tidyfinance_breakpoint_options"
  )
})
