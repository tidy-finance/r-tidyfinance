# Test assign_portfolio() function ------------------------------------
data <- data.frame(
  id = 1:100,
  exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
  market_cap = runif(100, 1e6, 1e9)
)

test_that("assign_portfolio assigns portfolios correctly with n_portfolios", {
  n_portfolios <- 5
  sorting_variable <- "market_cap"
  assigned_portfolios <- assign_portfolio(data, sorting_variable, n_portfolios = n_portfolios)

  expect_length(unique(assigned_portfolios), n_portfolios)
})

test_that("assign_portfolio assigns portfolios correctly with percentiles", {
  percentiles <- c(0.25, 0.75)
  sorting_variable <- "market_cap"
  assigned_portfolios <- assign_portfolio(data, sorting_variable, percentiles = percentiles)

  expect_length(unique(assigned_portfolios), length(percentiles) + 1)
})

test_that("assign_portfolio correctly calculates breakpoints based on filtered exchanges but assigns to all data", {
  n_portfolios <- 5
  sorting_variable <- "market_cap"
  exchanges <- c("NYSE")

  assigned_portfolios_with_exchanges <- assign_portfolio(data, sorting_variable, n_portfolios = n_portfolios, exchanges = exchanges)
  assigned_portfolios_without_exchanges <- assign_portfolio(data, sorting_variable, n_portfolios = n_portfolios)

  expect_length(unique(assigned_portfolios_with_exchanges), n_portfolios)
  expect_length(unique(assigned_portfolios_without_exchanges), n_portfolios)

})

test_that("assign_portfolio throws an error if both n_portfolios and percentiles are provided", {
  sorting_variable <- "market_cap"
  expect_error(assign_portfolio(data, sorting_variable, n_portfolios = 5, percentiles = c(0.25, 0.75)),
               "Please provide either n_portfolios or percentiles, not both.")
})

test_that("assign_portfolio throws an error if neither n_portfolios nor percentiles are provided", {
  sorting_variable <- "market_cap"
  expect_error(assign_portfolio(data, sorting_variable),
               "You must provide either n_portfolios or percentiles.")
})
