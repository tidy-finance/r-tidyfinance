#' Assign Portfolios Based on Sorting Variable
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function assigns data points to portfolios based on a specified sorting
#' variable and the selected function to compute breakpoints. Users can specify
#' a function to compute breakpoints. The function must take `data` and
#' `sorting_variable` as the first two arguments. Additional arguments can be
#' passed with `...`. The function needs to return an ascending vector of
#' breakpoints. By default, breakpoints are computed with
#' `tidyfinance::compute_breakpoints()`.
#'
#' @param data A data frame containing the dataset for portfolio assignment.
#' @param breakpoint_function A function to compute breakpoints. The default is
#'   set to `tidyfinance::compute_breakpoints()`.
#' @param sorting_variable A string specifying the column name in `data` to be
#'   used for sorting and determining portfolio assignments based on the
#'   breakpoints.
#' @param ... Optional additional arguments passed to `breakpoint_function`.
#'
#' @return A vector of portfolio assignments for each row in the input `data`.
#'
#' @examples
#' data <- data.frame(
#'   id = 1:100,
#'   exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
#'   market_cap = 1:100
#' )
#' assign_portfolio(data, "market_cap", n_portfolios = 5)
#' assign_portfolio(data, "market_cap", percentiles = c(0.2, 0.4, 0.6, 0.8), breakpoint_exchanges = c("NYSE"))
#'
#' @export
assign_portfolio <- function(data,
                             sorting_variable,
                             breakpoint_function = compute_breakpoints(),
                             ...) {
  # Exit condition for identical sorting variables
  if (length(unique(data[[sorting_variable]])) == 1) {
    cli::cli_warn("The sorting variable is constant and only one portfolio is returned.")
    return(rep(1, nrow(data_breakpoints)))
  }

  breakpoints <- breakpoint_function(
    data,
    sorting_variable,
    ...
  )

  # Assign portfolios
  portfolio_indices <- findInterval(
    data[[sorting_variable]], breakpoints, all.inside = TRUE
  )

  if (length(unique(na.omit(portfolio_indices)) )!= n_portfolios) {
    cli::cli_warn("The number of portfolios differs from the specified parameter due to clusters in the sorting variable.")
  }

  return(portfolio_indices)
}
