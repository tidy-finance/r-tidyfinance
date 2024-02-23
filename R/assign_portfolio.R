#' Assign Portfolios Based on Sorting Variable
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function assigns data points to portfolios based on a specified sorting
#' variable. It can optionally filter the data by exchanges before assignment.
#' The function requires either the number of portfolios to be created or
#' specific percentiles for the breakpoints, but not both.
#'
#' @param data A data frame containing the dataset for portfolio assignment.
#' @param sorting_variable A string specifying the column name in `data` to be
#'   used for sorting and determining portfolio assignments.
#' @param n_portfolios An optional integer specifying the number of equally
#'   sized portfolios to create. This parameter is mutually exclusive with
#'   `percentiles`.
#' @param percentiles An optional numeric vector specifying the percentiles for
#'   determining the breakpoints of the portfolios. This parameter is mutually
#'   exclusive with `n_portfolios`.
#' @param exchanges An optional character vector specifying exchange names to
#'   filter the data before computing breakpoints and assigning portfolios.
#'   Exchanges must be stored in a column named `exchange` in `data`. If `NULL`,
#'   no filtering is applied.
#'
#' @return A vector of portfolio assignments for each row in the input `data`.
#'
#' @examples
#' data <- data.frame(
#'   id = 1:100,
#'   exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
#'   market_cap = runif(100, 1e6, 1e9)
#' )
#' assign_portfolio(data, "market_cap", n_portfolios = 5)
#' assign_portfolio(data, "market_cap", percentiles = c(0.2, 0.4, 0.6, 0.8), exchanges = c("NYSE"))
#'
#' @export
#'
#' @note This function will stop and throw an error if both `n_portfolios` and
#'   `percentiles` are provided or if neither is provided. Ensure that you only
#'   use one of these parameters for specifying portfolio breakpoints.
assign_portfolio <- function(data,
                             sorting_variable,
                             n_portfolios = NULL,
                             percentiles = NULL,
                             exchanges = NULL) {

  if (!is.null(n_portfolios) && !is.null(percentiles)) {
    stop("Please provide either n_portfolios or percentiles, not both.")
  } else if (is.null(n_portfolios) && is.null(percentiles)) {
    stop("You must provide either n_portfolios or percentiles.")
  }

  if (!is.null(exchanges)) {
    data_breakpoints <- data[data$exchange %in% exchanges, ]
  } else {
    data_breakpoints <- data
  }

  if (!is.null(n_portfolios)) {
    probs <- seq(0, 1, length.out = n_portfolios + 1)
  } else {
    probs <- c(0, percentiles, 1)
  }

  sorting_values <- data_breakpoints[[sorting_variable]]
  breakpoints <- quantile(
    sorting_values, probs = probs, na.rm = TRUE, names = FALSE
  )

  sorting_values_all <- data[[sorting_variable]]
  portfolio_indices <- findInterval(
    sorting_values_all, breakpoints, all.inside = TRUE
  )

  return(portfolio_indices)
}
