#' Assign Portfolios Based on Sorting Variable
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Assigns data points to portfolios based on a specified sorting variable and
#' the selected function to compute breakpoints. Users can specify a function
#' to compute breakpoints. The function must take `data` and
#' `sorting_variable` as the first two arguments. Additional arguments are
#' passed with a named list [breakpoint_options()]. The function needs to
#' return an ascending vector of breakpoints. By default, breakpoints are
#' computed with [compute_breakpoints()]. The default column names can be
#' modified using [data_options()].
#'
#' @param data A data frame containing the dataset for portfolio assignment.
#' @param sorting_variable A string specifying the column name in `data` to be
#'   used for sorting and determining portfolio assignments based on the
#'   breakpoints.
#' @param breakpoint_options An optional named list of arguments passed to
#'   `breakpoint_function`.
#' @param breakpoint_function A function to compute breakpoints. The default
#'   is set to [compute_breakpoints()].
#' @param data_options A list of class `tidyfinance_data_options` (created via
#'   [data_options()]) specifying column name mappings. Passed through to
#'   `breakpoint_function`. When using the default [compute_breakpoints()],
#'   the `exchange` element is used to specify the exchange column, and
#'   `mktcap_lag` is used to specify the market capitalization column. Uses
#'   [data_options()] default if `NULL`: `"exchange" = "exchange"` and
#'   `"mktcap_lag" = "mktcap_lag"`.
#'
#' @returns A vector of integer portfolio assignments for each row in the
#'   input `data`.
#'
#' @family portfolio functions
#' @export
#'
#' @examples
#' set.seed(42)
#' data <- data.frame(
#'   id = 1:100,
#'   exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
#'   market_cap = 1:100
#' )
#'
#' assign_portfolio(data, "market_cap", breakpoint_options(n_portfolios = 5))
#'
#' assign_portfolio(
#'   data,
#'   "market_cap",
#'   breakpoint_options(
#'     percentiles = c(0.2, 0.4, 0.6, 0.8),
#'     breakpoints_exchanges = c("NYSE")
#'   )
#' )
#'
assign_portfolio <- function(
  data,
  sorting_variable,
  breakpoint_options = NULL,
  breakpoint_function = compute_breakpoints,
  data_options = NULL
) {
  x <- data[[sorting_variable]]
  n <- length(x)

  ux <- unique(x[!is.na(x)])
  if (length(ux) <= 1L) {
    cli::cli_warn(
      "The sorting variable is constant and only one portfolio is returned."
    )
    return(rep.int(1L, n))
  }

  breakpoints <- breakpoint_function(
    data,
    sorting_variable,
    breakpoint_options,
    data_options
  )

  if (anyNA(breakpoints)) {
    cli::cli_warn(
      "No portfolios were assigned due to missing breakpoints."
    )
    return(NA_integer_)
  }

  portfolio_indices <- findInterval(x, breakpoints, all.inside = TRUE)

  n_expected <- length(breakpoints) - 1L
  n_actual <- length(unique(na.omit(portfolio_indices)))
  if (n_actual != n_expected) {
    cli::cli_warn(
      paste(
        "The number of portfolios differs from the",
        "specified parameter due to clusters in the sorting variable."
      )
    )
  }

  portfolio_indices
}
