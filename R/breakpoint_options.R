#' Create Breakpoint Options for Portfolio Sorting
#'
#' Generates a structured list of options for defining breakpoints in
#' portfolio sorting. It includes parameters for the number of portfolios,
#' percentile thresholds, exchange-specific breakpoints, and smooth bunching,
#' along with additional optional parameters.
#'
#' @param n_portfolios Integer, optional. The number of portfolios to create.
#'   Must be a positive integer. If not provided, defaults to `NULL`.
#' @param percentiles Numeric vector, optional. A vector of percentile
#'   thresholds for defining breakpoints. Each value must be between 0 and 1.
#'   If not provided, defaults to `NULL`.
#' @param breakpoint_exchanges Character vector, optional. A non-empty vector
#'   specifying the exchange from which to compute the breakpoints. If not
#'   provided, defaults to `NULL`.
#' @param smooth_bunching Logical, optional. Indicates whether smooth bunching
#'   should be applied. Defaults to `FALSE`.
#' @param min_size_threshold Numeric, optional. When set to a value between 0
#'   and 1, stocks with market capitalization below this quantile are excluded
#'   from breakpoint computation. The quantile is computed among
#'   `breakpoint_exchanges` stocks if specified, otherwise among all stocks.
#'   Requires a market capitalization column in the data (see
#'   [data_options()]). Defaults to `NULL` (no size filtering).
#' @param ... Additional optional arguments. These will be captured in the
#'   resulting structure as a list.
#'
#' @returns A list of class `"tidyfinance_breakpoint_options"` containing the
#'   provided breakpoint options, including any additional arguments passed
#'   via `...`.
#'
#' @family portfolio functions
#' @export
#'
#' @examples
#' breakpoint_options(
#'   n_portfolios = 5,
#'   percentiles = c(0.2, 0.4, 0.6, 0.8),
#'   breakpoint_exchanges = "NYSE",
#'   smooth_bunching = TRUE,
#'   custom_threshold = 0.5,
#'   another_option = "example"
#' )
#'
breakpoint_options <- function(
  n_portfolios = NULL,
  percentiles = NULL,
  breakpoint_exchanges = NULL,
  smooth_bunching = FALSE,
  min_size_threshold = NULL,
  ...
) {
  # Error handling for n_portfolios
  if (
    !is.null(n_portfolios) &&
      (!is.numeric(n_portfolios) || n_portfolios <= 0 || n_portfolios %% 1 != 0)
  ) {
    cli::cli_abort("{.arg n_portfolios} must be a positive integer.")
  }

  # Error handling for percentiles
  if (
    !is.null(percentiles) &&
      (!is.numeric(percentiles) || any(percentiles < 0 | percentiles > 1))
  ) {
    cli::cli_abort(
      "{.arg percentiles} must be a numeric vector with values between 0 and 1."
    )
  }

  # Error handling for breakpoint_exchanges
  if (
    !is.null(breakpoint_exchanges) &&
      (!is.character(breakpoint_exchanges) || length(breakpoint_exchanges) == 0)
  ) {
    cli::cli_abort(
      "{.arg breakpoint_exchanges} must be a non-empty character string."
    )
  }

  # Error handling for smooth_bunching
  if (!is.logical(smooth_bunching) || length(smooth_bunching) != 1) {
    cli::cli_abort(
      "{.arg smooth_bunching} must be a single logical value (TRUE or FALSE)."
    )
  }

  # Error handling for min_size_threshold
  if (
    !is.null(min_size_threshold) &&
      (length(min_size_threshold) != 1L ||
        !is.numeric(min_size_threshold) ||
        min_size_threshold <= 0 ||
        min_size_threshold >= 1)
  ) {
    cli::cli_abort(
      paste(
        "{.arg min_size_threshold} must be NULL or a single numeric value",
        "between 0 and 1 (exclusive)."
      )
    )
  }

  # Create the list structure with class attribute
  structure(
    list(
      "n_portfolios" = n_portfolios,
      "percentiles" = percentiles,
      "breakpoint_exchanges" = breakpoint_exchanges,
      "smooth_bunching" = smooth_bunching,
      "min_size_threshold" = min_size_threshold,
      ...
    ),
    class = "tidyfinance_breakpoint_options"
  )
}
