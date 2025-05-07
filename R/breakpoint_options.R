#' Create Breakpoint Options for Portfolio Sorting
#'
#' This function generates a structured list of options for defining breakpoints in
#' portfolio sorting. It includes parameters for the number of portfolios, percentile
#' thresholds, exchange-specific breakpoints, and smooth bunching, along with additional
#' optional parameters.
#'
#' @param n_portfolios Integer, optional. The number of portfolios to create. Must be a
#'  positive integer. If not provided, defaults to \code{NULL}.
#' @param percentiles Numeric vector, optional. A vector of percentile thresholds for
#'  defining breakpoints. Each value should be between 0 and 1. If not provided, defaults
#'  to \code{NULL}.
#' @param breakpoint_exchanges Character, optional. A non-empty string specifying the
#'  exchange for which the breakpoints apply. If not provided, defaults to \code{NULL}.
#' @param smooth_bunching Logical, optional. Indicates whether smooth bunching should
#'  be applied. Defaults to \code{FALSE}.
#' @param ... Additional optional arguments. These will be captured in the resulting
#'  structure as a list.
#'
#' @return A list of class \code{"tidyfinance_breakpoint_options"} containing the provided
#' breakpoint options, including any additional arguments passed via \code{...}.
#'
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

  # Create the list structure with class attribute
  structure(
    list(
      "n_portfolios" = n_portfolios,
      "percentiles" = percentiles,
      "breakpoint_exchanges" = breakpoint_exchanges,
      "smooth_bunching" = smooth_bunching,
      ...
    ),
    class = "tidyfinance_breakpoint_options"
  )
}
