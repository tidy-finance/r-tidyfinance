#' Create Portfolio Sort Options
#'
#' Creates a list of options of class `tidyfinance_portfolio_sort_options` that
#' bundles sample construction filters and breakpoint specifications for use
#' with [implement_portfolio_sort()].
#'
#' @param filter_options A list of class `tidyfinance_filter_options` created
#'   by [filter_options()], or `NULL` (the default, which applies no filters).
#'   The arguments accepted by [filter_options()] include
#'   \itemize{
#'     \item `exclude_financials` A logical indicating whether to exclude
#'       financial firms (SIC codes 6000–6799). Defaults to `FALSE`.
#'     \item `exclude_utilities` A logical indicating whether to exclude
#'       utility firms (SIC codes 4900–4999). Defaults to `FALSE`.
#'     \item `min_stock_price` A single positive numeric specifying the
#'       minimum stock price required to include an observation. `NULL` (the
#'       default) applies no price filter.
#'     \item `min_size_quantile` A single numeric strictly between 0 and 1
#'       specifying the minimum cross-sectional size quantile (based on lagged
#'       market cap) required to include an observation. `NULL` (the default)
#'       applies no size quantile filter.
#'     \item `min_listing_age` A single non-negative integer or numeric
#'       specifying the minimum number of months a stock must have been listed
#'       in CRSP. `NULL` (the default) applies no listing age filter.
#'     \item `exclude_negative_book_equity` A logical indicating whether to
#'       exclude observations with non-positive book equity. Defaults to
#'       `FALSE`.
#'     \item `exclude_negative_earnings` A logical indicating whether to
#'       exclude observations with non-positive earnings. Defaults to `FALSE`.
#'   }
#' @param breakpoint_options_main A list of class
#'   `tidyfinance_breakpoint_options` created by [breakpoint_options()],
#'   specifying breakpoints for the primary sorting variable. The arguments
#'   accepted by [breakpoint_options()] include
#'   \itemize{
#'     \item `n_portfolios` An optional integer specifying the number of
#'       equally sized portfolios to create. This parameter is mutually
#'       exclusive with `percentiles`.
#'     \item `percentiles` An optional numeric vector specifying the
#'       percentiles for determining the breakpoints of the portfolios.
#'       This parameter is mutually exclusive with `n_portfolios`.
#'     \item `breakpoints_exchanges` An optional character vector specifying
#'       exchange names to filter the data before computing breakpoints.
#'       Exchanges must be stored in a column given by `data_options` (defaults
#'       to `exchange`). If `NULL`, no filtering is applied.
#'     \item `smooth_bunching` An optional logical parameter specifying if
#'       to attempt smoothing non-extreme portfolios if the sorting variable
#'       bunches on the extremes (`TRUE`), or not (`FALSE`, the default).
#'     \item `breakpoints_min_size_threshold` An optional numeric value between
#'       0 and 1 (exclusive). When set, stocks with market capitalization below
#'       this quantile are excluded from breakpoint computation.
#'   }
#' @param breakpoint_options_secondary A list of class
#'   `tidyfinance_breakpoint_options` created by [breakpoint_options()],
#'   specifying breakpoints for the secondary sorting variable, or `NULL`
#'   (the default) for univariate sorts. The arguments accepted by
#'   [breakpoint_options()] are the same as for `breakpoint_options_main`.
#' @param ... Additional arguments to be included in the options list.
#'
#' @returns A list of class `tidyfinance_portfolio_sort_options` containing
#'   the specified options.
#'
#' @family portfolio functions
#' @export
#'
#' @examples
#' portfolio_sort_options(
#'   filter_options = filter_options(exclude_financials = TRUE),
#'   breakpoint_options_main = breakpoint_options(n_portfolios = 10)
#' )
#'
portfolio_sort_options <- function(
  filter_options = NULL,
  breakpoint_options_main,
  breakpoint_options_secondary = NULL,
  ...
) {
  # Error handling for filter_options
  if (
    !is.null(filter_options) &&
      !inherits(filter_options, "tidyfinance_filter_options")
  ) {
    cli::cli_abort(
      paste0(
        "{.arg filter_options} must be {.val NULL} or of class ",
        "{.cls tidyfinance_filter_options}."
      )
    )
  }

  # Error handling for breakpoint_options_main
  if (missing(breakpoint_options_main)) {
    cli::cli_abort("{.arg breakpoint_options_main} must be provided.")
  }

  if (!inherits(breakpoint_options_main, "tidyfinance_breakpoint_options")) {
    cli::cli_abort(
      paste0(
        "{.arg breakpoint_options_main} must be of class ",
        "{.cls tidyfinance_breakpoint_options}."
      )
    )
  }

  # Error handling for breakpoint_options_secondary
  if (
    !is.null(breakpoint_options_secondary) &&
      !inherits(breakpoint_options_secondary, "tidyfinance_breakpoint_options")
  ) {
    cli::cli_abort(
      paste0(
        "{.arg breakpoint_options_secondary} must be {.val NULL} or of class ",
        "{.cls tidyfinance_breakpoint_options}."
      )
    )
  }

  # Create the list structure with class attribute
  structure(
    list(
      "filter_options" = filter_options,
      "breakpoint_options_main" = breakpoint_options_main,
      "breakpoint_options_secondary" = breakpoint_options_secondary,
      ...
    ),
    class = "tidyfinance_portfolio_sort_options"
  )
}
