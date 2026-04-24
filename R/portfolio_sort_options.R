#' Create Portfolio Sort Options
#'
#' Creates a list of options of class `tidyfinance_portfolio_sort_options` that
#' bundles sample construction filters and breakpoint specifications for use
#' with [implement_portfolio_sort()].
#'
#' @param filter_options A list of class `tidyfinance_filter_options` created
#'   by [filter_options()], or `NULL` (the default, which applies no filters).
#' @param breakpoint_options_main A list of class
#'   `tidyfinance_breakpoint_options` created by [breakpoint_options()],
#'   specifying breakpoints for the primary sorting variable.
#' @param breakpoint_options_secondary A list of class
#'   `tidyfinance_breakpoint_options` created by [breakpoint_options()],
#'   specifying breakpoints for the secondary sorting variable, or `NULL`
#'   (the default) for univariate sorts.
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
