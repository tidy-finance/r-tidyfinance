#' Create Filter Options
#'
#' Creates a list of filter options of class `tidyfinance_filter_options` used
#' for sample construction in TidyFinance-related functions. These options
#' control which observations are retained before portfolio sorting.
#'
#' @param exclude_financials A logical indicating whether to exclude financial
#'   firms (SIC codes 6000–6799). Defaults to `FALSE`.
#' @param exclude_utilities A logical indicating whether to exclude utility
#'   firms (SIC codes 4900–4999). Defaults to `FALSE`.
#' @param min_stock_price A single positive numeric specifying the minimum
#'   stock price required to include an observation. `NULL` (the default)
#'   applies no price filter.
#' @param min_size_quantile A single numeric strictly between 0 and 1
#'   specifying the minimum cross-sectional size quantile (based on lagged
#'   market cap) required to include an observation. `NULL` (the default)
#'   applies no size quantile filter. The cutoff is computed from NYSE
#'   stocks only. This requires an `exchange` column in the data (as
#'   mapped via [data_options()]); an error is raised if it is missing.
#' @param min_listing_age A single non-negative integer or numeric specifying
#'   the minimum number of months a stock must have been listed in CRSP.
#'   `NULL` (the default) applies no listing age filter.
#' @param exclude_negative_book_equity A logical indicating whether to exclude
#'   observations with non-positive book equity. Defaults to `FALSE`.
#' @param exclude_negative_earnings A logical indicating whether to exclude
#'   observations with non-positive earnings. Defaults to `FALSE`.
#' @param timing A character string specifying when the screens apply if
#'   portfolios are rebalanced once a year (`rebalancing_month` in
#'   [filter_sorting_data()] or [implement_portfolio_sort()]). `"formation"`
#'   (the default) evaluates the screens in the formation month only and keeps
#'   every stock that passes them for the whole holding period (ex-ante
#'   screens). `"every_period"` evaluates them in every month, so a stock that
#'   fails a screen in a holding month loses that month's return (ex-post
#'   screens). Ex-post screens use information that is not available when the
#'   portfolios are formed, so the resulting factor returns can contain
#'   look-ahead bias; a warning is issued when they are used. With monthly
#'   rebalancing, every month is a formation month and both options are
#'   identical.
#' @param ... Additional arguments to be included in the filter options list.
#'
#' @returns A list of class `tidyfinance_filter_options` containing the
#'   specified filter options.
#'
#' @family portfolio functions
#' @export
#'
#' @examples
#' filter_options(
#'   exclude_financials = TRUE,
#'   exclude_utilities = TRUE,
#'   min_stock_price = 1,
#'   min_listing_age = 12
#' )
#'
#' # Apply the screens in every month (ex-post), not only at formation
#' filter_options(min_stock_price = 5, timing = "every_period")
#'
filter_options <- function(
  exclude_financials = FALSE,
  exclude_utilities = FALSE,
  min_stock_price = NULL,
  min_size_quantile = NULL,
  min_listing_age = NULL,
  exclude_negative_book_equity = FALSE,
  exclude_negative_earnings = FALSE,
  timing = "formation",
  ...
) {
  validate_flag(exclude_financials, "exclude_financials")
  validate_flag(exclude_utilities, "exclude_utilities")
  validate_flag(exclude_negative_book_equity, "exclude_negative_book_equity")
  validate_flag(exclude_negative_earnings, "exclude_negative_earnings")

  if (
    !is.character(timing) ||
      length(timing) != 1L ||
      !timing %in% c("formation", "every_period")
  ) {
    cli::cli_abort(
      "{.arg timing} must be either {.val formation} or {.val every_period}."
    )
  }

  validate_optional_number(
    min_stock_price,
    "{.arg min_stock_price} must be a single positive numeric.",
    min = 0,
    min_strict = TRUE
  )
  validate_optional_number(
    min_size_quantile,
    paste0(
      "{.arg min_size_quantile} must be a single numeric ",
      "strictly between 0 and 1."
    ),
    min = 0,
    max = 1,
    min_strict = TRUE,
    max_strict = TRUE
  )
  validate_optional_number(
    min_listing_age,
    paste0(
      "{.arg min_listing_age} must be a single ",
      "non-negative integer or numeric."
    ),
    min = 0
  )

  warn_near_option_names(
    names(list(...)),
    c(
      "exclude_financials", "exclude_utilities", "min_stock_price",
      "min_size_quantile", "min_listing_age", "exclude_negative_book_equity",
      "exclude_negative_earnings", "timing"
    ),
    "filter_options"
  )

  # Create the list structure with class attribute
  structure(
    list(
      "exclude_financials" = exclude_financials,
      "exclude_utilities" = exclude_utilities,
      "min_stock_price" = min_stock_price,
      "min_size_quantile" = min_size_quantile,
      "min_listing_age" = min_listing_age,
      "exclude_negative_book_equity" = exclude_negative_book_equity,
      "exclude_negative_earnings" = exclude_negative_earnings,
      "timing" = timing,
      ...
    ),
    class = "tidyfinance_filter_options"
  )
}
