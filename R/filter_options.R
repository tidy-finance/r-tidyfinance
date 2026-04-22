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
#'   applies no size quantile filter.
#' @param min_listing_age A single non-negative integer or numeric specifying
#'   the minimum number of months a stock must have been listed in CRSP.
#'   `NULL` (the default) applies no listing age filter.
#' @param positive_book_equity A logical indicating whether to retain only
#'   observations with strictly positive book equity. Defaults to `FALSE`.
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
filter_options <- function(
  exclude_financials = FALSE,
  exclude_utilities = FALSE,
  min_stock_price = NULL,
  min_size_quantile = NULL,
  min_listing_age = NULL,
  positive_book_equity = FALSE,
  ...
) {
  # Error handling for exclude_financials
  if (
    !is.logical(exclude_financials) || 
      length(exclude_financials) != 1 || 
      is.na(exclude_financials)) {
    cli::cli_abort("{.arg exclude_financials} must be a single logical.")
  }

  # Error handling for exclude_utilities
  if (!is.logical(exclude_utilities) || 
    length(exclude_utilities) != 1 || 
    is.na(exclude_utilities)) {
    cli::cli_abort("{.arg exclude_utilities} must be a single logical.")
  }

  # Error handling for min_stock_price
  if (!is.null(min_stock_price)) {
    if (
      !is.numeric(min_stock_price) ||
        length(min_stock_price) != 1 ||
        is.na(min_stock_price) ||
        min_stock_price <= 0
    ) {
      cli::cli_abort(
        "{.arg min_stock_price} must be a single positive numeric."
      )
    }
  }

  # Error handling for min_size_quantile
  if (!is.null(min_size_quantile)) {
    if (
      !is.numeric(min_size_quantile) ||
        length(min_size_quantile) != 1 ||
        is.na(min_size_quantile) ||
        min_size_quantile <= 0 ||
        min_size_quantile >= 1
    ) {
      cli::cli_abort(
        "{.arg min_size_quantile} must be a single numeric strictly between 0 and 1."
      )
    }
  }

  # Error handling for min_listing_age
  if (!is.null(min_listing_age)) {
    if (
      !is.numeric(min_listing_age) ||
        length(min_listing_age) != 1 ||
        is.na(min_listing_age) ||
        min_listing_age < 0
    ) {
      cli::cli_abort(
        "{.arg min_listing_age} must be a single non-negative integer or numeric."
      )
    }
  }

  # Error handling for positive_book_equity
  if (!is.logical(positive_book_equity) || 
    length(positive_book_equity) != 1 || 
    is.na(positive_book_equity)) {
    cli::cli_abort("{.arg positive_book_equity} must be a single logical.")
  }

  # Create the list structure with class attribute
  structure(
    list(
      "exclude_financials" = exclude_financials,
      "exclude_utilities" = exclude_utilities,
      "min_stock_price" = min_stock_price,
      "min_size_quantile" = min_size_quantile,
      "min_listing_age" = min_listing_age,
      "positive_book_equity" = positive_book_equity,
      ...
    ),
    class = "tidyfinance_filter_options"
  )
}
