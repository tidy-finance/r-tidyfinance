#' Implement Portfolio Sort
#'
#' A convenience wrapper that combines sample construction filtering and
#' portfolio return computation into a single call. Equivalent to calling
#' [filter_sorting_data()] followed by [compute_portfolio_returns()].
#'
#' @param data A data frame containing the stock-level panel data.
#' @param sorting_variables A character vector of one or two column names to
#'   sort portfolios on.
#' @param sorting_method A string specifying the sorting method:
#'   `"univariate"`, `"bivariate-dependent"`, or `"bivariate-independent"`.
#' @param rebalancing_month An optional integer specifying the month in which
#'   portfolios are rebalanced annually. `NULL` (the default) means monthly
#'   rebalancing. Also determines the formation month in which the screens of
#'   `filter_options` are applied.
#' @param holding_period,skip Staggered rebalancing: portfolios are formed
#'   every period and each cohort is held for `holding_period` periods,
#'   starting `skip` periods after formation. See
#'   [compute_portfolio_returns()]. With the default
#'   `filter_options(timing = "formation")`, the screens only decide which
#'   stocks enter the cohort formed in a period; a stock that fails a screen
#'   later stays in its earlier cohorts until their holding period ends.
#' @param portfolio_sort_options A list of class
#'   `tidyfinance_portfolio_sort_options` created by
#'   [portfolio_sort_options()], bundling filter and breakpoint
#'   specifications. The arguments accepted by [portfolio_sort_options()]
#'   include
#'   \itemize{
#'     \item `filter_options` A list of class `tidyfinance_filter_options`
#'       created by [filter_options()], or `NULL` (the default, which applies
#'       no filters). Options include `exclude_financials`,
#'       `exclude_utilities`, `min_stock_price`, `min_size_quantile`,
#'       `min_listing_age`, `exclude_negative_book_equity`,
#'       `exclude_negative_earnings`, and `timing`. With `rebalancing_month`,
#'       the screens apply in the formation month only by default; see
#'       [filter_sorting_data()].
#'     \item `breakpoint_options_main` A list of class
#'       `tidyfinance_breakpoint_options` created by [breakpoint_options()],
#'       specifying breakpoints for the primary sorting variable. Options
#'       include `n_portfolios`, `percentiles`, `breakpoints_exchanges`,
#'       `smooth_bunching`, and `breakpoints_min_size_threshold`.
#'     \item `breakpoint_options_secondary` A list of class
#'       `tidyfinance_breakpoint_options` created by [breakpoint_options()],
#'       specifying breakpoints for the secondary sorting variable, or `NULL`
#'       (the default) for univariate sorts. Options are the same as for
#'       `breakpoint_options_main`.
#'   }
#' @param breakpoint_function_main The function used to compute breakpoints for
#'   the main sorting variable. Defaults to [compute_breakpoints()].
#' @param breakpoint_function_secondary The function used to compute breakpoints
#'   for the secondary sorting variable. Defaults to [compute_breakpoints()].
#' @param min_portfolio_size An integer specifying the minimum number of
#'   firms in the reported portfolio cross-section on a given date. Defaults
#'   to `1L` (at least one observation per reported portfolio). For univariate
#'   sorts that is firms per portfolio-date; for bivariate sorts that is firms
#'   per main-portfolio-date summed across the secondary buckets.
#'   Cross-sections below the threshold have their returns set to `NA`.
#'   Set to `0L` to deactivate the check. See [compute_portfolio_returns()].
#' @param cap_weight A numeric between 0 and 1 specifying the quantile at which
#'   portfolio weights are capped for the capped value-weighted return.
#'   Defaults to `0.8`.
#' @param cap_weight_exchanges A character vector of exchanges whose stocks
#'   determine the cap, or `NULL` for all stocks. Defaults to `"NYSE"`. See
#'   [compute_portfolio_returns()].
#' @param data_options A list of class `tidyfinance_data_options` (created via
#'   [data_options()]) specifying column name mappings. All elements are
#'   forwarded to [filter_sorting_data()] and [compute_portfolio_returns()].
#'   Uses [data_options()] default if `NULL`: `"id" = "permno"`,
#'   `"date" = "date"`, `"exchange" = "exchange"`,
#'   `"mktcap_lag" = "mktcap_lag"`, `"ret_excess" = "ret_excess"`,
#'   `"siccd" = "siccd"`, `"price" = "prc_lag"`,
#'   `"listing_age" = "listing_age"`, `"be" = "be"`, and
#'   `"earnings" = "ib"`.
#' @param quiet A logical indicating whether informational messages should be
#'   suppressed. Defaults to `FALSE`.
#'
#' @returns A data frame of portfolio returns as returned by
#'   [compute_portfolio_returns()].
#'
#' @family portfolio functions
#' @export
#'
#' @examples
#'
#' set.seed(123)
#' data <- data.frame(
#'   permno = 1:500,
#'   date = rep(
#'     seq.Date(
#'       from = as.Date("2020-01-01"),
#'       by = "month",
#'       length.out = 100
#'     ),
#'     each = 10
#'   ),
#'   mktcap_lag = runif(500, 100, 1000),
#'   ret_excess = rnorm(500),
#'   prc_lag = runif(500, 0.5, 50),
#'   size = runif(500, 50, 150)
#' )
#' implement_portfolio_sort(
#'   data = data,
#'   sorting_variables = "size",
#'   sorting_method = "univariate",
#'   portfolio_sort_options = portfolio_sort_options(
#'     filter_options = filter_options(
#'       min_stock_price = 1
#'     ),
#'     breakpoint_options_main = breakpoint_options(n_portfolios = 5)
#'   )
#' )
#'
#'
implement_portfolio_sort <- function(
  data,
  sorting_variables,
  sorting_method,
  rebalancing_month = NULL,
  holding_period = 1L,
  skip = 0L,
  portfolio_sort_options,
  breakpoint_function_main = compute_breakpoints,
  breakpoint_function_secondary = compute_breakpoints,
  min_portfolio_size = 1L,
  cap_weight = 0.8,
  cap_weight_exchanges = "NYSE",
  data_options = NULL,
  quiet = FALSE
) {
  if (!is.logical(quiet) || length(quiet) != 1 || is.na(quiet)) {
    cli::cli_abort("{.arg quiet} must be a single logical.")
  }

  if (
    !is.null(data_options) &&
      !inherits(data_options, "tidyfinance_data_options")
  ) {
    cli::cli_abort(
      paste0(
        "{.arg data_options} must be {.val NULL} or of class ",
        "{.cls tidyfinance_data_options}."
      )
    )
  }

  # Validate portfolio_sort_options
  if (!inherits(portfolio_sort_options, "tidyfinance_portfolio_sort_options")) {
    cli::cli_abort(
      paste0(
        "{.arg portfolio_sort_options} must be of class ",
        "{.cls tidyfinance_portfolio_sort_options}."
      )
    )
  }

  # Extract components from portfolio_sort_options
  filter_options <- portfolio_sort_options$filter_options
  breakpoint_options_main <- portfolio_sort_options$breakpoint_options_main
  breakpoint_options_secondary <-
    portfolio_sort_options$breakpoint_options_secondary

  # Apply sample construction filters
  staggered <- !identical(as.numeric(holding_period), 1) ||
    !identical(as.numeric(skip), 0)
  if (staggered && !is.null(filter_options) &&
    has_active_screens(filter_options)) {
    data <- screen_staggered_formation(
      data,
      sorting_variables = sorting_variables,
      filter_options = filter_options,
      data_options = data_options,
      quiet = quiet
    )
  } else {
    data <- filter_sorting_data(
      data,
      filter_options = filter_options,
      data_options = data_options,
      rebalancing_month = rebalancing_month,
      quiet = quiet
    )
  }

  # Compute and return portfolio returns
  compute_portfolio_returns(
    data,
    sorting_variables = sorting_variables,
    sorting_method = sorting_method,
    rebalancing_month = rebalancing_month,
    holding_period = holding_period,
    skip = skip,
    breakpoint_options_main = breakpoint_options_main,
    breakpoint_options_secondary = breakpoint_options_secondary,
    breakpoint_function_main = breakpoint_function_main,
    breakpoint_function_secondary = breakpoint_function_secondary,
    min_portfolio_size = min_portfolio_size,
    cap_weight = cap_weight,
    cap_weight_exchanges = cap_weight_exchanges,
    data_options = data_options,
    quiet = quiet
  )
}

#' Apply screens to the formation of staggered portfolios (internal helper)
#'
#' With staggered rebalancing, every month is a formation month, but stocks
#' are held for several months. With `timing = "formation"`, a stock that
#' fails a screen is not added to the cohort formed in that month, but stays
#' in the cohorts formed earlier: its sorting variables are set to `NA` in
#' that month instead of removing the row. With `timing = "every_period"`,
#' failing rows are removed, so earlier cohorts lose the stock's return, and
#' a warning is issued.
#'
#' @param data,sorting_variables,filter_options,data_options,quiet See
#'   [implement_portfolio_sort()].
#'
#' @returns `data`, screened.
#' @noRd
screen_staggered_formation <- function(
  data,
  sorting_variables,
  filter_options,
  data_options,
  quiet
) {
  if (is.null(data_options)) {
    data_options <- data_options()
  }

  passed <- apply_screens(data, filter_options, data_options, quiet)

  if (identical(filter_options$timing, "every_period")) {
    warn_every_period_screens()
    return(passed)
  }

  id_col <- data_options$id
  date_col <- data_options$date
  require_column(data, id_col, "data_options$id")
  require_column(data, date_col, "data_options$date")

  keep <- paste(data[[id_col]], data[[date_col]]) %in%
    paste(passed[[id_col]], passed[[date_col]])
  for (sorting_variable in sorting_variables) {
    data[[sorting_variable]][!keep] <- NA
  }
  data
}
