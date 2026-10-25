#' Compute Portfolio Returns
#'
#' Computes individual portfolio returns based on specified sorting variables
#' and sorting methods. The portfolios can be rebalanced every period or on an
#' annual frequency by specifying a rebalancing month, which is only applicable
#' at a monthly return frequency. The function supports univariate and
#' bivariate sorts, with the latter supporting dependent and independent
#' sorting methods.
#'
#' @details
#' Holding periods and skips are counted on the grid of dates in `data`,
#' which must therefore not have gaps (e.g., one row per stock-month in a
#' monthly panel). With staggered rebalancing (`holding_period > 1` or
#' `skip > 0`), the returned panel starts on the first date on which all
#' `holding_period` cohorts are held.
#'
#' A portfolio's return on a date is `NA` when the portfolio holds fewer than
#' `min_portfolio_size` stocks or when a constituent return is missing, because
#' the equal- and value-weighted averages are taken over all assigned stocks
#' without dropping `NA`s.
#'
#' The function checks for consistency in the provided arguments. For
#' univariate sorts, a single sorting variable and a corresponding number of
#' portfolios must be provided. For bivariate sorts, two sorting variables and
#' two corresponding numbers of portfolios (or percentiles) are required. The
#' sorting method determines how portfolios are assigned and how returns are
#' computed. The function handles missing and extreme values appropriately
#' based on the specified sorting method and rebalancing frequency.
#'
#' @param data A data frame containing the dataset for portfolio assignment
#'   and return computation. The panel data must include individual stock
#'   identifiers and the time point. It must contain columns for the sorting
#'   variables and excess returns. Additionally, lagged market capitalization
#'   is required for value-weighted returns (see parameter `data_options`).
#' @param sorting_variables A character vector specifying the column names in
#'   `data` to be used for sorting and determining portfolio assignments. For
#'   univariate sorts, provide a single variable. For bivariate sorts, provide
#'   two variables, where the first string refers to the main variable and the
#'   second string refers to the secondary ("control") variable.
#' @param sorting_method A string specifying the sorting method to be used.
#'   Possible values are:
#'   \itemize{
#'     \item `"univariate"`: For a single sorting variable.
#'     \item `"bivariate-dependent"`: For two sorting variables, where the
#'       main sort depends on the secondary variable.
#'     \item `"bivariate-independent"`: For two independent sorting variables.
#'   }
#'   For bivariate sorts, the portfolio returns are averaged over the
#'   controlling sorting variable (i.e., the second sorting variable), and only
#'   portfolio returns for the main sorting variable (given as the first
#'   element of `sorting_variables`) are returned.
#' @param rebalancing_month An integer between 1 and 12 specifying the month
#'   in which to form portfolios that are held constant for one year. For
#'   example, setting it to `7` creates portfolios in July that are held
#'   constant until June of the following year. The default `NULL` corresponds
#'   to periodic rebalancing.
#' @param holding_period A positive integer specifying for how many periods
#'   portfolios are held. With the default `1L`, portfolios are rebalanced
#'   every period. With `holding_period = K > 1`, portfolios are formed every
#'   period and each cohort is held for `K` periods, so that `K` overlapping
#'   cohorts are held at any time (staggered rebalancing, as in Jegadeesh and
#'   Titman, 1993). The return of a portfolio in a period is the
#'   equal-weighted average of the returns of the cohorts held in that
#'   period; within a cohort, returns are weighted as in the
#'   `ret_excess_*` columns, using the weights of the holding period. A stock
#'   stays in its cohort for the whole holding period even if its sorting
#'   variables are missing in a later period. Cannot be combined with
#'   `rebalancing_month`.
#' @param skip A non-negative integer specifying the number of periods
#'   between portfolio formation and the first holding period. The default
#'   `0L` holds a cohort from the period in which it is formed, consistent
#'   with sorting variables that are already lagged (i.e., known at the start
#'   of the period). Jegadeesh and Titman (1993), e.g., also report
#'   strategies that skip one period. Cannot be combined with
#'   `rebalancing_month`.
#' @param breakpoint_options_main A named list of [breakpoint_options()]
#'   passed to `breakpoint_function_main` for the main sorting variable.
#' @param breakpoint_options_secondary An optional named list of
#'   [breakpoint_options()] passed to `breakpoint_function_secondary`.
#' @param breakpoint_function_main A function to compute the breakpoints for
#'   the main sorting variable. The default is set to [compute_breakpoints()].
#' @param breakpoint_function_secondary A function to compute the breakpoints
#'   for the secondary sorting variable. The default is set to
#'   [compute_breakpoints()].
#' @param min_portfolio_size An integer specifying the minimum number of
#'   firms required in the reported portfolio cross-section on a given date
#'   (default `1L`, i.e. at least one observation per reported portfolio).
#'   For both univariate and bivariate sorts the threshold is applied to the
#'   firm count per `(portfolio, date)` of the reported cross-section: for
#'   univariate sorts that is firms per portfolio-date; for bivariate sorts
#'   that is firms per main-portfolio-date summed across the secondary
#'   buckets. Cross-sections below the threshold have their returns set to
#'   `NA`. A typical value is `5L` (the Fama-French convention). Set to `0L`
#'   to deactivate the check entirely.
#' @param cap_weight A numeric value between 0 and 1 specifying the percentile
#'   at which market capitalization is capped per date when computing capped
#'   value-weighted excess returns (i.e., `ret_excess_vw_capped`). Defaults to
#'   `0.8`.
#' @param cap_weight_exchanges A character vector of exchanges whose stocks
#'   determine the cap: market capitalization is capped at the `cap_weight`
#'   percentile of the stocks listed on these exchanges on each date. Defaults
#'   to `"NYSE"`, following Jensen, Kelly, and Pedersen (2023), who cap at the
#'   80th percentile of NYSE stocks. `NULL` uses all stocks. If the exchange
#'   column (mapped via [data_options()]) is missing, or no stock is listed on
#'   these exchanges on a date, the percentile of all stocks is used and a
#'   message says so.
#' @param data_options A list of class `tidyfinance_data_options` (created via
#'   [data_options()]) specifying column name mappings. The `id` element is
#'   used to specify the entity (i.e., firm), `date` is used to specify the
#'   date column, `ret_excess` is used to specify the excess return column, and
#'   `mktcap_lag` is used to specify the market capitalization. Uses
#'   [data_options()] default if `NULL`: `"id" = "permno"`,
#'   `"date" = "date"`, `"ret_excess" = "ret_excess"`, and
#'   `"mktcap_lag" = "mktcap_lag"`. The `exchange` element (default
#'   `"exchange"`) is used with `cap_weight_exchanges`.
#' @param quiet A logical value indicating whether to suppress informational
#'   messages about missing values in the output panel (default is `FALSE`).
#'
#' @note Ensure that `data` contains all required columns: the specified
#'   sorting variables and excess returns (see options and defaults set in
#'   `data_options`). The function will stop and throw an error if any required
#'   columns are missing.
#'
#' @returns A data frame with computed portfolio returns as a complete panel
#'   (all portfolio-date combinations), containing the following columns:
#'   \itemize{
#'     \item `portfolio`: The portfolio identifier.
#'     \item `date`: The date of the portfolio return.
#'     \item `ret_excess_vw`: The value-weighted excess return of the
#'       portfolio (only computed if `data` contains a lagged market
#'       capitalization column specified by `data_options()`, which defaults
#'       to `mktcap_lag`). `NA` if insufficient observations for that
#'       portfolio-date.
#'     \item `ret_excess_ew`: The equal-weighted excess return of the
#'       portfolio. `NA` if insufficient observations for that portfolio-date.
#'     \item `ret_excess_vw_capped`: The capped value-weighted excess return
#'       of the portfolio (only computed if `data` contains a lagged market
#'       capitalization column specified by `data_options()`, which defaults
#'       to `mktcap_lag`). Weights are computed using market capitalization
#'       capped at the `cap_weight` percentile per date. `NA` if insufficient
#'       observations for that portfolio-date.
#'   }
#'
#' @references
#'   Jegadeesh, N., & Titman, S. (1993). Returns to buying winners and selling
#'   losers: Implications for stock market efficiency. *Journal of Finance*,
#'   48(1), 65-91. \doi{10.1111/j.1540-6261.1993.tb04702.x}
#'
#'   Jensen, T. I., Kelly, B., & Pedersen, L. H. (2023). Is there a
#'   replication crisis in finance? *Journal of Finance*, 78(5), 2465-2518.
#'   \doi{10.1111/jofi.13249}
#'
#' @family portfolio functions
#' @export
#'
#' @examples
#' set.seed(42)
#' # Univariate sorting with periodic rebalancing
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
#'   size = runif(500, 50, 150)
#' )
#'
#' compute_portfolio_returns(
#'   data, "size", "univariate",
#'   breakpoint_options_main = breakpoint_options(n_portfolios = 5)
#' )
#'
#' # Bivariate dependent sorting with annual rebalancing
#' compute_portfolio_returns(
#'   data, c("size", "mktcap_lag"), "bivariate-dependent", 7,
#'   breakpoint_options_main = breakpoint_options(n_portfolios = 5),
#'   breakpoint_options_secondary = breakpoint_options(n_portfolios = 3),
#' )
#'
compute_portfolio_returns <- function(
  data,
  sorting_variables,
  sorting_method,
  rebalancing_month = NULL,
  holding_period = 1L,
  skip = 0L,
  breakpoint_options_main,
  breakpoint_options_secondary = NULL,
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

  if (is.null(data_options)) {
    data_options <- data_options()
  }

  if (is.null(sorting_variables) || length(sorting_variables) == 0) {
    cli::cli_abort("You must provide at least one sorting variable.")
  }

  if (
    !sorting_method %in%
      c("univariate", "bivariate-dependent", "bivariate-independent")
  ) {
    cli::cli_abort("Invalid sorting method.")
  }

  if (
    (sorting_method %in% c("bivariate-dependent", "bivariate-independent")) &&
      is.null(breakpoint_options_secondary)
  ) {
    cli::cli_warn(
      "No 'breakpoint_options_secondary' specified in bivariate sort."
    )
  }

  if (
    !is.numeric(cap_weight) ||
      length(cap_weight) != 1L ||
      is.na(cap_weight) ||
      cap_weight < 0 ||
      cap_weight > 1
  ) {
    cli::cli_abort("'cap_weight' must be a single numeric value in [0, 1].")
  }

  if (
    !is.null(cap_weight_exchanges) &&
      (!is.character(cap_weight_exchanges) ||
        length(cap_weight_exchanges) == 0L ||
        anyNA(cap_weight_exchanges))
  ) {
    cli::cli_abort(
      "{.arg cap_weight_exchanges} must be {.val NULL} or a character vector."
    )
  }

  if (
    !is.numeric(holding_period) ||
      length(holding_period) != 1L ||
      is.na(holding_period) ||
      holding_period < 1 ||
      holding_period != round(holding_period)
  ) {
    cli::cli_abort("{.arg holding_period} must be a single positive integer.")
  }
  if (
    !is.numeric(skip) ||
      length(skip) != 1L ||
      is.na(skip) ||
      skip < 0 ||
      skip != round(skip)
  ) {
    cli::cli_abort("{.arg skip} must be a single non-negative integer.")
  }
  holding_period <- as.integer(holding_period)
  skip <- as.integer(skip)
  staggered <- holding_period > 1L || skip > 0L
  if (staggered && !is.null(rebalancing_month)) {
    cli::cli_abort(
      paste(
        "{.arg holding_period} and {.arg skip} cannot be combined with",
        "{.arg rebalancing_month}."
      )
    )
  }

  id_col <- data_options$id
  date_col <- data_options$date
  ret_col <- data_options$ret_excess
  w_col <- data_options$mktcap_lag
  w_capped_col <- paste0(w_col, "_capped")

  required_columns <- c(sorting_variables, id_col, date_col, ret_col)

  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Missing columns: {paste(missing_columns, collapse = ', ')}."
    )
  }

  mktcap_lag_missing <- !(w_col %in% colnames(data))
  if (mktcap_lag_missing) {
    data[[w_col]] <- 1L
  }

  # Store the dates before filtering out missing values, sorted so that the
  # returned panel is ordered by date whatever the order of the input
  all_dates <- sort(unique(data[[date_col]]))

  # With staggered rebalancing, held stocks keep their returns even without
  # sorting variables, so the full panel is kept for the holding periods
  if (staggered) {
    holding_data <- data |>
      check_new_col(w_capped_col)
  }

  # Filter out rows with missing values in any sorting variable, as these cannot
  # be assigned to portfolios
  data <- data |>
    tidyr::drop_na(dplyr::all_of(sorting_variables))

  # Compute capped market capitalization per date before replacing NAs,
  # so that the quantile is not distorted by zero-filled missing values
  data <- data |>
    check_new_col(w_capped_col) |>
    cap_market_cap(
      w_col,
      w_capped_col,
      date_col,
      data_options$exchange,
      cap_weight,
      if (mktcap_lag_missing) NULL else cap_weight_exchanges,
      quiet
    )

  if (staggered) {
    # Cap the weights of held stocks with the cap of the formation universe,
    # before its NA market caps are replaced, so that they do not enter the
    # percentile as zeros
    holding_data <- holding_data |>
      cap_market_cap(
        w_col,
        w_capped_col,
        date_col,
        data_options$exchange,
        cap_weight,
        if (mktcap_lag_missing) NULL else cap_weight_exchanges,
        quiet = TRUE,
        cap_data = data
      )
    missing_mcap_holding <- is.na(holding_data[[w_col]])
    holding_data[[w_col]][missing_mcap_holding] <- 0
    holding_data[[w_capped_col]][missing_mcap_holding] <- 0
  }

  # Replace NA market caps with 0, as these observations should not contribute
  # to the value-weighted return
  missing_mcap_data <- is.na(data[[w_col]])
  data[[w_col]][missing_mcap_data] <- 0
  data[[w_capped_col]][missing_mcap_data] <- 0

  # Handle edge case where all observations are filtered out
  if (nrow(data) == 0L) {
    if (!quiet) {
      cli::cli_inform(
        paste0(
          "Returning an empty panel: every row has a missing sorting value, ",
          "so no portfolio could be formed."
        )
      )
    }

    empty_result <- tibble::tibble(
      portfolio = integer(0L),
      !!date_col := as.Date(character(0L)),
      ret_excess_ew = numeric(0L)
    )
    if (!mktcap_lag_missing) {
      empty_result <- empty_result |>
        tibble::add_column(
          ret_excess_vw = numeric(0L),
          .before = "ret_excess_ew"
        ) |>
        tibble::add_column(
          ret_excess_vw_capped = numeric(0L),
          .before = "ret_excess_ew"
        )
    }
    return(empty_result)
  }

  if (
    !is.null(rebalancing_month) &&
      (rebalancing_month > 12L || rebalancing_month < 1L)
  ) {
    cli::cli_abort("Invalid rebalancing_month.")
  }

  if (sorting_method == "univariate" && length(sorting_variables) > 1L) {
    cli::cli_abort("Only provide one sorting variable for univariate sorts.")
  }
  if (sorting_method != "univariate" && length(sorting_variables) != 2L) {
    cli::cli_abort("Provide two sorting variables for bivariate sorts.")
  }

  if (staggered) {
    dates <- all_dates
    portfolio_returns <- compute_staggered_returns(
      formation_data = data,
      holding_data = holding_data,
      sorting_method = sorting_method,
      holding_period = holding_period,
      skip = skip,
      dates = dates,
      id_col = id_col,
      date_col = date_col,
      ret_col = ret_col,
      w_col = w_col,
      w_capped_col = w_capped_col,
      min_portfolio_size = min_portfolio_size,
      sorting_variables = sorting_variables,
      breakpoint_options_main = breakpoint_options_main,
      breakpoint_options_secondary = breakpoint_options_secondary,
      breakpoint_function_main = breakpoint_function_main,
      breakpoint_function_secondary = breakpoint_function_secondary,
      data_options = data_options
    )

    # The panel starts once all holding_period cohorts are held
    first_formation <- match(min(data[[date_col]]), dates)
    first_full <- first_formation + skip + holding_period - 1L
    all_dates <- if (first_full <= length(dates)) {
      dates[seq(first_full, length(dates))]
    } else {
      dates[0]
    }
  }

  if (!staggered && sorting_method == "univariate") {
    if (is.null(rebalancing_month)) {
      portfolio_returns <- data |>
        check_new_col("portfolio") |>
        dplyr::group_by(.data[[date_col]]) |>
        dplyr::mutate(
          portfolio = assign_portfolio(
            data = dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables,
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        )
    } else {
      filtered_data <- data |>
        dplyr::filter(month(.data[[date_col]]) == rebalancing_month)

      if (nrow(filtered_data) == 0) {
        cli::cli_abort(
          paste0(
            "No observations match {.arg rebalancing_month} = ",
            "{rebalancing_month}. Check that the data contains dates in ",
            "the specified rebalancing month."
          )
        )
      }

      portfolio_data <- assign_formation_portfolios(
        filtered_data,
        sorting_variables = sorting_variables,
        sorting_method = sorting_method,
        breakpoint_options_main = breakpoint_options_main,
        breakpoint_options_secondary = breakpoint_options_secondary,
        breakpoint_function_main = breakpoint_function_main,
        breakpoint_function_secondary = breakpoint_function_secondary,
        data_options = data_options,
        id_col = id_col,
        date_col = date_col
      )

      portfolio_returns <- join_rebalanced_portfolios(
        data,
        portfolio_data,
        date_col,
        id_col
      )
    }

    portfolio_returns <- portfolio_returns |>
      dplyr::group_by(.data$portfolio, .data[[date_col]]) |>
      summarise_portfolio_returns(
        ret_col,
        w_col,
        w_capped_col,
        min_portfolio_size
      )
  }

  if (!staggered && sorting_method == "bivariate-dependent") {
    if (is.null(rebalancing_month)) {
      portfolio_returns <- data |>
        check_new_col(
          c("portfolio_secondary", "portfolio_main", ".min_mktcap_main")
        ) |>
        dplyr::group_by(.data[[date_col]]) |>
        dplyr::mutate(
          portfolio_secondary = assign_portfolio(
            dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          ),
          .min_mktcap_main = main_min_mktcap(
            dplyr::pick(dplyr::everything()),
            breakpoint_options_main,
            breakpoint_function_main,
            data_options
          )
        ) |>
        dplyr::group_by(.data[[date_col]], .data$portfolio_secondary) |>
        dplyr::mutate(
          portfolio_main = assign_portfolio(
            dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = with_min_mktcap(
              breakpoint_options_main,
              dplyr::first(.data$.min_mktcap_main)
            ),
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-".min_mktcap_main")
    } else {
      portfolio_data <- assign_formation_portfolios(
        data |>
          dplyr::filter(month(.data[[date_col]]) == rebalancing_month),
        sorting_variables = sorting_variables,
        sorting_method = sorting_method,
        breakpoint_options_main = breakpoint_options_main,
        breakpoint_options_secondary = breakpoint_options_secondary,
        breakpoint_function_main = breakpoint_function_main,
        breakpoint_function_secondary = breakpoint_function_secondary,
        data_options = data_options,
        id_col = id_col,
        date_col = date_col
      )

      portfolio_returns <- join_rebalanced_portfolios(
        data,
        portfolio_data,
        date_col,
        id_col
      )
    }

    portfolio_returns <- aggregate_bivariate_returns(
      portfolio_returns,
      date_col,
      ret_col,
      w_col,
      w_capped_col,
      min_portfolio_size
    )
  }

  if (!staggered && sorting_method == "bivariate-independent") {
    if (is.null(rebalancing_month)) {
      portfolio_returns <- data |>
        check_new_col(c("portfolio_secondary", "portfolio_main")) |>
        dplyr::group_by(.data[[date_col]]) |>
        dplyr::mutate(
          portfolio_secondary = assign_portfolio(
            dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables[2L],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          ),
          portfolio_main = assign_portfolio(
            dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables[1L],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        dplyr::ungroup()
    } else {
      portfolio_data <- assign_formation_portfolios(
        data |>
          dplyr::filter(month(.data[[date_col]]) == rebalancing_month),
        sorting_variables = sorting_variables,
        sorting_method = sorting_method,
        breakpoint_options_main = breakpoint_options_main,
        breakpoint_options_secondary = breakpoint_options_secondary,
        breakpoint_function_main = breakpoint_function_main,
        breakpoint_function_secondary = breakpoint_function_secondary,
        data_options = data_options,
        id_col = id_col,
        date_col = date_col
      )

      portfolio_returns <- join_rebalanced_portfolios(
        data,
        portfolio_data,
        date_col,
        id_col
      )
    }

    portfolio_returns <- aggregate_bivariate_returns(
      portfolio_returns,
      date_col,
      ret_col,
      w_col,
      w_capped_col,
      min_portfolio_size
    )
  }

  portfolio_returns <- portfolio_returns[
    !is.na(portfolio_returns$portfolio) &
      portfolio_returns[[date_col]] %in% all_dates,
  ]

  # When rebalancing annually, dates before the first rebalancing month
  # cannot receive portfolio assignments, so exclude them from the panel
  if (!is.null(rebalancing_month)) {
    matching_dates <- all_dates[month(all_dates) == rebalancing_month]
    if (length(matching_dates) == 0L) {
      cli::cli_abort(
        "No dates in data match for rebalancing month = {rebalancing_month}."
      )
    }
    first_rebalancing_date <- min(matching_dates)
    all_dates <- all_dates[all_dates >= first_rebalancing_date]
  }

  # Complete the panel: ensure all portfolio-date combinations are present
  all_portfolios <- sort(unique(portfolio_returns$portfolio))

  complete_panel <- tidyr::expand_grid(
    portfolio = all_portfolios,
    !!date_col := all_dates
  )

  return_cols <- if (mktcap_lag_missing) {
    "ret_excess_ew"
  } else {
    c("ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped")
  }

  portfolio_returns <- complete_panel |>
    dplyr::left_join(portfolio_returns, by = c("portfolio", date_col)) |>
    dplyr::select(dplyr::all_of(c("portfolio", date_col, return_cols)))

  # Count and report missing values
  n_missing <- sum(is.na(portfolio_returns[["ret_excess_ew"]]))

  if (!quiet && n_missing > 0L) {
    cli::cli_inform(
      paste0(
        "Returning a complete panel with {n_missing} missing portfolio ",
        "return{?s}. A portfolio return is missing on a date when the ",
        "portfolio has no assigned stocks, has fewer stocks than ",
        "{.arg min_portfolio_size} ({min_portfolio_size}), or has a stock ",
        "with a missing return."
      )
    )
  }

  portfolio_returns
}

#' Summarise portfolio returns (internal helper)
#'
#' Computes equal-weighted, value-weighted, and capped value-weighted returns
#' for an already-grouped data frame. Groups with fewer than
#' `min_portfolio_size` observations receive `NA`.
#'
#' @param data A grouped data frame.
#' @param ret_col Column name for excess returns.
#' @param w_col Column name for market capitalization weights.
#' @param w_capped_col Column name for capped market capitalization weights.
#' @param min_portfolio_size Minimum number of stocks per portfolio-date.
#'
#' @returns An ungrouped data frame with columns `ret_excess_vw`,
#'   `ret_excess_ew`, and `ret_excess_vw_capped`.
#'
#' @keywords internal
#' @noRd
summarise_portfolio_returns <- function(
  data,
  ret_col,
  w_col,
  w_capped_col,
  min_portfolio_size
) {
  data |>
    dplyr::summarise(
      ret_excess_vw = dplyr::if_else(
        dplyr::n() < min_portfolio_size | sum(.data[[w_col]]) == 0,
        NA_real_,
        stats::weighted.mean(.data[[ret_col]], .data[[w_col]])
      ),
      ret_excess_ew = dplyr::if_else(
        dplyr::n() < min_portfolio_size,
        NA_real_,
        mean(.data[[ret_col]])
      ),
      ret_excess_vw_capped = dplyr::if_else(
        dplyr::n() < min_portfolio_size | sum(.data[[w_capped_col]]) == 0,
        NA_real_,
        stats::weighted.mean(.data[[ret_col]], .data[[w_capped_col]])
      ),
      .groups = "drop"
    )
}

#' Aggregate bivariate-sort returns across the secondary dimension
#'
#' Computes cell-level returns over `(portfolio_main, portfolio_secondary,
#' date)` without an occupancy threshold, then averages across the secondary
#' buckets to obtain reported `(portfolio_main, date)` returns. The
#' `min_portfolio_size` threshold is applied to the per-`(portfolio_main,
#' date)` firm count (the reported cross-section), not per cell.
#'
#' @param portfolio_returns A panel with columns `portfolio_main`,
#'   `portfolio_secondary`, the date column, and per-stock returns/weights.
#' @param date_col,ret_col,w_col,w_capped_col Column names.
#' @param min_portfolio_size Minimum firms per reported `(portfolio_main,
#'   date)` cross-section. Cross-sections below this size receive `NA`.
#' @param by Additional grouping columns, e.g., the cohort of staggered
#'   portfolios.
#'
#' @returns A data frame with columns `portfolio`, the date column, and the
#'   three return columns.
#'
#' @keywords internal
#' @noRd
aggregate_bivariate_returns <- function(
  portfolio_returns,
  date_col,
  ret_col,
  w_col,
  w_capped_col,
  min_portfolio_size,
  by = character()
) {
  n_per_main_date <- portfolio_returns |>
    dplyr::filter(
      !is.na(.data$portfolio_main),
      !is.na(.data$portfolio_secondary)
    ) |>
    dplyr::count(
      .data$portfolio_main,
      .data[[date_col]],
      dplyr::across(dplyr::all_of(by)),
      name = "n_firms"
    )

  portfolio_returns |>
    dplyr::group_by(
      .data$portfolio_main,
      .data$portfolio_secondary,
      .data[[date_col]],
      dplyr::across(dplyr::all_of(by))
    ) |>
    summarise_portfolio_returns(
      ret_col,
      w_col,
      w_capped_col,
      min_portfolio_size = 0L
    ) |>
    dplyr::group_by(
      portfolio = .data$portfolio_main,
      .data[[date_col]],
      dplyr::across(dplyr::all_of(by))
    ) |>
    dplyr::summarise(
      dplyr::across(
        c("ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped"),
        \(x) mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      n_per_main_date,
      by = stats::setNames(
        c("portfolio_main", date_col, by),
        c("portfolio", date_col, by)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        c("ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped"),
        \(x) {
          dplyr::if_else(
            is.nan(x) |
              is.na(.data$n_firms) |
              .data$n_firms < min_portfolio_size,
            NA_real_,
            x
          )
        }
      )
    ) |>
    dplyr::select(-"n_firms")
}

#' Join rebalanced portfolio assignments to sorting data (internal helper)
#'
#' Performs an inequality join to carry forward annual portfolio assignments to
#' all dates within the 12-month rebalancing window.
#'
#' @param data The full panel of stock-level data.
#' @param portfolio_data Portfolio assignments at rebalancing dates.
#' @param date_col Name of the date column.
#' @param id_col Name of the stock identifier column.
#'
#' @returns A data frame with portfolio assignments joined to all dates.
#'
#' @keywords internal
#' @noRd
join_rebalanced_portfolios <- function(
  data,
  portfolio_data,
  date_col,
  id_col
) {
  # To avoid undefined global variable notes in checks, the columns and the
  # closest() helper used in the join_by() below are defined explicitly here.
  ..id <- ..date <- NULL # nolint: object_name_linter.
  lower_bound <- upper_bound <- closest <- NULL

  data |>
    dplyr::rename(
      "..date" = dplyr::all_of(date_col),
      "..id" = dplyr::all_of(id_col)
    ) |>
    dplyr::left_join(
      portfolio_data |>
        dplyr::rename("..id" = dplyr::all_of(id_col)) |>
        dplyr::mutate(
          lower_bound = .data[[date_col]],
          upper_bound = .data[[date_col]] + months(12)
        ) |>
        dplyr::select(-dplyr::all_of(date_col)),
      dplyr::join_by(
        ..id,
        closest(..date >= lower_bound),
        ..date <= upper_bound
      ),
      relationship = "many-to-one"
    ) |>
    dplyr::rename("{date_col}" := "..date", "{id_col}" := "..id")
}

#' Cap market capitalization per date (internal helper)
#'
#' Caps `w_col` at the `cap_weight` percentile of the stocks listed on
#' `cap_weight_exchanges` on each date, or of all stocks if
#' `cap_weight_exchanges` is `NULL`, the exchange column is missing, or no
#' stock is listed on these exchanges on a date.
#'
#' @param data A data frame.
#' @param w_col,w_capped_col,date_col,exchange_col Column names.
#' @param cap_weight The percentile at which to cap.
#' @param cap_weight_exchanges `NULL` or a character vector of exchanges.
#' @param quiet A logical; if `TRUE`, no message is emitted.
#' @param cap_data The panel whose cross-section determines the cap on each
#'   date. Defaults to `data`. Dates of `data` without rows in `cap_data` are
#'   capped from `data` itself.
#'
#' @returns `data` with the capped column `w_capped_col`.
#'
#' @keywords internal
#' @noRd
cap_market_cap <- function(
  data,
  w_col,
  w_capped_col,
  date_col,
  exchange_col,
  cap_weight,
  cap_weight_exchanges,
  quiet,
  cap_data = data
) {
  if (
    !is.null(cap_weight_exchanges) &&
      !exchange_col %in% colnames(cap_data)
  ) {
    if (!quiet) {
      cli::cli_inform(
        c(
          paste(
            "Column {.val {exchange_col}} not found: capping market",
            "capitalization at the percentile of all stocks."
          ),
          "i" = paste(
            "Add the exchange column or set",
            "{.code cap_weight_exchanges = NULL} to silence this message."
          )
        ),
        .frequency = "regularly",
        .frequency_id = "tidyfinance_cap_weight_exchanges"
      )
    }
    cap_weight_exchanges <- NULL
  }

  compute_caps <- function(cap_data) {
    if (
      is.null(cap_weight_exchanges) ||
        !exchange_col %in% colnames(cap_data)
    ) {
      reference <- rep(TRUE, nrow(cap_data))
    } else {
      reference <- cap_data[[exchange_col]] %in% cap_weight_exchanges
    }

    tibble::tibble(
      .date = cap_data[[date_col]],
      .w = cap_data[[w_col]],
      .reference = reference
    ) |>
      dplyr::group_by(.data$.date) |>
      dplyr::summarise(
        .has_reference = any(.data$.reference & !is.na(.data$.w)),
        .cap = stats::quantile(
          if (any(.data$.reference & !is.na(.data$.w))) {
            .data$.w[.data$.reference]
          } else {
            .data$.w
          },
          cap_weight,
          na.rm = TRUE,
          names = FALSE
        ),
        .groups = "drop"
      )
  }

  caps <- compute_caps(cap_data)

  n_fallback <- sum(!caps$.has_reference)
  if (!quiet && !is.null(cap_weight_exchanges) && n_fallback > 0L) {
    cli::cli_inform(
      paste(
        "No {.val {cap_weight_exchanges}} stocks with market capitalization",
        "on {n_fallback} date{?s}: capping at the percentile of all stocks",
        "on {cli::qty(n_fallback)}{?that date/these dates}."
      )
    )
  }

  # Dates of data without rows in cap_data are capped from data itself
  missing_dates <- !data[[date_col]] %in% caps$.date
  if (any(missing_dates)) {
    caps <- dplyr::bind_rows(caps, compute_caps(data[missing_dates, ]))
  }

  cap <- caps$.cap[match(data[[date_col]], caps$.date)]
  data[[w_capped_col]] <- pmin(data[[w_col]], cap)
  data
}

#' Minimum market cap of the main breakpoints in dependent sorts (internal
#' helper)
#'
#' In dependent bivariate sorts, the main breakpoints are computed within each
#' secondary portfolio. The minimum market cap of
#' `breakpoints_min_size_threshold`
#' must nevertheless come from the full cross-section of a date, so it is
#' computed here, before grouping by the secondary portfolio. Only the default
#' [compute_breakpoints()] uses it; custom breakpoint functions receive their
#' options unchanged.
#'
#' @param data The cross-section of one date.
#' @param breakpoint_options,breakpoint_function,data_options See
#'   [assign_portfolio()].
#'
#' @returns A single numeric minimum market capitalization, or `NA` if none
#'   applies.
#'
#' @keywords internal
#' @noRd
main_min_mktcap <- function(
  data,
  breakpoint_options,
  breakpoint_function,
  data_options
) {
  threshold <- breakpoint_options$breakpoints_min_size_threshold
  if (
    is.null(threshold) ||
      !identical(breakpoint_function, compute_breakpoints)
  ) {
    return(NA_real_)
  }
  compute_min_mktcap(
    data,
    threshold,
    breakpoint_options$breakpoints_exchanges,
    data_options
  )
}

#' Add an absolute minimum market cap to breakpoint options (internal helper)
#'
#' @param breakpoint_options A list of class `tidyfinance_breakpoint_options`.
#' @param min_mktcap A single numeric minimum market capitalization, or `NA`
#'   to leave the options
#'   unchanged.
#'
#' @returns The breakpoint options.
#'
#' @keywords internal
#' @noRd
with_min_mktcap <- function(breakpoint_options, min_mktcap) {
  if (!is.na(min_mktcap)) {
    breakpoint_options$breakpoints_min_mktcap <- min_mktcap
  }
  breakpoint_options
}
