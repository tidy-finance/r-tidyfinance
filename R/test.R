# benchmark-compute_portfolio_returns.R
#
# Contains original + optimized implementations, correctness checks, and
# benchmarks across data sizes and all sorting methods.
#
# Run with:
#   Rscript benchmark-compute_portfolio_returns.R
#
# Requirements:
#   install.packages(c("bench", "cli", "dplyr", "lubridate"))

library(bench)
library(cli)
library(dplyr)
library(lubridate)

# =============================================================================
# Helpers: breakpoint_options, data_options, compute_breakpoints,
#          assign_portfolio (shared by both versions)
# =============================================================================

# =============================================================================
# Test data generator
# =============================================================================
make_panel <- function(n_stocks = 50, n_months = 24, seed = 42) {
  set.seed(seed)
  dates <- seq.Date(
    from = as.Date("2020-01-01"),
    by = "month",
    length.out = n_months
  )
  expand.grid(permno = seq_len(n_stocks), date = dates) |>
    mutate(
      mktcap_lag = runif(n(), 100, 10000),
      ret_excess = rnorm(n(), 0, 0.05),
      size = runif(n(), 1, 100),
      bm = runif(n(), 0.5, 3),
      exchange = sample(c("NYSE", "NASDAQ"), n(), replace = TRUE),
      # Extra columns that make pick(everything()) expensive
      extra1 = rnorm(n()),
      extra2 = rnorm(n()),
      extra3 = rnorm(n()),
      extra4 = rnorm(n()),
      extra5 = rnorm(n()),
      extra6 = rnorm(n())
    ) |>
    arrange(date, permno) |>
    as.data.frame()
}

# =============================================================================
# ORIGINAL IMPLEMENTATION (verbatim from source, using pick(everything()))
# =============================================================================
compute_portfolio_returns_original <- function(
  sorting_data,
  sorting_variables,
  sorting_method,
  rebalancing_month = NULL,
  breakpoint_options_main,
  breakpoint_options_secondary = NULL,
  breakpoint_function_main = compute_breakpoints,
  breakpoint_function_secondary = compute_breakpoints,
  min_portfolio_size = 0,
  data_options = NULL
) {
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

  required_columns <- c(
    sorting_variables,
    data_options$date,
    data_options$id,
    data_options$ret_excess
  )
  missing_columns <- setdiff(required_columns, colnames(sorting_data))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "Missing columns: {paste(missing_columns, collapse = ', ')}."
    )
  }

  mktcap_lag_missing <- !(data_options$mktcap_lag %in% colnames(sorting_data))
  if (mktcap_lag_missing) {
    sorting_data$mktcap_lag <- 1
  }

  if (
    !is.null(rebalancing_month) &&
      (rebalancing_month > 12 || rebalancing_month < 1)
  ) {
    cli::cli_abort("Invalid rebalancing_month.")
  }

  if (sorting_method == "univariate") {
    if (length(sorting_variables) > 1) {
      cli::cli_abort("Only provide one sorting variable for univariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio = assign_portfolio(
            data = pick(everything()),
            sorting_variable = sorting_variables,
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        )
    } else {
      portfolio_data <- sorting_data |>
        filter(month(.data[[data_options$date]]) == rebalancing_month) |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio = assign_portfolio(
            data = pick(everything()),
            sorting_variable = sorting_variables,
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        select(all_of(c(data_options$id, data_options$date, "portfolio")))

      portfolio_returns <- sorting_data |>
        rename(
          "..date" = all_of(data_options$date),
          "..id" := all_of(data_options$id)
        ) |>
        left_join(
          portfolio_data |>
            rename("..id" = all_of(data_options$id)) |>
            mutate(
              lower_bound = .data[[data_options$date]],
              upper_bound = .data[[data_options$date]] + months(12)
            ) |>
            select(-all_of(data_options$date)),
          join_by(..id, closest(..date >= lower_bound), ..date < upper_bound),
          relationship = "many-to-one"
        ) |>
        rename("{data_options$date}" := "..date", "{data_options$id}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      group_by(portfolio, .data[[data_options$date]]) |>
      summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(
            .data[[data_options$ret_excess]],
            .data[[data_options$mktcap_lag]]
          )
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(.data[[data_options$ret_excess]])
        ),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-dependent") {
    if (length(sorting_variables) != 2) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio_secondary = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        group_by(.data[[data_options$date]], portfolio_secondary) |>
        mutate(
          portfolio_main = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup()
    } else {
      portfolio_data <- sorting_data |>
        filter(month(.data[[data_options$date]]) == rebalancing_month) |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio_secondary = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        group_by(.data[[data_options$date]], portfolio_secondary) |>
        mutate(
          portfolio_main = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        select(all_of(c(
          data_options$id,
          data_options$date,
          "portfolio_main",
          "portfolio_secondary"
        )))

      portfolio_returns <- sorting_data |>
        rename(
          "..date" = all_of(data_options$date),
          "..id" := all_of(data_options$id)
        ) |>
        left_join(
          portfolio_data |>
            rename("..id" = all_of(data_options$id)) |>
            mutate(lower_bound = date, upper_bound = date + months(12)) |>
            select(-all_of(data_options$date)),
          join_by(..id, closest(..date >= lower_bound), ..date < upper_bound),
          relationship = "many-to-one"
        ) |>
        rename("{data_options$date}" := "..date", "{data_options$id}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      group_by(
        portfolio_main,
        portfolio_secondary,
        .data[[data_options$date]]
      ) |>
      summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(ret_excess, mktcap_lag)
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(ret_excess)
        ),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, .data[[data_options$date]]) |>
      summarize(
        across(c(ret_excess_vw, ret_excess_ew), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-independent") {
    if (length(sorting_variables) != 2) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio_secondary = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          ),
          portfolio_main = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup()
    } else {
      portfolio_data <- sorting_data |>
        filter(month(.data[[data_options$date]]) == rebalancing_month) |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio_secondary = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          ),
          portfolio_main = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        select(all_of(c(
          data_options$id,
          data_options$date,
          "portfolio_main",
          "portfolio_secondary"
        )))

      portfolio_returns <- sorting_data |>
        rename(
          "..date" = all_of(data_options$date),
          "..id" := all_of(data_options$id)
        ) |>
        left_join(
          portfolio_data |>
            rename("..id" = all_of(data_options$id)) |>
            mutate(
              lower_bound = .data[[data_options$date]],
              upper_bound = .data[[data_options$date]] + months(12)
            ) |>
            select(-all_of(data_options$date)),
          join_by(..id, closest(..date >= lower_bound), ..date < upper_bound),
          relationship = "many-to-one"
        ) |>
        rename("{data_options$date}" := "..date", "{data_options$id}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      group_by(
        portfolio_main,
        portfolio_secondary,
        .data[[data_options$date]]
      ) |>
      summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(ret_excess, mktcap_lag)
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(ret_excess)
        ),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, .data[[data_options$date]]) |>
      summarize(
        across(c(ret_excess_vw, ret_excess_ew), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (mktcap_lag_missing) {
    portfolio_returns <- portfolio_returns |> select(-ret_excess_vw)
  }
  portfolio_returns[!is.na(portfolio_returns$portfolio), ]
}

# =============================================================================
# OPTIMIZED IMPLEMENTATION
# =============================================================================
#' Compute Portfolio Returns
#'
#' This function computes individual portfolio returns based on specified
#' sorting variables and sorting methods. The portfolios can be rebalanced every
#' period or on an annual frequency by specifying a rebalancing month, which is
#' only applicable at a monthly return frequency. The function supports
#' univariate and bivariate sorts, with the latter supporting dependent and
#' independent sorting methods.
#'
#' @details The function checks for consistency in the provided arguments. For
#'   univariate sorts, a single sorting variable and a corresponding number of
#'   portfolios must be provided. For bivariate sorts, two sorting variables and
#'   two corresponding numbers of portfolios (or percentiles) are required. The
#'   sorting method determines how portfolios are assigned and returns are
#'   computed. The function handles missing and extreme values appropriately
#'   based on the specified sorting method and rebalancing frequency.
#'
#' @param sorting_data A data frame containing the dataset for portfolio
#'   assignment and return computation. Following CRSP naming conventions, the
#'   panel data must identify individual stocks with `permno` and the time point
#'   with `date`. It must contain columns for the sorting variables and
#'   `ret_excess`. Additionally, `mktcap_lag` is needed for value-weighted
#'   returns.
#' @param sorting_variables A character vector specifying the column names in
#'   `sorting_data` to be used for sorting and determining portfolio
#'   assignments. For univariate sorts, provide a single variable. For bivariate
#'   sorts, provide two variables, where the first string refers to the main
#'   variable and the second string refers to the secondary ("control")
#'   variable.
#' @param sorting_method A string specifying the sorting method to be used.
#'   Possible values are:
#'   \itemize{
#'     \item `"univariate"`: For a single sorting variable.
#'     \item `"bivariate-dependent"`: For two sorting variables, where the main
#'      sort depends on the secondary variable.
#'     \item `"bivariate-independent"`: For two independent sorting variables.
#'   }
#'   For bivariate sorts, the portfolio returns are averaged over the
#'   controlling sorting variable (i.e., the second sorting variable) and only
#'   portfolio returns for the main sorting variable (given as the first element
#'   of `sorting_variable`) are returned.
#' @param rebalancing_month An integer between 1 and 12 specifying the month in
#'   which to form portfolios that are held constant for one year. For example,
#'   setting it to `7` creates portfolios in July that are held constant until
#'   June of the following year. The default `NULL` corresponds to periodic
#'   rebalancing.
#' @param breakpoint_options_main A named list of \link{breakpoint_options} passed to
#'   `breakpoint_function` for the main sorting variable.
#' @param breakpoint_function_main A function to compute the main sorting
#'   variable. The default is set to \link{compute_breakpoints}.
#' @param breakpoint_options_secondary An optional named list of \link{breakpoint_options}
#'   passed to `breakpoint_function` for the secondary sorting variable.
#' @param breakpoint_function_secondary A function to compute the secondary
#'   sorting variable. The default is set to \link{compute_breakpoints}.
#' @param min_portfolio_size An integer specifying the minimum number of
#'   portfolio constituents (default is set to `0`, effectively deactivating the
#'   check). Small portfolios' returns are set to zero.
#' @param data_options A named list of \link{data_options} with characters, indicating
#'   the column names required to run this function.  The required column names identify dates,
#'   the stocks, and returns. Defaults to `date=date`, `id=permno`, and `ret_excess = ret_excess`.
#'
#' @return A data frame with computed portfolio returns, containing the
#'   following columns:
#'   \itemize{
#'     \item `portfolio`: The portfolio identifier.
#'     \item `date`: The date of the portfolio return.
#'     \item `ret_excess_vw`: The value-weighted excess return of the portfolio
#'      (only computed if the `sorting_data` contains `mktcap_lag`)
#'     \item `ret_excess_ew`: The equal-weighted excess return of the portfolio.
#'   }
#'
#' @note Ensure that the `sorting_data` contains all the required columns: The
#'   specified sorting variables and `ret_excess`. The function will stop and
#'   throw an error if any required columns are missing.
#'
#' @export
#'
#' @examples
#' # Univariate sorting with periodic rebalancing
#' data <- data.frame(
#'   permno = 1:500,
#'   date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 100), each = 10),
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
#'   data, c("size", "mktcap_lag"), "bivariate-independent", 7,
#'   breakpoint_options_main = breakpoint_options(n_portfolios = 5),
#'   breakpoint_options_secondary = breakpoint_options(n_portfolios = 3),
#' )
#'
compute_portfolio_returns_optimized <- function(
  sorting_data,
  sorting_variables,
  sorting_method,
  rebalancing_month = NULL,
  breakpoint_options_main,
  breakpoint_options_secondary = NULL,
  breakpoint_function_main = compute_breakpoints,
  breakpoint_function_secondary = compute_breakpoints,
  min_portfolio_size = 0,
  data_options = NULL
) {
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
    cli::cli_abort(
      "Invalid sorting method. Choose 'univariate', 'bivariate-dependent', or 'bivariate-independent'."
    )
  }

  if (
    (sorting_method %in% c("bivariate-dependent", "bivariate-independent")) &&
      is.null(breakpoint_options_secondary)
  ) {
    cli::cli_warn(
      "No 'breakpoint_options_secondary' specified in bivariate sort."
    )
  }

  required_columns <- c(
    sorting_variables,
    data_options$date,
    data_options$id,
    data_options$ret_excess
  )
  missing_columns <- setdiff(required_columns, colnames(sorting_data))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "The 'sorting_data' is missing the following required columns: {paste(missing_columns, collapse = ', ')}."
    )
  }

  mktcap_lag_missing <- !(data_options$mktcap_lag %in% colnames(sorting_data))

  if (mktcap_lag_missing) {
    sorting_data$mktcap_lag <- 1
  }

  if (
    !is.null(rebalancing_month) &&
      (rebalancing_month > 12 || rebalancing_month < 1)
  ) {
    cli::cli_abort(
      "The 'rebalancing_month' must be NULL (periodic rebalancing) or an integer between 1 and 12 (annual rebalancing)."
    )
  }

  date_col <- data_options$date
  id_col <- data_options$id
  ret_col <- data_options$ret_excess
  mktcap_col <- data_options$mktcap_lag
  exchange_col <- data_options$exchange

  # ── Core optimization: build a slim data frame with only the columns that
  #    breakpoint_function + return computation actually need.
  #    The original passes ALL columns through pick(everything()) per group,
  #    which is the dominant cost when extra columns are present.
  needed_cols <- unique(c(
    sorting_variables,
    date_col,
    id_col,
    ret_col,
    mktcap_col,
    if (exchange_col %in% colnames(sorting_data)) exchange_col
  ))
  slim <- sorting_data[, needed_cols, drop = FALSE]

  if (sorting_method == "univariate") {
    if (length(sorting_variables) > 1) {
      cli::cli_abort("Only provide one sorting variable for univariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      slim$portfolio <- assign_portfolios_bulk(
        slim,
        sorting_variables,
        date_col,
        breakpoint_options_main,
        breakpoint_function_main,
        data_options
      )
      portfolio_returns <- slim
    } else {
      reb_rows <- month(slim[[date_col]]) == rebalancing_month
      reb_data <- slim[reb_rows, , drop = FALSE]

      reb_data$portfolio <- assign_portfolios_bulk(
        reb_data,
        sorting_variables,
        date_col,
        breakpoint_options_main,
        breakpoint_function_main,
        data_options
      )

      portfolio_returns <- merge_annual_rebalancing(
        slim,
        reb_data[, c(id_col, date_col, "portfolio"), drop = FALSE],
        date_col,
        id_col,
        rebalancing_month
      )
    }

    portfolio_returns <- portfolio_returns |>
      group_by(portfolio, .data[[date_col]]) |>
      summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(.data[[ret_col]], .data[[mktcap_col]])
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(.data[[ret_col]])
        ),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-dependent") {
    if (length(sorting_variables) != 2) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      slim$portfolio_secondary <- assign_portfolios_bulk(
        slim,
        sorting_variables[2],
        date_col,
        breakpoint_options_secondary,
        breakpoint_function_secondary,
        data_options
      )

      slim$portfolio_main <- assign_portfolios_bulk_nested(
        slim,
        sorting_variables[1],
        date_col,
        "portfolio_secondary",
        breakpoint_options_main,
        breakpoint_function_main,
        data_options
      )

      portfolio_returns <- slim
    } else {
      reb_rows <- month(slim[[date_col]]) == rebalancing_month
      reb_data <- slim[reb_rows, , drop = FALSE]

      reb_data$portfolio_secondary <- assign_portfolios_bulk(
        reb_data,
        sorting_variables[2],
        date_col,
        breakpoint_options_secondary,
        breakpoint_function_secondary,
        data_options
      )

      reb_data$portfolio_main <- assign_portfolios_bulk_nested(
        reb_data,
        sorting_variables[1],
        date_col,
        "portfolio_secondary",
        breakpoint_options_main,
        breakpoint_function_main,
        data_options
      )

      portfolio_returns <- merge_annual_rebalancing(
        slim,
        reb_data[,
          c(id_col, date_col, "portfolio_main", "portfolio_secondary"),
          drop = FALSE
        ],
        date_col,
        id_col,
        rebalancing_month
      )
    }

    portfolio_returns <- portfolio_returns |>
      group_by(portfolio_main, portfolio_secondary, .data[[date_col]]) |>
      summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(.data[[ret_col]], .data[[mktcap_col]])
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(.data[[ret_col]])
        ),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, .data[[date_col]]) |>
      summarize(
        across(c(ret_excess_vw, ret_excess_ew), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-independent") {
    if (length(sorting_variables) != 2) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      slim$portfolio_secondary <- assign_portfolios_bulk(
        slim,
        sorting_variables[2],
        date_col,
        breakpoint_options_secondary,
        breakpoint_function_secondary,
        data_options
      )

      slim$portfolio_main <- assign_portfolios_bulk(
        slim,
        sorting_variables[1],
        date_col,
        breakpoint_options_main,
        breakpoint_function_main,
        data_options
      )

      portfolio_returns <- slim
    } else {
      reb_rows <- month(slim[[date_col]]) == rebalancing_month
      reb_data <- slim[reb_rows, , drop = FALSE]

      reb_data$portfolio_secondary <- assign_portfolios_bulk(
        reb_data,
        sorting_variables[2],
        date_col,
        breakpoint_options_secondary,
        breakpoint_function_secondary,
        data_options
      )

      reb_data$portfolio_main <- assign_portfolios_bulk(
        reb_data,
        sorting_variables[1],
        date_col,
        breakpoint_options_main,
        breakpoint_function_main,
        data_options
      )

      portfolio_returns <- merge_annual_rebalancing(
        slim,
        reb_data[,
          c(id_col, date_col, "portfolio_main", "portfolio_secondary"),
          drop = FALSE
        ],
        date_col,
        id_col,
        rebalancing_month
      )
    }

    portfolio_returns <- portfolio_returns |>
      group_by(portfolio_main, portfolio_secondary, .data[[date_col]]) |>
      summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(.data[[ret_col]], .data[[mktcap_col]])
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(.data[[ret_col]])
        ),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, .data[[date_col]]) |>
      summarize(
        across(c(ret_excess_vw, ret_excess_ew), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (mktcap_lag_missing) {
    portfolio_returns <- portfolio_returns |> select(-ret_excess_vw)
  }

  portfolio_returns[!is.na(portfolio_returns$portfolio), ]
}


# --- Internal helper functions ------------------------------------------------

#' Bulk-assign portfolios for a single sorting variable grouped by date
#'
#' Replaces: group_by(date) |> mutate(assign_portfolio(pick(everything()), ...))
#'
#' The speed gains come from:
#' 1. The caller passes a slim data frame (only needed columns), so
#'    data[idx, ] only copies a few columns instead of all of them.
#' 2. cli warnings fire at most once instead of per-group.
#' 3. split() on a factor is O(n) with no string conversion.
#'
#' @keywords internal
#' @noRd
assign_portfolios_bulk <- function(
  data,
  sorting_variable,
  date_col,
  breakpoint_options,
  breakpoint_function,
  data_options
) {
  dates <- data[[date_col]]
  sv <- data[[sorting_variable]]
  n <- nrow(data)

  portfolio <- integer(n)

  # split on factor avoids repeated Date->character conversion
  date_indices <- split(seq_len(n), factor(dates))

  warned_constant <- FALSE
  warned_cluster <- FALSE

  for (idx in date_indices) {
    x <- sv[idx]

    if (length(x) <= 1L || all(x == x[1L], na.rm = TRUE)) {
      if (!warned_constant) {
        cli::cli_warn(
          "The sorting variable is constant and only one portfolio is returned."
        )
        warned_constant <- TRUE
      }
      portfolio[idx] <- 1L
      next
    }

    # data is already slim so this subset is cheap
    date_data <- data[idx, , drop = FALSE]
    bp <- breakpoint_function(
      date_data,
      sorting_variable,
      breakpoint_options,
      data_options
    )

    p <- findInterval(x, bp, all.inside = TRUE)

    if (!warned_cluster) {
      n_expected <- length(bp) - 1L
      n_actual <- sum(tabulate(p, nbins = n_expected) > 0L)
      if (n_actual != n_expected) {
        cli::cli_warn(
          "The number of portfolios differs from the specified parameter due to clusters in the sorting variable."
        )
        warned_cluster <- TRUE
      }
    }

    portfolio[idx] <- p
  }

  portfolio
}


#' Bulk-assign portfolios nested within (date, nest_col) groups
#'
#' For bivariate-dependent sorts where main-variable breakpoints are
#' computed within each (date × secondary portfolio) group.
#'
#' @keywords internal
#' @noRd
assign_portfolios_bulk_nested <- function(
  data,
  sorting_variable,
  date_col,
  nest_col,
  breakpoint_options,
  breakpoint_function,
  data_options
) {
  dates <- data[[date_col]]
  nest_vals <- data[[nest_col]]
  sv <- data[[sorting_variable]]
  n <- nrow(data)

  portfolio <- integer(n)

  # interaction() is cheaper than paste() — no large string allocation
  group_factor <- interaction(dates, nest_vals, drop = TRUE, lex.order = FALSE)
  group_indices <- split(seq_len(n), group_factor)

  warned_constant <- FALSE
  warned_cluster <- FALSE

  for (idx in group_indices) {
    x <- sv[idx]

    if (length(x) <= 1L || all(x == x[1L], na.rm = TRUE)) {
      if (!warned_constant) {
        cli::cli_warn(
          "The sorting variable is constant and only one portfolio is returned."
        )
        warned_constant <- TRUE
      }
      portfolio[idx] <- 1L
      next
    }

    date_data <- data[idx, , drop = FALSE]
    bp <- breakpoint_function(
      date_data,
      sorting_variable,
      breakpoint_options,
      data_options
    )

    p <- findInterval(x, bp, all.inside = TRUE)

    if (!warned_cluster) {
      n_expected <- length(bp) - 1L
      n_actual <- sum(tabulate(p, nbins = n_expected) > 0L)
      if (n_actual != n_expected) {
        cli::cli_warn(
          "The number of portfolios differs from the specified parameter due to clusters in the sorting variable."
        )
        warned_cluster <- TRUE
      }
    }

    portfolio[idx] <- p
  }

  portfolio
}


#' Merge annual rebalancing portfolios via vectorized match()
#'
#' Replaces the expensive non-equi join_by(closest(...)) with a
#' vectorized match() on a composite (id, rebalancing_year) key.
#' This avoids building a full merge result or hash table.
#'
#' @keywords internal
#' @noRd
merge_annual_rebalancing <- function(
  sorting_data,
  portfolio_assignments,
  date_col,
  id_col,
  rebalancing_month
) {
  # Compute rebalancing year: if month >= reb_month, use current year;
  # otherwise use previous year.
  sd_months <- month(sorting_data[[date_col]])
  sd_years <- year(sorting_data[[date_col]])
  reb_year_sd <- ifelse(sd_months >= rebalancing_month, sd_years, sd_years - 1L)

  reb_year_pa <- year(portfolio_assignments[[date_col]])

  # Build composite keys for vectorized lookup
  sd_key <- paste0(sorting_data[[id_col]], ".", reb_year_sd)
  pa_key <- paste0(portfolio_assignments[[id_col]], ".", reb_year_pa)

  match_idx <- match(sd_key, pa_key)

  # Copy portfolio columns onto sorting_data in-place (no merge/join overhead)
  port_cols <- setdiff(names(portfolio_assignments), c(id_col, date_col))
  for (col in port_cols) {
    sorting_data[[col]] <- portfolio_assignments[[col]][match_idx]
  }

  sorting_data
}


# =============================================================================
# CORRECTNESS CHECKS
# =============================================================================
cat("=== Correctness checks ===\n")

run_check <- function(label, ...) {
  r_orig <- suppressWarnings(compute_portfolio_returns_original(...))
  r_optim <- suppressWarnings(compute_portfolio_returns_optimized(...))
  # Compare after arranging to ensure row order doesn't matter
  r_orig <- r_orig |> arrange(across(everything()))
  r_optim <- r_optim |> arrange(across(everything()))
  ok <- isTRUE(all.equal(r_orig, r_optim, tolerance = 1e-12))
  cat(sprintf("  %-55s %s\n", label, if (ok) "PASS" else "FAIL"))
  if (!ok) {
    cat(
      "    Dimensions: orig =",
      paste(dim(r_orig), collapse = "x"),
      " optim =",
      paste(dim(r_optim), collapse = "x"),
      "\n"
    )
    diffs <- which(
      !sapply(seq_len(ncol(r_orig)), function(i) {
        isTRUE(all.equal(r_orig[[i]], r_optim[[i]], tolerance = 1e-12))
      })
    )
    if (length(diffs) > 0) {
      cat("    Differing columns:", colnames(r_orig)[diffs], "\n")
    }
  }
  invisible(ok)
}

data_small <- make_panel(n_stocks = 50, n_months = 24, seed = 42)
bp_main <- breakpoint_options(n_portfolios = 5)
bp_sec <- breakpoint_options(n_portfolios = 3)

run_check(
  "univariate, periodic",
  data_small,
  "size",
  "univariate",
  breakpoint_options_main = bp_main
)

run_check(
  "univariate, annual (July)",
  data_small,
  "size",
  "univariate",
  rebalancing_month = 7,
  breakpoint_options_main = bp_main
)

run_check(
  "bivariate-dependent, periodic",
  data_small,
  c("size", "bm"),
  "bivariate-dependent",
  breakpoint_options_main = bp_main,
  breakpoint_options_secondary = bp_sec
)

run_check(
  "bivariate-dependent, annual (July)",
  data_small,
  c("size", "bm"),
  "bivariate-dependent",
  rebalancing_month = 7,
  breakpoint_options_main = bp_main,
  breakpoint_options_secondary = bp_sec
)

run_check(
  "bivariate-independent, periodic",
  data_small,
  c("size", "bm"),
  "bivariate-independent",
  breakpoint_options_main = bp_main,
  breakpoint_options_secondary = bp_sec
)

run_check(
  "bivariate-independent, annual (July)",
  data_small,
  c("size", "bm"),
  "bivariate-independent",
  rebalancing_month = 7,
  breakpoint_options_main = bp_main,
  breakpoint_options_secondary = bp_sec
)

run_check(
  "univariate, no mktcap_lag",
  data_small |> select(-mktcap_lag),
  "size",
  "univariate",
  breakpoint_options_main = bp_main
)

run_check(
  "univariate, min_portfolio_size = 20",
  data_small,
  "size",
  "univariate",
  breakpoint_options_main = bp_main,
  min_portfolio_size = 20
)

cat("\n")

# =============================================================================
# BENCHMARK 1: Univariate periodic — varying panel size
# =============================================================================
cat(
  "=== Benchmark 1: Univariate periodic (5 portfolios) — varying panel size ===\n"
)
cat("    (Extra columns included to stress-test pick() overhead)\n")
cat(sprintf(
  "%-16s %-8s %-20s %-20s %-10s\n",
  "Stocks×Months",
  "Rows",
  "Original (median)",
  "Optimized (median)",
  "Speedup"
))
cat(paste(rep("-", 78), collapse = ""), "\n")

configs <- list(
  list(n_stocks = 100, n_months = 24),
  list(n_stocks = 200, n_months = 60),
  list(n_stocks = 500, n_months = 60),
  list(n_stocks = 500, n_months = 120),
  list(n_stocks = 1000, n_months = 120)
)

for (cfg in configs) {
  df <- make_panel(n_stocks = cfg$n_stocks, n_months = cfg$n_months, seed = 1)
  n_rows <- nrow(df)
  label <- sprintf("%d × %d", cfg$n_stocks, cfg$n_months)
  iters <- max(3L, as.integer(200000 / n_rows))

  bm <- bench::mark(
    original = suppressWarnings(compute_portfolio_returns_original(
      df,
      "size",
      "univariate",
      breakpoint_options_main = bp_main
    )),
    optimized = suppressWarnings(compute_portfolio_returns_optimized(
      df,
      "size",
      "univariate",
      breakpoint_options_main = bp_main
    )),
    iterations = iters,
    check = FALSE,
    filter_gc = FALSE
  )

  t_orig <- as.numeric(bm$median[1], units = "ms")
  t_optim <- as.numeric(bm$median[2], units = "ms")
  speedup <- t_orig / t_optim

  cat(sprintf(
    "%-16s %-8s %-20s %-20s %-10s\n",
    label,
    format(n_rows, big.mark = ","),
    sprintf("%.1f ms", t_orig),
    sprintf("%.1f ms", t_optim),
    sprintf("%.2fx", speedup)
  ))
}

# =============================================================================
# BENCHMARK 2: Univariate annual rebalancing
# =============================================================================
cat(
  "\n=== Benchmark 2: Univariate annual rebalancing (July, 5 portfolios) ===\n"
)
cat(sprintf(
  "%-16s %-8s %-20s %-20s %-10s\n",
  "Stocks×Months",
  "Rows",
  "Original (median)",
  "Optimized (median)",
  "Speedup"
))
cat(paste(rep("-", 78), collapse = ""), "\n")

for (cfg in configs[1:3]) {
  df <- make_panel(n_stocks = cfg$n_stocks, n_months = cfg$n_months, seed = 1)
  n_rows <- nrow(df)
  label <- sprintf("%d × %d", cfg$n_stocks, cfg$n_months)
  iters <- max(3L, as.integer(100000 / n_rows))

  bm <- bench::mark(
    original = suppressWarnings(compute_portfolio_returns_original(
      df,
      "size",
      "univariate",
      rebalancing_month = 7,
      breakpoint_options_main = bp_main
    )),
    optimized = suppressWarnings(compute_portfolio_returns_optimized(
      df,
      "size",
      "univariate",
      rebalancing_month = 7,
      breakpoint_options_main = bp_main
    )),
    iterations = iters,
    check = FALSE,
    filter_gc = FALSE
  )

  t_orig <- as.numeric(bm$median[1], units = "ms")
  t_optim <- as.numeric(bm$median[2], units = "ms")
  speedup <- t_orig / t_optim

  cat(sprintf(
    "%-16s %-8s %-20s %-20s %-10s\n",
    label,
    format(n_rows, big.mark = ","),
    sprintf("%.1f ms", t_orig),
    sprintf("%.1f ms", t_optim),
    sprintf("%.2fx", speedup)
  ))
}

# =============================================================================
# BENCHMARK 3: Bivariate independent — the most expensive path
# =============================================================================
cat("\n=== Benchmark 3: Bivariate independent periodic (5×3 portfolios) ===\n")
cat(sprintf(
  "%-16s %-8s %-20s %-20s %-10s\n",
  "Stocks×Months",
  "Rows",
  "Original (median)",
  "Optimized (median)",
  "Speedup"
))
cat(paste(rep("-", 78), collapse = ""), "\n")

for (cfg in configs[1:3]) {
  df <- make_panel(n_stocks = cfg$n_stocks, n_months = cfg$n_months, seed = 1)
  n_rows <- nrow(df)
  label <- sprintf("%d × %d", cfg$n_stocks, cfg$n_months)
  iters <- max(3L, as.integer(100000 / n_rows))

  bm <- bench::mark(
    original = suppressWarnings(compute_portfolio_returns_original(
      df,
      c("size", "bm"),
      "bivariate-independent",
      breakpoint_options_main = bp_main,
      breakpoint_options_secondary = bp_sec
    )),
    optimized = suppressWarnings(compute_portfolio_returns_optimized(
      df,
      c("size", "bm"),
      "bivariate-independent",
      breakpoint_options_main = bp_main,
      breakpoint_options_secondary = bp_sec
    )),
    iterations = iters,
    check = FALSE,
    filter_gc = FALSE
  )

  t_orig <- as.numeric(bm$median[1], units = "ms")
  t_optim <- as.numeric(bm$median[2], units = "ms")
  speedup <- t_orig / t_optim

  cat(sprintf(
    "%-16s %-8s %-20s %-20s %-10s\n",
    label,
    format(n_rows, big.mark = ","),
    sprintf("%.1f ms", t_orig),
    sprintf("%.1f ms", t_optim),
    sprintf("%.2fx", speedup)
  ))
}

# =============================================================================
# BENCHMARK 4: Memory comparison (univariate periodic, large panel)
# =============================================================================
cat(
  "\n=== Memory comparison: Univariate periodic (500 stocks × 120 months) ===\n"
)

df_mem <- make_panel(n_stocks = 500, n_months = 120, seed = 1)

bm_mem <- bench::mark(
  original = suppressWarnings(compute_portfolio_returns_original(
    df_mem,
    "size",
    "univariate",
    breakpoint_options_main = bp_main
  )),
  optimized = suppressWarnings(compute_portfolio_returns_optimized(
    df_mem,
    "size",
    "univariate",
    breakpoint_options_main = bp_main
  )),
  iterations = 3,
  check = FALSE,
  filter_gc = FALSE
)

cat(sprintf(
  "  Original  — median: %7.1f ms, mem_alloc: %s\n",
  as.numeric(bm_mem$median[1], units = "ms"),
  format(bm_mem$mem_alloc[1])
))
cat(sprintf(
  "  Optimized — median: %7.1f ms, mem_alloc: %s\n",
  as.numeric(bm_mem$median[2], units = "ms"),
  format(bm_mem$mem_alloc[2])
))

cat("\nDone.\n")
