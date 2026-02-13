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
compute_portfolio_returns <- function(
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
  # Validation -------------------------------------------------------------

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

  is_bivariate <- sorting_method != "univariate"

  if (is_bivariate && is.null(breakpoint_options_secondary)) {
    cli::cli_warn(
      "No 'breakpoint_options_secondary' specified in bivariate sort."
    )
  }

  date_col <- data_options$date
  id_col <- data_options$id
  ret_col <- data_options$ret_excess
  w_col <- data_options$mktcap_lag

  required_columns <- c(sorting_variables, date_col, id_col, ret_col)
  missing_columns <- setdiff(required_columns, colnames(sorting_data))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "The 'sorting_data' is missing the following required columns: {paste(missing_columns, collapse = ', ')}."
    )
  }

  mktcap_lag_missing <- !(w_col %in% names(sorting_data))
  if (mktcap_lag_missing) {
    sorting_data[[w_col]] <- 1
  }

  if (
    !is.null(rebalancing_month) &&
      (rebalancing_month > 12 || rebalancing_month < 1)
  ) {
    cli::cli_abort(
      "The 'rebalancing_month' must be NULL (periodic rebalancing) or an integer between 1 and 12 (annual rebalancing)."
    )
  }

  if (!is_bivariate && length(sorting_variables) > 1) {
    cli::cli_abort("Only provide one sorting variable for univariate sorts.")
  }
  if (is_bivariate && length(sorting_variables) != 2) {
    cli::cli_abort("Provide two sorting variables for bivariate sorts.")
  }

  assign_cols_main <- unique(c(id_col, date_col, sorting_variables[1], w_col))
  assign_cols_sec <- if (is_bivariate) {
    unique(c(id_col, date_col, sorting_variables[2], w_col))
  }

  # Univariate sorts -------------------------------------------------------

  if (isFALSE(is_bivariate)) {
    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
        mutate(
          portfolio = assign_portfolio(
            data = pick(everything()),
            sorting_variable = sorting_variables,
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          ),
          .by = all_of(date_col)
        )
    } else {
      portfolio_data <- sorting_data |>
        filter(month(.data[[date_col]]) == rebalancing_month) |>
        mutate(
          portfolio = assign_portfolio(
            data = pick(everything()),
            sorting_variable = sorting_variables,
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          ),
          .by = all_of(date_col)
        ) |>
        select(all_of(c(id_col, date_col, "portfolio")))

      portfolio_returns <- sorting_data |>
        rename(
          "..date" = all_of(date_col),
          "..id" := all_of(id_col)
        ) |>
        left_join(
          portfolio_data |>
            rename("..id" = all_of(id_col)) |>
            mutate(
              lower_bound = .data[[date_col]],
              upper_bound = .data[[date_col]] + months(12)
            ) |>
            select(-all_of(date_col)),
          join_by(..id, closest(..date >= lower_bound), ..date < upper_bound),
          relationship = "many-to-one"
        ) |>
        rename("{date_col}" := "..date", "{id_col}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      aggregate_portfolio_returns(
        min_portfolio_size = min_portfolio_size,
        by = c("portfolio", date_col),
        ret_col = ret_col,
        w_col = w_col
      )
  }

  # Bivariate sorts --------------------------------------------------------

  if (is_bivariate) {
    is_dependent <- sorting_method == "bivariate-dependent"
    if (isFALSE(is_dependent)) {
      if (is.null(rebalancing_month)) {
        portfolio_returns <- sorting_data |>
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
            ),
            .by = all_of(date_col)
          )
      } else {
        portfolio_data <- sorting_data |>
          filter(month(.data[[date_col]]) == rebalancing_month) |>
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
            ),
            .by = all_of(date_col)
          ) |>
          select(all_of(c(
            id_col,
            date_col,
            "portfolio_main",
            "portfolio_secondary"
          )))

        portfolio_returns <- sorting_data |>
          rename(
            "..date" = all_of(date_col),
            "..id" := all_of(id_col)
          ) |>
          left_join(
            portfolio_data |>
              rename("..id" = all_of(id_col)) |>
              mutate(
                lower_bound = .data[[date_col]],
                upper_bound = .data[[date_col]] + months(12)
              ) |>
              select(-all_of(date_col)),
            join_by(..id, closest(..date >= lower_bound), ..date < upper_bound),
            relationship = "many-to-one"
          ) |>
          rename("{date_col}" := "..date", "{id_col}" := "..id")
      }

      portfolio_returns <- portfolio_returns |>
        aggregate_portfolio_returns(
          min_portfolio_size = min_portfolio_size,
          by = c(
            "portfolio_main",
            "portfolio_secondary",
            date_col
          ),
          ret_col = ret_col,
          w_col = w_col
        ) |>
        rename(portfolio = portfolio_main) |>
        summarize(
          ret_excess_vw = mean(ret_excess_vw, na.rm = TRUE),
          ret_excess_ew = mean(ret_excess_ew, na.rm = TRUE),
          .by = c(portfolio, all_of(date_col))
        )
    } else {
      if (is.null(rebalancing_month)) {
        portfolio_returns <- sorting_data |>
          mutate(
            portfolio_secondary = assign_portfolio(
              pick(everything()),
              sorting_variable = sorting_variables[2],
              breakpoint_options = breakpoint_options_secondary,
              breakpoint_function = breakpoint_function_secondary,
              data_options = data_options
            ),
            .by = all_of(date_col)
          ) |>
          mutate(
            portfolio_main = assign_portfolio(
              pick(everything()),
              sorting_variable = sorting_variables[1],
              breakpoint_options = breakpoint_options_main,
              breakpoint_function = breakpoint_function_main,
              data_options = data_options
            ),
            .by = c(all_of(date_col), portfolio_secondary)
          )
      } else {
        portfolio_data <- sorting_data |>
          filter(month(.data[[date_col]]) == rebalancing_month) |>
          mutate(
            portfolio_secondary = assign_portfolio(
              pick(everything()),
              sorting_variable = sorting_variables[2],
              breakpoint_options = breakpoint_options_secondary,
              breakpoint_function = breakpoint_function_secondary,
              data_options = data_options
            ),
            .by = all_of(date_col)
          ) |>
          mutate(
            portfolio_main = assign_portfolio(
              pick(everything()),
              sorting_variable = sorting_variables[1],
              breakpoint_options = breakpoint_options_main,
              breakpoint_function = breakpoint_function_main,
              data_options = data_options
            ),
            .by = c(all_of(date_col), portfolio_secondary)
          ) |>
          select(all_of(c(
            id_col,
            date_col,
            "portfolio_main",
            "portfolio_secondary"
          )))

        portfolio_returns <- sorting_data |>
          rename(
            "..date" = all_of(date_col),
            "..id" := all_of(id_col)
          ) |>
          left_join(
            portfolio_data |>
              rename("..id" = all_of(id_col)) |>
              mutate(
                lower_bound = .data[[date_col]],
                upper_bound = .data[[date_col]] + months(12)
              ) |>
              select(-all_of(date_col)),
            join_by(..id, closest(..date >= lower_bound), ..date < upper_bound),
            relationship = "many-to-one"
          ) |>
          rename("{date_col}" := "..date", "{id_col}" := "..id")
      }

      portfolio_returns <- portfolio_returns |>
        aggregate_portfolio_returns(
          min_portfolio_size = min_portfolio_size,
          by = c(
            "portfolio_main",
            "portfolio_secondary",
            date_col
          ),
          ret_col = ret_col,
          w_col = w_col
        ) |>
        rename(portfolio = portfolio_main) |>
        summarize(
          ret_excess_vw = mean(ret_excess_vw, na.rm = TRUE),
          ret_excess_ew = mean(ret_excess_ew, na.rm = TRUE),
          .by = c(portfolio, all_of(date_col))
        )
    }
  }

  if (mktcap_lag_missing) {
    portfolio_returns <- portfolio_returns |> select(-ret_excess_vw)
  }

  portfolio_returns[!is.na(portfolio_returns$portfolio), ]
}

#' @keywords internal
#' @noRd
aggregate_portfolio_returns <- function(
  portfolio_returns,
  min_portfolio_size,
  by,
  ret_col,
  w_col
) {
  portfolio_returns |>
    summarize(
      ret_excess_vw = if_else(
        dplyr::n() < min_portfolio_size,
        NA_real_,
        stats::weighted.mean(
          .data[[ret_col]],
          .data[[w_col]]
        )
      ),
      ret_excess_ew = if_else(
        dplyr::n() < min_portfolio_size,
        NA_real_,
        mean(.data[[ret_col]])
      ),
      .by = all_of(by)
    )
}
