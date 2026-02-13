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

  date_col <- data_options$date
  id_col <- data_options$id
  ret_col <- data_options$ret_excess
  w_col <- data_options$mktcap_lag

  required_columns <- c(sorting_variables, date_col, id_col, ret_col)

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
