#' Compute Portfolio Returns
#'
#' @description
#' `r lifecycle::badge("experimental")`
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
#' @param cap_weight A numeric value between 0 and 1 specifying the percentile
#'   at which market capitalization is winsorized per date when computing
#'   `ret_excess_vw_capped`. Defaults to `0.8`.
#' @param data_options A named list of \link{data_options} with characters, indicating
#'   the column names required to run this function.  The required column names identify dates,
#'   the stocks, and returns. Defaults to `date=date`, `id=permno`, and `ret_excess = ret_excess`.
#' @param quiet A logical value indicating whether to suppress informational
#'   messages about missing values in the output panel (default is `FALSE`).
#'
#' @return A data frame with computed portfolio returns as a complete panel
#'   (all portfolio-date combinations), containing the following columns:
#'   \itemize{
#'     \item `portfolio`: The portfolio identifier.
#'     \item `date`: The date of the portfolio return.
#'     \item `ret_excess_vw`: The value-weighted excess return of the portfolio
#'      (only computed if the `sorting_data` contains `mktcap_lag`). `NA` if
#'      insufficient observations for that portfolio-date.
#'     \item `ret_excess_ew`: The equal-weighted excess return of the portfolio.
#'      `NA` if insufficient observations for that portfolio-date.
#'     \item `ret_excess_vw_capped`: The capped value-weighted excess return of the portfolio
#'      (only computed if the `sorting_data` contains `mktcap_lag`). Weights are
#'      computed using market capitalization winsorized at the `cap_weight`
#'      percentile per date. `NA` if insufficient observations for that
#'      portfolio-date.
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
  min_portfolio_size = 0L,
  cap_weight = 0.8,
  data_options = NULL,
  quiet = FALSE
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
  w_capped_col <- paste0(w_col, "_capped")

  required_columns <- c(sorting_variables, date_col, id_col, ret_col)

  missing_columns <- setdiff(required_columns, colnames(sorting_data))
  if (length(missing_columns) > 0L) {
    cli::cli_abort(
      "Missing columns: {paste(missing_columns, collapse = ', ')}."
    )
  }

  mktcap_lag_missing <- !(w_col %in% colnames(sorting_data))
  if (mktcap_lag_missing) {
    sorting_data$mktcap_lag <- 1L
  }

  # Store the dates before filtering out missing values
  all_dates <- unique(sorting_data[[date_col]])

  # Filter out rows with missing values in any sorting variable, as these cannot
  # be assigned to portfolios
  sorting_data <- sorting_data |>
    tidyr::drop_na(dplyr::all_of(sorting_variables))

  # Compute capped market capitalization per date before replacing NAs,
  # so that the quantile is not distorted by zero-filled missing values
  sorting_data <- sorting_data |>
    dplyr::group_by(.data[[date_col]]) |>
    dplyr::mutate(
      !!w_capped_col := pmin(
        .data[[w_col]],
        stats::quantile(.data[[w_col]], cap_weight, na.rm = TRUE)
      )
    ) |>
    dplyr::ungroup()

  # Replace NA market caps with 0, as these observations should not contribute
  # to the value-weighted return
  missing_mcap_data <- is.na(sorting_data[[w_col]])
  sorting_data[[w_col]][missing_mcap_data] <- 0
  sorting_data[[w_capped_col]][missing_mcap_data] <- 0

  # Drop dates with insufficient observations
  if (min_portfolio_size > 0L) {
    sorting_data <- sorting_data |>
      dplyr::group_by(.data[[date_col]]) |>
      filter(n() >= min_portfolio_size) |>
      ungroup()
  }

  # Handle edge case where all observations are filtered out
  if (nrow(sorting_data) == 0L) {
    if (!quiet) {
      cli::cli_inform(
        paste0(
          "Returning an empty panel: all observations were filtered out ",
          "(n() < {min_portfolio_size} on every date)."
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

  if (sorting_method == "univariate") {
    if (length(sorting_variables) > 1L) {
      cli::cli_abort("Only provide one sorting variable for univariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
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
      portfolio_data <- sorting_data |>
        dplyr::filter(month(.data[[date_col]]) == rebalancing_month) |>
        dplyr::group_by(.data[[date_col]]) |>
        dplyr::mutate(
          portfolio = assign_portfolio(
            data = dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables,
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(all_of(c(id_col, date_col, "portfolio")))

      portfolio_returns <- sorting_data |>
        dplyr::rename(
          "..date" = all_of(date_col),
          "..id" := all_of(id_col)
        ) |>
        dplyr::left_join(
          portfolio_data |>
            dplyr::rename("..id" = all_of(id_col)) |>
            dplyr::mutate(
              lower_bound = .data[[date_col]],
              upper_bound = .data[[date_col]] + months(12)
            ) |>
            dplyr::select(-all_of(date_col)),
          dplyr::join_by(..id, closest(..date >= lower_bound), ..date <= upper_bound),
          relationship = "many-to-one"
        ) |>
        dplyr::rename("{date_col}" := "..date", "{id_col}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      dplyr::group_by(portfolio, .data[[date_col]]) |>
      dplyr::summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(
            .data[[ret_col]],
            .data[[w_col]]
          )
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(.data[[ret_col]])
        ),
        ret_excess_vw_capped = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(.data[[ret_col]], .data[[w_capped_col]])
        ),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-dependent") {
    if (length(sorting_variables) != 2L) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
        dplyr::group_by(.data[[date_col]]) |>
        dplyr::mutate(
          portfolio_secondary = assign_portfolio(
            dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(.data[[date_col]], portfolio_secondary) |>
        dplyr::mutate(
          portfolio_main = assign_portfolio(
            dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        dplyr::ungroup()
    } else {
      portfolio_data <- sorting_data |>
        dplyr::filter(month(.data[[date_col]]) == rebalancing_month) |>
        dplyr::group_by(.data[[date_col]]) |>
        dplyr::mutate(
          portfolio_secondary = assign_portfolio(
            dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables[2L],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::group_by(.data[[date_col]], portfolio_secondary) |>
        dplyr::mutate(
          portfolio_main = assign_portfolio(
            dplyr::pick(dplyr::everything()),
            sorting_variable = sorting_variables[1L],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(all_of(c(
          id_col,
          date_col,
          "portfolio_main",
          "portfolio_secondary"
        )))

      portfolio_returns <- sorting_data |>
        dplyr::rename(
          "..date" = all_of(date_col),
          "..id" := all_of(id_col)
        ) |>
        dplyr::left_join(
          portfolio_data |>
            dplyr::rename("..id" = all_of(id_col)) |>
            dplyr::mutate(
              lower_bound = .data[[date_col]],
              upper_bound = .data[[date_col]] + months(12)
            ) |>
            dplyr::select(-all_of(date_col)),
          dplyr::join_by(..id, closest(..date >= lower_bound), ..date <= upper_bound),
          relationship = "many-to-one"
        ) |>
        dplyr::rename("{date_col}" := "..date", "{id_col}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      dplyr::group_by(
        portfolio_main,
        portfolio_secondary,
        .data[[date_col]]
      ) |>
      dplyr::summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(.data[[ret_col]], .data[[w_col]])
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(.data[[ret_col]])
        ),
        ret_excess_vw_capped = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(.data[[ret_col]], .data[[w_capped_col]])
        ),
        .groups = "drop"
      ) |>
      dplyr::group_by(portfolio = portfolio_main, .data[[date_col]]) |>
      dplyr::summarize(
        across(c(ret_excess_vw, ret_excess_ew, ret_excess_vw_capped), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-independent") {
    if (length(sorting_variables) != 2L) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
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
      portfolio_data <- sorting_data |>
        dplyr::filter(month(.data[[date_col]]) == rebalancing_month) |>
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
        dplyr::ungroup() |>
        dplyr::select(all_of(c(
          id_col,
          date_col,
          "portfolio_main",
          "portfolio_secondary"
        )))

      portfolio_returns <- sorting_data |>
        dplyr::rename(
          "..date" = all_of(date_col),
          "..id" := all_of(id_col)
        ) |>
        dplyr::left_join(
          portfolio_data |>
            dplyr::rename("..id" = all_of(id_col)) |>
            dplyr::mutate(
              lower_bound = .data[[date_col]],
              upper_bound = .data[[date_col]] + months(12L)
            ) |>
            dplyr::select(-all_of(date_col)),
          dplyr::join_by(..id, closest(..date >= lower_bound), ..date <= upper_bound),
          relationship = "many-to-one"
        ) |>
        dplyr::rename("{date_col}" := "..date", "{id_col}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      dplyr::group_by(
        portfolio_main,
        portfolio_secondary,
        .data[[date_col]]
      ) |>
      dplyr::summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(.data[[ret_col]], .data[[w_col]])
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(.data[[ret_col]])
        ),
        ret_excess_vw_capped = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(.data[[ret_col]], .data[[w_capped_col]])
        ),
        .groups = "drop"
      ) |>
      dplyr::group_by(portfolio = portfolio_main, .data[[date_col]]) |>
      dplyr::summarize(
        across(c(ret_excess_vw, ret_excess_ew, ret_excess_vw_capped), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  portfolio_returns <- portfolio_returns[!is.na(portfolio_returns$portfolio), ]

  # Complete the panel: ensure all portfolio-date combinations are present
  all_portfolios <- na.omit(unique(portfolio_returns$portfolio))

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
        "Returning a complete panel with {n_missing} missing value{?s} ",
        "in factor returns due to insufficient observations ",
        "(n() < {min_portfolio_size})."
      )
    )
  }

  portfolio_returns
}
