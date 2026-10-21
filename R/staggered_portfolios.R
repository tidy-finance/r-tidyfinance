#' Assign portfolios at formation dates (internal helper)
#'
#' Assigns portfolios on every date of `data` with the given sorting method.
#' Used for annual rebalancing (on the dates of the rebalancing month) and for
#' staggered rebalancing (on every date).
#'
#' @param data A panel with the sorting variables on its formation dates.
#' @param sorting_variables,sorting_method,breakpoint_options_main,
#'   breakpoint_options_secondary,breakpoint_function_main,
#'   breakpoint_function_secondary,data_options See
#'   [compute_portfolio_returns()].
#' @param id_col,date_col Column names.
#'
#' @returns A data frame with the id and date columns and `portfolio` for
#'   univariate sorts, or `portfolio_main` and `portfolio_secondary` for
#'   bivariate sorts.
#'
#' @keywords internal
#' @noRd
assign_formation_portfolios <- function(
  data,
  sorting_variables,
  sorting_method,
  breakpoint_options_main,
  breakpoint_options_secondary,
  breakpoint_function_main,
  breakpoint_function_secondary,
  data_options,
  id_col,
  date_col
) {
  if (sorting_method == "univariate") {
    return(
      data |>
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
        ) |>
        dplyr::ungroup() |>
        dplyr::select(dplyr::all_of(c(id_col, date_col, "portfolio")))
    )
  }

  portfolio_data <- data |>
    check_new_col(
      c("portfolio_secondary", "portfolio_main", ".min_mktcap_main")
    ) |>
    dplyr::group_by(.data[[date_col]]) |>
    dplyr::mutate(
      portfolio_secondary = assign_portfolio(
        dplyr::pick(dplyr::everything()),
        sorting_variable = sorting_variables[2L],
        breakpoint_options = breakpoint_options_secondary,
        breakpoint_function = breakpoint_function_secondary,
        data_options = data_options
      )
    )

  if (sorting_method == "bivariate-dependent") {
    portfolio_data <- portfolio_data |>
      dplyr::mutate(
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
          sorting_variable = sorting_variables[1L],
          breakpoint_options = with_min_mktcap(
            breakpoint_options_main,
            dplyr::first(.data$.min_mktcap_main)
          ),
          breakpoint_function = breakpoint_function_main,
          data_options = data_options
        )
      )
  } else {
    portfolio_data <- portfolio_data |>
      dplyr::mutate(
        portfolio_main = assign_portfolio(
          dplyr::pick(dplyr::everything()),
          sorting_variable = sorting_variables[1L],
          breakpoint_options = breakpoint_options_main,
          breakpoint_function = breakpoint_function_main,
          data_options = data_options
        )
      )
  }

  portfolio_data |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c(
      id_col,
      date_col,
      "portfolio_main",
      "portfolio_secondary"
    )))
}

#' Compute returns of staggered (overlapping) portfolios (internal helper)
#'
#' Forms portfolios on every date of `formation_data` and holds each cohort
#' for `holding_period` dates, starting `skip` dates after formation. The
#' return of a portfolio on a date is the equal-weighted average of the
#' returns of the cohorts held on that date (Jegadeesh and Titman, 1993).
#' Each cohort's return is computed from `holding_data`, so a stock stays in
#' its cohort even if it lacks a sorting variable or fails a screen in a
#' holding month.
#'
#' @param formation_data The panel used to form portfolios: rows with
#'   non-missing sorting variables that pass the screens.
#' @param holding_data The full panel with returns and weights.
#' @param holding_period,skip See [compute_portfolio_returns()].
#' @param dates The sorted dates of the panel. Holding dates are counted on
#'   this grid, so it must not have gaps.
#' @param ret_col,w_col,w_capped_col,min_portfolio_size See
#'   [compute_portfolio_returns()].
#' @param ... Passed to `assign_formation_portfolios()`.
#'
#' @returns A data frame with columns `portfolio`, the date column,
#'   `ret_excess_vw`, `ret_excess_ew`, and `ret_excess_vw_capped`.
#'
#' @keywords internal
#' @noRd
compute_staggered_returns <- function(
  formation_data,
  holding_data,
  sorting_method,
  holding_period,
  skip,
  dates,
  id_col,
  date_col,
  ret_col,
  w_col,
  w_capped_col,
  min_portfolio_size,
  ...
) {
  assignments <- assign_formation_portfolios(
    formation_data,
    sorting_method = sorting_method,
    id_col = id_col,
    date_col = date_col,
    ...
  ) |>
    check_new_col(".cohort") |>
    dplyr::rename(.cohort = dplyr::all_of(date_col))

  # Expand each assignment to its holding dates on the date grid
  formation_index <- match(assignments$.cohort, dates)
  offsets <- skip + seq_len(holding_period) - 1L
  held <- assignments[rep(seq_len(nrow(assignments)), each = holding_period), ]
  holding_index <- rep(formation_index, each = holding_period) +
    rep(offsets, times = nrow(assignments))
  held[[date_col]] <- dates[holding_index]
  held <- held[!is.na(held[[date_col]]), ]

  cohort_returns <- held |>
    dplyr::inner_join(
      holding_data |>
        dplyr::select(dplyr::all_of(c(
          id_col,
          date_col,
          ret_col,
          w_col,
          w_capped_col
        ))),
      by = c(id_col, date_col)
    )

  if (sorting_method == "univariate") {
    cohort_returns <- cohort_returns |>
      dplyr::group_by(.data$portfolio, .data$.cohort, .data[[date_col]]) |>
      summarise_portfolio_returns(
        ret_col,
        w_col,
        w_capped_col,
        min_portfolio_size
      )
  } else {
    cohort_returns <- aggregate_bivariate_returns(
      cohort_returns,
      date_col,
      ret_col,
      w_col,
      w_capped_col,
      min_portfolio_size,
      by = ".cohort"
    )
  }

  cohort_returns |>
    dplyr::group_by(.data$portfolio, .data[[date_col]]) |>
    dplyr::summarise(
      dplyr::across(
        c("ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped"),
        \(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}
