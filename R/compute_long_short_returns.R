#' Compute Long-Short Returns
#'
#' Calculates long-short returns based on the returns of portfolios. The
#' long-short return is computed as the difference between the returns of the
#' "top" and "bottom" portfolios. The direction of the calculation can be
#' adjusted based on whether the return from the "bottom" portfolio is
#' subtracted from or added to the return from the "top" portfolio.
#'
#' @param data A data frame of portfolio returns as returned by
#'   [compute_portfolio_returns()]. It must include the portfolio identifier
#'   and date columns (named via `data_options`) and at least one of the
#'   return columns `ret_excess_vw`, `ret_excess_ew`, and
#'   `ret_excess_vw_capped`.
#' @param direction A character string specifying the direction of the
#'   long-short return calculation. It can be either `"top_minus_bottom"` or
#'   `"bottom_minus_top"`. Default is `"top_minus_bottom"`. If set to
#'   `"bottom_minus_top"`, the return will be computed as (bottom - top).
#' @param data_options A list of class `tidyfinance_data_options` (created via
#'   [data_options()]) specifying column name mappings. The `date` element
#'   specifies the date column and `portfolio` the assigned portfolio. Uses
#'   [data_options()] default if `NULL`: `"date" = "date"` and
#'   `"portfolio" = "portfolio"`.
#'
#' @returns A data frame with the date column and one column per portfolio
#'   return measure present in `data` (`ret_excess_vw`, `ret_excess_ew`, and
#'   `ret_excess_vw_capped`), each holding the long-short return for that
#'   measure, arranged by date.
#'
#' @family portfolio functions
#' @export
#'
#' @examples
#' set.seed(42)
#' data <- data.frame(
#'   permno = 1:100,
#'   date = rep(
#'     seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 100),
#'     each = 10
#'   ),
#'   mktcap_lag = runif(100, 100, 1000),
#'   ret_excess = rnorm(100),
#'   size = runif(100, 50, 150)
#' )
#'
#' portfolio_returns <- compute_portfolio_returns(
#'   data, "size", "univariate",
#'   breakpoint_options_main = breakpoint_options(n_portfolios = 5)
#' )
#'
#' compute_long_short_returns(portfolio_returns)
#'
compute_long_short_returns <- function(
  data,
  direction = "top_minus_bottom",
  data_options = NULL
) {
  valid_directions <- c("top_minus_bottom", "bottom_minus_top")
  if (
    !is.character(direction) ||
      length(direction) != 1L ||
      !direction %in% valid_directions
  ) {
    cli::cli_abort(
      "{.arg direction} must be one of {.val {valid_directions}}."
    )
  }

  if (is.null(data_options)) {
    data_options <- data_options()
  }

  data <- data |>
    check_new_col("..date") |>
    dplyr::mutate(..date = .data[[data_options$date]])

  # The return columns are those that compute_portfolio_returns() produces
  # (ret_excess_vw, ret_excess_ew, ret_excess_vw_capped), which it names the
  # same way regardless of data_options$ret_excess. For backward compatibility
  # also accept any column whose name contains the configured ret_excess name.
  # Columns are kept in data order.
  canonical <- c("ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped")
  ret_cols <- colnames(data)[
    colnames(data) %in% canonical |
      grepl(data_options$ret_excess, colnames(data), fixed = TRUE)
  ]
  if (length(ret_cols) == 0L) {
    cli::cli_abort(c(
      "No portfolio return columns found in {.arg data}.",
      "i" = paste(
        "Expected one or more of {.val {canonical}} (as returned by",
        "{.fn compute_portfolio_returns}), or a column whose name contains",
        "{.val {data_options$ret_excess}}."
      )
    ))
  }

  data |>
    group_by(.data$..date) |>
    filter(
      .data[[data_options$portfolio]] %in%
        c(
          min(.data[[data_options$portfolio]]),
          max(.data[[data_options$portfolio]])
        )
    ) |>
    mutate(
      "{data_options$portfolio}" := factor(
        if_else(
          .data[[data_options$portfolio]] ==
            min(.data[[data_options$portfolio]]),
          "bottom",
          "top"
        ),
        levels = c("bottom", "top")
      )
    ) |>
    ungroup() |>
    tidyr::pivot_longer(
      dplyr::all_of(ret_cols),
      names_to = "ret_measure",
      values_to = "ret"
    ) |>
    tidyr::pivot_wider(
      names_from = "portfolio",
      values_from = "ret",
      names_expand = TRUE
    ) |>
    check_new_col("long_short_return") |>
    mutate(
      long_short_return = (.data$top - .data$bottom) *
        if_else(direction == "bottom_minus_top", -1, 1)
    ) |>
    arrange(.data$..date) |>
    select(-c("top", "bottom", "..date")) |>
    tidyr::pivot_wider(
      names_from = "ret_measure",
      values_from = "long_short_return"
    )
}
