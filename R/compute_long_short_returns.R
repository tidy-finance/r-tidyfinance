#' Compute Long-Short Returns
#'
#' This function calculates long-short returns based on the returns of portfolios. The long-short
#' return is computed as the difference between the returns of the "top" and "bottom" portfolios. The direction of the calculation can be adjusted based on whether the return from the "bottom" portfolio is subtracted from or added to the return from the "top" portfolio.
#'
#' @param data A data frame containing portfolio returns. The data frame must include
#'  columns for the portfolio identifier, date, and return measurements. The portfolio column should
#'  indicate different portfolios, and there should be columns for return measurements prefixed with
#'  "ret_excess".
#' @param direction A character string specifying the direction of the long-short return calculation.
#'  It can be either "top_minus_bottom" or "bottom_minus_top". Default is "top_minus_bottom". If set
#'  to "bottom_minus_top", the return will be computed as (bottom - top).
#' @param data_options A named list of \link{data_options} with characters, indicating the column names
#'  required to run this function. The required column names identify dates. Defaults to `date = date`, `portfolio=portfolio` and `ret_excess = ret_excess`
#'
#' @return A data frame with columns for date, return measurement types (from the "ret_measure"
#'  column), and the computed long-short returns. The data frame is arranged by date and pivoted to
#'  have return measurement types as columns with their corresponding long-short returns.
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   permno = 1:100,
#'   date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 100), each = 10),
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

  if (is.null(data_options)) {
    data_options <- data_options(portfolio="portfolio", ret_excess = "ret_excess")
  }

  data$..date <- data[[data_options$date]]

  data |>
    group_by(..date) |>
    filter(.data[[data_options$portfolio]] %in% c(min(.data[[data_options$portfolio]]), max(.data[[data_options$portfolio]]))) |>
    mutate("{data_options$portfolio}" := if_else(.data[[data_options$portfolio]] == min(.data[[data_options$portfolio]]), "bottom", "top")) |>
    ungroup() |>
    tidyr::pivot_longer(contains(data_options$ret_excess), names_to = "ret_measure", values_to = "ret")|>
    tidyr::pivot_wider(names_from = portfolio, values_from = ret) |>
    mutate(long_short_return = (top - bottom) * if_else(direction == "bottom_minus_top", -1, 1)) |>
    arrange(..date) |>
    select(-c(top, bottom, ..date))|>
    tidyr::pivot_wider(names_from = ret_measure, values_from = long_short_return)
}
