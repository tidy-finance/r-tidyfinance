#' Compute Long-Short Returns
#'
#' This function calculates long-short returns based on the returns of portfolios. The long-short
#' return is computed as the difference between the returns of the "top" and "bottom" portfolios. The direction of the calculation can be adjusted based on whether the return from the "bottom" portfolio is subtracted from or added to the return from the "top" portfolio.
#'
#' @param portfolio_returns A data frame containing portfolio returns. The data frame must include
#'  columns for the portfolio identifier, date, and return measurements. The portfolio column should
#'  indicate different portfolios, and there should be columns for return measurements prefixed with
#'  "ret_".
#' @param direction A character string specifying the direction of the long-short return calculation.
#'  It can be either "top_minus_bottom" or "bottom_minus_top". Default is "top_minus_bottom". If set
#'  to "bottom_minus_top", the return will be computed as (bottom - top).
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
#' portfolio_returns <- compute_portfolio_returns(
#'   data, sorting_variables = "size", sorting_method = "univariate",
#'   breakpoint_options_main = list(n_portfolios = 5)
#' )
#' compute_long_short_returns(portfolio_returns)
#'
compute_long_short_returns <- function(
    portfolio_returns,
    direction = "top_minus_bottom"
) {
  portfolio_returns |>
    filter(portfolio %in% c(min(portfolio), max(portfolio))) |>
    mutate(portfolio = if_else(portfolio == min(portfolio), "bottom", "top")) |>
    tidyr::pivot_longer(contains("ret_"), names_to = "ret_measure", values_to = "ret")|>
    tidyr::pivot_wider(names_from = portfolio, values_from = ret) |>
    mutate(long_short_return = (top - bottom) * if_else(direction == "bottom_minus_top", -1, 1)) |>
    arrange(date) |> select(date, ret_measure, long_short_return)|>
    tidyr::pivot_wider(names_from = ret_measure, values_from = long_short_return)
}
