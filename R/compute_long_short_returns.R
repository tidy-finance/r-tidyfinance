#' Compute long-short portfolio returns
#'
#' Compute long-short portfolio returns
#'
#' @param portfolio_returns Some dataset...
#'
#' @return A tibble with long-short returns.
#'
#' @examples
#' data <- data.frame(
#'   permno = 1:100,
#'   date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 100), each = 10),
#'   mktcap_lag = runif(100, 100, 1000),
#'   ret_excess = rnorm(100),
#'   size = runif(100, 50, 150)
#' )
#' portfolio_returns <- compute_portfolio_returns(data, sorting_variables = "size", sorting_method = "univariate", n_portfolios = 5)
#' \donttest{
#'   compute_long_short_returns(portfolio_returns)
#' }
#'
#' @export
compute_long_short_returns <- function(
    portfolio_returns,
    direction = "top_minus_bottom"
) {
  portfolio_returns |>
    filter(portfolio %in% c(min(portfolio), max(portfolio))) |>
    mutate(portfolio = if_else(portfolio == min(portfolio), "bottom", "top")) |>
    tidyr::pivot_longer(contains("ret_"), names_to="ret_measure", values_to="ret")|>
    tidyr::pivot_wider(names_from = portfolio, values_from = ret) |>
    mutate(long_short_return = (top - bottom) * if_else(direction == "bottom_minus_top", -1, 1)) |>
    arrange(date) |> select(date, ret_measure, long_short_return)|>
    tidyr::pivot_wider(names_from=ret_measure, values_from=long_short_return)
}
