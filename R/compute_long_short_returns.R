#' Compute long-short portfolio returns
#'
#' Compute long-short portfolio returns
#'
#' @param portfolio_returns Some dataset...
#'
#' @return A tibble with long-short returns.
#'
#' @examples
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
    filter(portfolio %in% c(1, max(portfolio))) |>
    mutate(portfolio = if_else(portfolio == 1, "bottom", "top")) |>
    group_by(date) |>
    summarize(long_short_return = ret_excess[portfolio == "top"] - ret_excess[portfolio == "bottom"],
              .groups = "drop") |>
    mutate(long_short_return = long_short_return * if_else(direction == "bottom_minus_top", -1, 1)) |>
    arrange(date)
}
