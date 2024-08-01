#' Portfolio sort: Annual, single sort
#'
#' Perform portfolio sort # TODO: Expand when parameters are settled.
#'
#' @param sorting_data Some dataset... # TODO: Expand when parameters are settled.
#'
#' @return A tibble with returns per portfolio.
#'
#' @examples
#' \donttest{
#'   compute_portfolio_return_annual_single(crsp_monthly, "shrout") # TODO: Is there a more interesting example with the base data?
#' }
#'
#' @export
compute_portfolio_return_annual_single <- function(
    sorting_data,
    sorting_variable,
    breakpoints_main = 5, # TODO: Do we want to support breakpoints using percentiles in the function assign_portfolios?
    breakpoints_exchanges = "NYSE",
    weighting_scheme = "VW"
) {
  # Create portfolio assignment
  portfolio_data <- sorting_data |> # TODO: In principle, we can make this replaceable and allow passing a tibble with permno, date, portfolio?!
    filter(month(date) == 7) |>
    group_by(date) |>
    mutate(
      portfolio = assign_portfolios(
        sorting_data = pick(all_of(sorting_variable), exchange),
        sorting_variable = sorting_variable,
        breakpoints_number = breakpoints_main,
        breakpoints_exchanges = breakpoints_exchanges
      )) |>
    ungroup() |>
    select(permno, date, portfolio) |>
    drop_na()

  # Merge and compute returns
  sorting_data |>
    left_join(portfolio_data |>
                mutate(lower_bound = date,
                       upper_bound = date + months(12)) |>
                select(-date),
              join_by(permno, closest(date >= lower_bound), date < upper_bound),
              relationship = "many-to-one") |>
    drop_na(portfolio) |>
    group_by(date, portfolio) |>
    summarize(ret_excess = if_else(weighting_scheme == "VW",
                                   weighted.mean(ret_excess, mktcap_lag),
                                   mean(ret_excess)),
              .groups = "drop")
}


#' Portfolio sort: Annual, independent double sort
#'
#' Perform portfolio sort
#'
#' @param sorting_data Some dataset...
#'
#' @return A tibble with returns per portfolio.
#'
#' @examples
#' \donttest{
#'   compute_portfolio_return_annual_double_independent(crsp_monthly, "shrout")
#' }
#'
#' @export
compute_portfolio_return_annual_double_independent <- function(
    sorting_data,
    sorting_variable,
    breakpoints_main = 5,
    breakpoints_secondary = 2,
    breakpoints_exchanges = "NYSE",
    weighting_scheme = "VW"
) {
  # Create portfolio assignment
  portfolio_data <- sorting_data |>
    filter(month(date) == 7) |>
    group_by(date) |>
    mutate(
      portfolio_main = assign_portfolios(
        sorting_data = pick(all_of(sorting_variable), exchange),
        sorting_variable = sorting_variable,
        breakpoints_number = breakpoints_main,
        breakpoints_exchanges = breakpoints_exchanges
      ),
      portfolio_secondary = assign_portfolios(
        sorting_data = pick(mktcap_lag, exchange),
        sorting_variable = "mktcap_lag",
        breakpoints_number = breakpoints_secondary,
        breakpoints_exchanges = breakpoints_exchanges
      )) |>
    ungroup() |>
    drop_na(portfolio_main, portfolio_secondary) |>
    mutate(portfolio = paste0(portfolio_main, "-", portfolio_secondary)) |>
    select(permno, date, portfolio_main, portfolio)

  # Merge and compute returns
  sorting_data |>
    left_join(portfolio_data |>
                mutate(lower_bound = date,
                       upper_bound = date + months(12)) |>
                select(-date),
              join_by(permno, closest(date >= lower_bound), date < upper_bound),
              relationship = "many-to-one") |>
    drop_na(portfolio) |>
    group_by(date, portfolio) |>
    summarize(ret_excess = if_else(weighting_scheme == "VW",
                                   weighted.mean(ret_excess, mktcap_lag),
                                   mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") |>
    group_by(date, portfolio_main) |>
    summarize(ret_excess = mean(ret_excess),
              .groups = "drop") |>
    rename(portfolio = portfolio_main)
}


#' Portfolio sort: Annual, dependent double sort
#'
#' Perform portfolio sort
#'
#' @param sorting_data Some dataset...
#'
#' @return A tibble with returns per portfolio.
#'
#' @examples
#' \donttest{
#'   compute_portfolio_return_annual_double_dependent(crsp_monthly, "shrout")
#' }
#'
#' @export
compute_portfolio_return_annual_double_dependent <- function(
    sorting_data,
    sorting_variable,
    breakpoints_main = 5,
    breakpoints_secondary = 2,
    breakpoints_exchanges = "NYSE",
    weighting_scheme = "VW"
) {
  # Create portfolio assignment
  portfolio_data <- sorting_data |>
    filter(month(date) == 7) |>
    group_by(date) |>
    mutate(
      portfolio_secondary = assign_portfolios(
        sorting_data = pick(mktcap_lag, exchange),
        sorting_variable = "mktcap_lag",
        breakpoints_number = breakpoints_secondary,
        breakpoints_exchanges = breakpoints_exchanges
      )) |>
    ungroup() |>
    drop_na(portfolio_secondary) |>
    group_by(date, portfolio_secondary) |>
    mutate(
      portfolio_main = assign_portfolios(
        sorting_data = pick(all_of(sorting_variable), exchange),
        sorting_variable = sorting_variable,
        breakpoints_number = breakpoints_main,
        breakpoints_exchanges = breakpoints_exchanges
      )) |>
    ungroup() |>
    drop_na(portfolio_main) |>
    mutate(portfolio = paste0(portfolio_main, "-", portfolio_secondary)) |>
    select(permno, date, portfolio_main, portfolio)

  # Merge and compute returns
  sorting_data |>
    left_join(portfolio_data |>
                mutate(lower_bound = date,
                       upper_bound = date + months(12)) |>
                select(-date),
              join_by(permno, closest(date >= lower_bound), date < upper_bound),
              relationship = "many-to-one") |>
    drop_na(portfolio) |>
    group_by(date, portfolio) |>
    summarize(ret_excess = if_else(weighting_scheme == "VW",
                                   weighted.mean(ret_excess, mktcap_lag),
                                   mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") |>
    group_by(date, portfolio_main) |>
    summarize(ret_excess = mean(ret_excess),
              .groups = "drop") |>
    rename(portfolio = portfolio_main)
}


#' Portfolio sort: Monthly, single sort
#'
#' Perform portfolio sort
#'
#' @param sorting_data Some dataset...
#'
#' @return A tibble with returns per portfolio.
#'
#' @examples
#' \donttest{
#'   compute_portfolio_return_monthly_single(crsp_monthly, "shrout")
#' }
#'
#' @export
compute_portfolio_return_monthly_single <- function(
    sorting_data,
    sorting_variable,
    breakpoints_main = 5,
    breakpoints_exchanges = "NYSE",
    weighting_scheme = "VW"
) {
  sorting_data |>
    group_by(date) |>
    mutate(
      portfolio = assign_portfolios(
        sorting_data = pick(all_of(sorting_variable), exchange),
        sorting_variable = sorting_variable,
        breakpoints_number = breakpoints_main,
        breakpoints_exchanges = breakpoints_exchanges
      )) |>
    ungroup() |>
    drop_na(portfolio) |>
    group_by(date, portfolio) |>
    summarize(ret_excess = if_else(weighting_scheme == "VW",
                                   weighted.mean(ret_excess, mktcap_lag),
                                   mean(ret_excess)),
              .groups = "drop")
}


#' Portfolio sort: Monthly, independent double sort
#'
#' Perform portfolio sort
#'
#' @param sorting_data Some dataset...
#'
#' @return A tibble with returns per portfolio.
#'
#' @examples
#' \donttest{
#'   compute_portfolio_return_monthly_double_independent(crsp_monthly, "shrout")
#' }
#'
#' @export
compute_portfolio_return_monthly_double_independent <- function(
    sorting_data,
    sorting_variable,
    breakpoints_main = 5,
    breakpoints_secondary = 2,
    breakpoints_exchanges = "NYSE",
    weighting_scheme = "VW"
) {
  sorting_data |>
    group_by(date) |>
    mutate(
      portfolio_main = assign_portfolios(
        sorting_data = pick(all_of(sorting_variable), exchange),
        sorting_variable = sorting_variable,
        breakpoints_number = breakpoints_main,
        breakpoints_exchanges = breakpoints_exchanges
      ),
      portfolio_secondary = assign_portfolios(
        sorting_data = pick(mktcap_lag, exchange),
        sorting_variable = "mktcap_lag",
        breakpoints_number = breakpoints_secondary,
        breakpoints_exchanges = breakpoints_exchanges
      )) |>
    ungroup() |>
    drop_na(portfolio_main, portfolio_secondary) |>
    mutate(portfolio = paste0(portfolio_main, "-", portfolio_secondary)) |>
    group_by(date, portfolio) |>
    summarize(ret_excess = if_else(weighting_scheme == "VW",
                                   weighted.mean(ret_excess, mktcap_lag),
                                   mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") |>
    group_by(date, portfolio_main) |>
    summarize(ret_excess = mean(ret_excess),
              .groups = "drop") |>
    rename(portfolio = portfolio_main)
}


#' Portfolio sort: Monthly, dependent double sort
#'
#' Perform portfolio sort
#'
#' @param sorting_data Some dataset...
#'
#' @return A tibble with returns per portfolio.
#'
#' @examples
#' \donttest{
#'   compute_portfolio_return_monthly_double_dependent(crsp_monthly, "shrout")
#' }
#'
#' @export
compute_portfolio_return_monthly_double_dependent <- function(
    sorting_data,
    sorting_variable,
    breakpoints_main = 5,
    breakpoints_secondary = 2,
    breakpoints_exchanges = "NYSE",
    weighting_scheme = "VW"
) {
  sorting_data |>
    group_by(date) |>
    mutate(
      portfolio_secondary = assign_portfolios(
        sorting_data = pick(mktcap_lag, exchange),
        sorting_variable = "mktcap_lag",
        breakpoints_number = breakpoints_secondary,
        breakpoints_exchanges = breakpoints_exchanges
      )) |>
    ungroup() |>
    drop_na(portfolio_secondary) |>
    group_by(date, portfolio_secondary) |>
    mutate(
      portfolio_main = assign_portfolios(
        sorting_data = pick(all_of(sorting_variable), exchange),
        sorting_variable = sorting_variable,
        breakpoints_number = breakpoints_main,
        breakpoints_exchanges = breakpoints_exchanges
      )) |>
    ungroup() |>
    drop_na(portfolio_main) |>
    mutate(portfolio = paste0(portfolio_main, "-", portfolio_secondary)) |>
    group_by(date, portfolio) |>
    summarize(ret_excess = if_else(weighting_scheme == "VW",
                                   weighted.mean(ret_excess, mktcap_lag),
                                   mean(ret_excess)),
              portfolio_main = unique(portfolio_main),
              .groups = "drop") |>
    group_by(date, portfolio_main) |>
    summarize(ret_excess = mean(ret_excess),
              .groups = "drop") |>
    rename(portfolio = portfolio_main)
}
