# # Move lag_method to separate function?
#
# if (lag_method == "fama-french") {
#
# }
#
# if (lag_method == "6-months") {
#
# }
#
# if (lag_method == "mixed") {
#
# }
#
# tidy_finance <- DBI::dbConnect(
#   RSQLite::SQLite(),
#   "../website/r/data/tidy_finance_r.sqlite",
#   extended_types = TRUE
# )
#
# sorting_data <- tbl(tidy_finance, "crsp_monthly") |>
#   select(permno, date, ret_excess, mktcap, mktcap_lag), exchange |>
#   collect()

# NOTE: use sorting variables and n_portfolios as vectors
# NOTE: don't drop missing portfolio rows, users should take care of this before sorts
# NOTE: align naming with books (not single, double)
# NOTE: just return value-weighted and equal-weighted returns instead of letting user choose
# NOTE: I don't see the need to rename n_portfolios
compute_portfolio_returns <- function(
    sorting_data,
    sorting_variables,
    sorting_method,
    rebalancing_frequency = "monthly",
    n_portfolios = NULL,
    percentiles = NULL,
    breakpoint_exchanges = NULL
) {

  # TODO: add checks
  # sorting_variables, sorting_method and (n_portfolios or percentiles) must be given
  # if exchanges are provided, check whether the data contains the column
  # check whether sorting_data has sorting_variable and mktcap_lag column

  # TODO: pass percentiles instead of n_portfolios

  # TODO: implement "annual" method put forward by PW using rebalancing_frequency parameter

  if (sorting_method == "univariate") {

    if (length(sorting_variables) > 1) {
      stop("Only provide one sorting variable for univariate sorts.")
    }
    if (length(n_portfolios) > 1) {
      stop("Only provide one number of portfolios for univariate sorts.")
    }

    portfolio_returns <- sorting_data |>
      group_by(date) |>
      mutate(
        portfolio = assign_portfolio(
          data = pick(everything()),
          sorting_variable = sorting_variables[1],
          n_portfolios = n_portfolios[1],
          breakpoint_exchanges = breakpoint_exchanges
        )
      ) |>
      group_by(portfolio, date) |>
      summarize(
        ret_excess_vw = weighted.mean(ret_excess, mktcap_lag),
        ret_excess_ew = mean(ret_excess),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-dependent") {
    if (length(sorting_variables) != 2) {
      stop("Provide two sorting variables for bivariate sorts.")
    }
    if (length(n_portfolios) != 2) {
      stop("Provide two numbers of portfolios for bivariate sorts.")
    }

     portfolio_returns <- sorting_data |>
      group_by(date) |>
      mutate(
        portfolio_secondary = assign_portfolio(
          pick(everything()),
          sorting_variable = sorting_variables[1],
          n_portfolios = n_portfolios[1],
          breakpoint_exchanges = breakpoint_exchanges
        )) |>
      ungroup() |>
      group_by(date, portfolio_secondary) |>
      mutate(
        portfolio_main = assign_portfolio(
          pick(everything()),
          sorting_variable = sorting_variables[2],
          n_portfolios = n_portfolios[2],
          breakpoint_exchanges = breakpoint_exchanges
        )) |>
      ungroup() |>
      group_by(portfolio_main, portfolio_secondary, date) |>
      summarize(
        ret_excess_vw = weighted.mean(ret_excess, mktcap_lag),
        ret_excess_ew = mean(ret_excess),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, date) |>
      summarize(across(c(ret_excess_vw, ret_excess_ew), mean),
                .groups = "drop")
  }

  if (sorting_method == "bivariate-independent") {
    if (length(sorting_variables) != 2) {
      stop("Provide two sorting variables for bivariate sorts.")
    }
    if (length(n_portfolios) != 2) {
      stop("Provide two numbers of portfolios for bivariate sorts.")
    }

    portfolio_returns <- sorting_data |>
      group_by(date) |>
      mutate(
        portfolio_secondary = assign_portfolio(
          pick(everything()),
          sorting_variable = sorting_variables[1],
          n_portfolios = n_portfolios[1],
          breakpoint_exchanges = breakpoint_exchanges
        ),
        portfolio_main = assign_portfolio(
          pick(everything()),
          sorting_variable = sorting_variables[2],
          n_portfolios = n_portfolios[2],
          breakpoint_exchanges = breakpoint_exchanges
        )) |>
      ungroup() |>
      group_by(portfolio_main, portfolio_secondary, date) |>
      summarize(
        ret_excess_vw = weighted.mean(ret_excess, mktcap_lag),
        ret_excess_ew = mean(ret_excess),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, date) |>
      summarize(across(c(ret_excess_vw, ret_excess_ew), mean),
                .groups = "drop")
  }

  portfolio_returns
}
