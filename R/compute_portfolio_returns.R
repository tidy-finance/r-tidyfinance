compute_portfolio_returns <- function(
    sorting_data,
    sorting_variables,
    sorting_method,
    rebalancing_frequency = "monthly",
    n_portfolios = NULL, # must be a vector
    percentiles = NULL, # must be a list with length == length(sorting_variables)
    breakpoint_exchanges = NULL
) {

  # TODO: add checks
  # sorting_variables, sorting_method and (n_portfolios or percentiles) must be given
  # if exchanges are provided, check whether the data contains the column
  # check whether sorting_data has sorting_variable and mktcap_lag column

  # TODO: pass percentiles instead of n_portfolios

  if (sorting_method == "univariate") {

    if (length(sorting_variables) > 1) {
      cli::cli_abort("Only provide one sorting variable for univariate sorts.")
    }
    if (length(n_portfolios) > 1) {
      cli::cli_abort("Only provide one number of portfolios for univariate sorts.")
    }

    if (rebalancing_frequency == "monthly") {
      portfolio_returns <- sorting_data |>
        group_by(date) |>
        mutate(
          portfolio = assign_portfolio(
            data = pick(everything()),
            sorting_variable = sorting_variables[1],
            n_portfolios = n_portfolios[1],
            breakpoint_exchanges = breakpoint_exchanges
          )
        )
    } else if (rebalancing_frequency == "annual") {
      portfolio_data <- sorting_data |>
        filter(month(date) == 7) |>
        group_by(date) |>
        mutate(
          portfolio = assign_portfolio(
            data = pick(everything()),
            sorting_variable = sorting_variables[1],
            n_portfolios = n_portfolios[1],
            breakpoint_exchanges = breakpoint_exchanges
          )) |>
        ungroup() |>
        select(permno, date, portfolio)

      portfolio_returns <- sorting_data |>
        left_join(portfolio_data |>
                    mutate(lower_bound = date,
                           upper_bound = date + months(12)) |>
                    select(-date),
                  join_by(permno, closest(date >= lower_bound), date < upper_bound),
                  relationship = "many-to-one")
    }

    portfolio_returns <- portfolio_returns |>
      group_by(portfolio, date) |>
      summarize(
        ret_excess_vw = stats::weighted.mean(ret_excess, mktcap_lag),
        ret_excess_ew = mean(ret_excess),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-dependent") {
    if (length(sorting_variables) != 2) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }
    if (length(n_portfolios) != 2) {
      cli::cli_abort("Provide two numbers of portfolios for bivariate sorts.")
    }

    if (rebalancing_frequency == "monthly") {
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
        ungroup()

    } else if (rebalancing_frequency == "annual") {
      portfolio_data <- sorting_data |>
        filter(month(date) == 7) |>
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
        select(permno, date, portfolio_main, portfolio_secondary)

      portfolio_returns <- sorting_data |>
        left_join(portfolio_data |>
                    mutate(lower_bound = date,
                           upper_bound = date + months(12)) |>
                    select(-date),
                  join_by(permno, closest(date >= lower_bound), date < upper_bound),
                  relationship = "many-to-one")
    }
    portfolio_returns <- portfolio_returns  |>
      group_by(portfolio_main, portfolio_secondary, date) |>
      summarize(
        ret_excess_vw = stats::weighted.mean(ret_excess, mktcap_lag),
        ret_excess_ew = mean(ret_excess),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, date) |>
      summarize(across(c(ret_excess_vw, ret_excess_ew), mean),
                .groups = "drop")
  }

  if (sorting_method == "bivariate-independent") {
    if (length(sorting_variables) != 2) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }
    if (length(n_portfolios) != 2) {
      cli::cli_abort("Provide two numbers of portfolios for bivariate sorts.")
    }

    if (rebalancing_frequency == "monthly") {
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
        ungroup()

    } else if (rebalancing_frequency == "annual") {
      portfolio_data <- sorting_data |>
        filter(month(date) == 7) |>
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
        select(permno, date, portfolio_main, portfolio)

      portfolio_returns <- sorting_data |>
        left_join(portfolio_data |>
                    mutate(lower_bound = date,
                           upper_bound = date + months(12)) |>
                    select(-date),
                  join_by(permno, closest(date >= lower_bound), date < upper_bound),
                  relationship = "many-to-one")
    }
    portfolio_returns <- portfolio_returns  |>
      group_by(portfolio_main, portfolio_secondary, date) |>
      summarize(
        ret_excess_vw = stats::weighted.mean(ret_excess, mktcap_lag),
        ret_excess_ew = mean(ret_excess),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, date) |>
      summarize(across(c(ret_excess_vw, ret_excess_ew), mean),
                .groups = "drop")
  }

  portfolio_returns
}
