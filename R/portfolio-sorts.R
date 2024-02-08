assign_portfolio <- function(data,
                             sorting_variable,
                             n_portfolios) {
  # Compute breakpoints
  breakpoints <- data |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )

  # Assign portfolios
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |>
        pull({{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)

  # Output
  return(assigned_portfolios)
}

assign_portfolio <- function(n_portfolios,
                             exchanges,
                             data) {
  # Compute breakpoints
  breakpoints <- data |>
    filter(exchange %in% exchanges) |>
    pull(mktcap_lag) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )

  # Assign portfolios
  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(mktcap_lag,
                                    breakpoints,
                                    all.inside = TRUE
    )) |>
    pull(portfolio)

  # Output
  return(assigned_portfolios)
}

assign_portfolio <- function(data,
                             sorting_variable,
                             n_portfolios,
                             exchanges) {
  breakpoints <- data |>
    filter(exchange %in% exchanges) |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = seq(0, 1, length.out = n_portfolios + 1),
      na.rm = TRUE,
      names = FALSE
    )

  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |>
        pull({{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)

  return(assigned_portfolios)
}

assign_portfolio <- function(data,
                             sorting_variable,
                             percentiles) {
  breakpoints <- data |>
    filter(exchange == "NYSE") |>
    pull({{ sorting_variable }}) |>
    quantile(
      probs = percentiles,
      na.rm = TRUE,
      names = FALSE
    )

  assigned_portfolios <- data |>
    mutate(portfolio = findInterval(
      pick(everything()) |>
        pull({{ sorting_variable }}),
      breakpoints,
      all.inside = TRUE
    )) |>
    pull(portfolio)

  return(assigned_portfolios)
}

