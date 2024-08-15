#' Compute Portfolio Returns
#'
#' This function computes portfolio returns based on specified sorting variables and sorting methods.
#' The portfolios can be rebalanced at a specified frequency, such as monthly or annually. The function
#' supports univariate and bivariate sorts and can handle both dependent and independent sorting methods.
#'
#' @details
#' The function checks for consistency in the provided arguments. For univariate sorts, a single sorting
#' variable and a corresponding number of portfolios must be provided. For bivariate sorts, two sorting
#' variables and two corresponding numbers of portfolios (or percentiles) are required. The sorting method
#' determines how portfolios are assigned and returns are computed. The function handles missing and extreme
#' values appropriately based on the specified sorting method and rebalancing frequency.
#'
#' @param sorting_data A data frame containing the dataset for portfolio assignment and return computation.
#'   It must contain columns for the sorting variables, `mktcap_lag`, and `ret_excess`.
#' @param sorting_variables A character vector specifying the column names in `sorting_data` to be used
#'   for sorting and determining portfolio assignments. For univariate sorts, provide a single variable.
#'   For bivariate sorts, provide two variables.
#' @param sorting_method A string specifying the sorting method to be used. Possible values are:
#'   \itemize{
#'     \item `"univariate"`: For a single sorting variable.
#'     \item `"bivariate-dependent"`: For two sorting variables, where the second sort is dependent on the first.
#'     \item `"bivariate-independent"`: For two independent sorting variables.
#'   }
#' @param rebalancing_frequency A string specifying the frequency of rebalancing. Default is `"monthly"`.
#'   Other supported value is `"annual"`.
#' @param n_portfolios An optional numeric vector specifying the number of portfolios to create for each
#'   sorting variable. For univariate sorts, provide a single number. For bivariate sorts, provide two numbers.
#'   Must match the length of `sorting_variables`.
#' @param percentiles An optional list of numeric vectors specifying the percentiles for determining the
#'   breakpoints of the portfolios. This parameter is mutually exclusive with `n_portfolios`. The length of
#'   the list must match the length of `sorting_variables`.
#' @param breakpoint_exchanges An optional character vector specifying exchange names to filter the data
#'   before computing breakpoints and assigning portfolios. Exchanges must be stored in a column named
#'   `exchange` in `sorting_data`. If `NULL`, no filtering is applied.
#'
#' @return A data frame with computed portfolio returns, containing the following columns:
#'   \itemize{
#'     \item `portfolio`: The portfolio identifier.
#'     \item `date`: The date of the portfolio return.
#'     \item `ret_excess_vw`: The value-weighted excess return of the portfolio (only computed if the `sorting_data` contains the column mktcap_lag)
#'     \item `ret_excess_ew`: The equal-weighted excess return of the portfolio.
#'   }
#'
#' @note
#' Ensure that the `sorting_data` contains all the required columns: the specified sorting variables,
#' and `ret_excess`. The function will stop and throw an error if any required columns are missing.
#'
#' @export
#'
#' @examples
#' # Univariate sorting with monthly rebalancing
#' data <- data.frame(
#'   permno = 1:100,
#'   date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 100), each = 10),
#'   mktcap_lag = runif(100, 100, 1000),
#'   ret_excess = rnorm(100),
#'   size = runif(100, 50, 150)
#' )
#' compute_portfolio_returns(data, sorting_variables = "size", sorting_method = "univariate", n_portfolios = 5)
#'
#' # Bivariate dependent sorting with annual rebalancing
#' compute_portfolio_returns(data, sorting_variables = c("size", "mktcap_lag"), sorting_method = "bivariate-dependent", n_portfolios = c(3, 3), rebalancing_frequency = "annual")
#'
compute_portfolio_returns <- function(
    sorting_data,
    sorting_variables,
    sorting_method,
    rebalancing_frequency = "monthly",
    n_portfolios = NULL, # must be a vector
    percentiles = NULL, # must be a list with length == length(sorting_variables)
    breakpoint_exchanges = NULL
) {

  if (is.null(sorting_variables) || length(sorting_variables) == 0) {
    cli::cli_abort("You must provide at least one sorting variable.")
  }

  if (!sorting_method %in% c("univariate", "bivariate-dependent", "bivariate-independent")) {
    cli::cli_abort("Invalid sorting method. Choose 'univariate', 'bivariate-dependent', or 'bivariate-independent'.")
  }

  if (is.null(n_portfolios) && is.null(percentiles)) {
    cli::cli_abort("You must provide either 'n_portfolios' or 'percentiles'.")
  }

  if (!is.null(n_portfolios) && !is.null(percentiles)) {
    cli::cli_abort("Please provide either 'n_portfolios' or 'percentiles', not both.")
  }

  if (!is.null(n_portfolios) && length(n_portfolios) != length(sorting_variables)) {
    cli::cli_abort("'n_portfolios' must have the same length as 'sorting_variables'.")
  }

  if (!is.null(percentiles)) {
    if (!is.list(percentiles) || length(percentiles) != length(sorting_variables)) {
      cli::cli_abort("'percentiles' must be a list with the same length as 'sorting_variables'.")
    }
  }

  if (!is.null(breakpoint_exchanges)) {
    if (!("exchange" %in% colnames(sorting_data))) {
      cli::cli_abort("The 'sorting_data' must contain an 'exchange' column when 'breakpoint_exchanges' is provided.")
    }
  }

  required_columns <- c(sorting_variables, "ret_excess")
  missing_columns <- setdiff(required_columns, colnames(sorting_data))
  if (length(missing_columns) > 0) {
    cli::cli_abort(glue::glue("The 'sorting_data' is missing the following required columns: {paste(missing_columns, collapse = ', ')}."))
  }

  mktcap_lag_missing <- !"mktcap_lag"%in%colnames(sorting_data)

  if (mktcap_lag_missing){
    sorting_data$mktcap_lag <- 1
  }

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
        select(permno, date, portfolio_main, portfolio_secondary)

      portfolio_returns <- sorting_data |>
        left_join(portfolio_data |>
                    mutate(lower_bound = date,
                           upper_bound = date + months(12)) |>
                    select(-date),
                  join_by(permno, closest(date >= lower_bound), date < upper_bound),
                  relationship = "many-to-one")
    }
    portfolio_returns <- portfolio_returns |>
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

  if(mktcap_lag_missing){
    portfolio_returns <- portfolio_returns |> select(-ret_excess_vw)
  }

  portfolio_returns[!is.na(portfolio_returns$portfolio),]

}
