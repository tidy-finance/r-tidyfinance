#' Assign Portfolios Based on Sorting Variable
#'
#' This function assigns data points to portfolios based on a specified sorting
#' variable. It can optionally filter the data by exchanges before assignment.
#' The function requires either the number of portfolios to be created or
#' specific percentiles for the breakpoints, but not both. The function also
#' handles cases where the sorting variable clusters on the edges, by assigning
#' all extreme values to the edge portfolio(s) and attempting to sort the
#' remaining values into the equal-sized portfolios (which can be deactivated).
#'
#' @param data A data frame containing the dataset for portfolio assignment.
#' @param sorting_variable A string specifying the column name in `data` to be
#'   used for sorting and determining portfolio assignments.
#' @param n_portfolios An optional integer specifying the number of equally
#'   sized portfolios to create. This parameter is mutually exclusive with
#'   `percentiles`.
#' @param percentiles An optional numeric vector specifying the percentiles for
#'   determining the breakpoints of the portfolios. This parameter is mutually
#'   exclusive with `n_portfolios`.
#' @param exchanges An optional character vector specifying exchange names to
#'   filter the data before computing breakpoints and assigning portfolios.
#'   Exchanges must be stored in a column named `exchange` in `data`. If `NULL`,
#'   no filtering is applied.
#' @param smooth_bunching An optional logical parameter specifying if to
#'   attempt smoothing non-extreme portfolios if the sorting variable bunches on
#'   the extremes (TRUE, the default), or not (FALSE). In some cases, smoothing
#'   will not result in equal-sized portfolios off the edges due to multiple
#'   clusters. If sufficiently large bunching is detected, `percentiles` is
#'   ignored and equally-spaced portfolios are returned for these cases with a
#'   warning.
#'
#' @return A vector of portfolio assignments for each row in the input `data`.
#'
#' @examples
#' data <- data.frame(
#'   id = 1:100,
#'   exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
#'   market_cap = 1:100
#' )
#' assign_portfolio(data, "market_cap", n_portfolios = 5)
#' assign_portfolio(data, "market_cap", percentiles = c(0.2, 0.4, 0.6, 0.8), breakpoint_exchanges = c("NYSE"))
#'
#' @export
#'
#' @note This function will stop and throw an error if both `n_portfolios` and
#'   `percentiles` are provided or if neither is provided. Ensure that you only
#'   use one of these parameters for specifying portfolio breakpoints.
assign_portfolio <- function(data,
                             sorting_variable,
                             n_portfolios = NULL,
                             percentiles = NULL,
                             breakpoint_exchanges = NULL,
                             smooth_bunching = TRUE) {

  if (!is.null(n_portfolios) && !is.null(percentiles)) {
    cli::cli_abort("Please provide either 'n_portfolios' or 'percentiles', not both.")
  } else if (is.null(n_portfolios) && is.null(percentiles)) {
    cli::cli_abort("You must provide either 'n_portfolios' or 'percentiles.'")
  }

  data_breakpoints <- data

  if (!is.null(breakpoint_exchanges)) {
    if (!("exchange" %in% colnames(data))) {
      cli::cli_abort("Please provide the column 'exchange' when filtering.")
    }
    data_breakpoints <- data_breakpoints |>
      filter(exchange %in% breakpoint_exchanges)
  }

  if (!is.null(n_portfolios)) {
    if (n_portfolios <= 1) {
      cli::cli_abort("Number of portfolios must be larger than 1.")
    } else {
      probs <- seq(0, 1, length.out = n_portfolios + 1)
    }
  } else {
    probs <- c(0, percentiles, 1)
    n_portfolios <- length(probs) - 1
  }

  sorting_values <- data_breakpoints[[sorting_variable]]

  # Exit condition for identical sorting variables
  if (length(unique(sorting_values)) == 1) {
    cli::cli_warn("The sorting variable is constant and only one portfolio is returned.")
    return(rep(1, nrow(data_breakpoints)))
  }

  breakpoints <- quantile(
    sorting_values, probs = probs, na.rm = TRUE, names = FALSE
  )

  if (smooth_bunching) {
    # Portfolio 1 and n are overpopulated
    if (breakpoints[1] == breakpoints[2] && breakpoints[n_portfolios] == breakpoints[n_portfolios + 1]) {
      if (!is.null(percentiles)) {
        cli::cli_warn("'smooth_bunching' is TRUE and equally-spaced portfolios are returned for non-edge portfolios.")
      }

      sorting_values_new <- sorting_values[which(sorting_values > breakpoints[1] & sorting_values < breakpoints[n_portfolios + 1])]

      probs_new <- seq(0, 1, length.out = n_portfolios - 1)

      breakpoints_new <- quantile(
        sorting_values_new, probs = probs_new, na.rm = TRUE, names = FALSE
      )

      breakpoints_new[n_portfolios - 1] <- breakpoints_new[n_portfolios - 1] + 1e-15

      breakpoints <- c(breakpoints[1], breakpoints_new, breakpoints[n_portfolios + 1])
    }

    # Portfolio 1 is overpopulated
    if (breakpoints[1] == breakpoints[2]) {
      if (!is.null(percentiles)) {
        cli::cli_warn("'smooth_bunching' is TRUE and equally-spaced portfolios are returned for non-edge portfolios.")
      }

      sorting_values_new <- sorting_values[which(sorting_values > breakpoints[1])]

      probs_new <- seq(0, 1, length.out = n_portfolios)

      breakpoints_new <- quantile(
        sorting_values_new, probs = probs_new, na.rm = TRUE, names = FALSE
      )

      breakpoints <- c(breakpoints[1], breakpoints_new)
    }

    # Portfolio n is overpopulated
    if (breakpoints[n_portfolios] == breakpoints[n_portfolios + 1]) {
      if (!is.null(percentiles)) {
        cli::cli_warn("'smooth_bunching' is TRUE and equally-spaced portfolios are returned for non-edge portfolios.")
      }

      sorting_values_new <- sorting_values[which(sorting_values < breakpoints[n_portfolios])]

      probs_new <- seq(0, 1, length.out = n_portfolios)

      breakpoints_new <- quantile(
        sorting_values_new, probs = probs_new, na.rm = TRUE, names = FALSE
      )

      breakpoints_new[n_portfolios] <- breakpoints_new[n_portfolios] + 1e-15

      breakpoints <- c(breakpoints_new, breakpoints[n_portfolios + 1])
    }
  }

  breakpoints[2:(n_portfolios + 1)] <- breakpoints[2:(n_portfolios + 1)] + 1e-20

  # Assign portfolios
  sorting_values_all <- data[[sorting_variable]]
  portfolio_indices <- findInterval(
    sorting_values_all, breakpoints, all.inside = TRUE
  )

  if (length(unique(na.omit(portfolio_indices)) )!= n_portfolios) {
    cli::cli_warn("The number of portfolios differs from the specified parameter due to clusters in the sorting variable.")
  }

  return(portfolio_indices)
}
