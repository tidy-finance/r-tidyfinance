#' Compute Breakpoints Based on Sorting Variable
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function computes breakpoints based on a specified sorting. It can
#' optionally filter the data by exchanges before computing the breakpoints. The
#' function requires either the number of portfolios to be created or specific
#' percentiles for the breakpoints, but not both.
#'
#' @param data A data frame containing the dataset for breakpoint computation.
#' @param sorting_variable A string specifying the column name in `data` to be
#'   used for determining breakpoints.
#' @param n_portfolios An optional integer specifying the number of equally
#'   sized portfolios to create. This parameter is mutually exclusive with
#'   `percentiles`.
#' @param percentiles An optional numeric vector specifying the percentiles for
#'   determining the breakpoints of the portfolios. This parameter is mutually
#'   exclusive with `n_portfolios`.
#' @param breakpoint_exchanges An optional character vector specifying exchange
#'   names to filter the data before computing breakpoints. Exchanges must be
#'   stored in a column named `exchange` in `data`. If `NULL`, no filtering is
#'   applied.
#'
#' @return A vector of breakpoints of the desired length.
#'
#' @examples
#' data <- data.frame(
#'   id = 1:100,
#'   exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
#'   market_cap = 1:100
#' )
#' compute_breakpoints(data, "market_cap", n_portfolios = 5)
#' compute_breakpoints(data, "market_cap", percentiles = c(0.2, 0.4, 0.6, 0.8), breakpoint_exchanges = c("NYSE"))
#'
#' @export
#'
#' @note This function will stop and throw an error if both `n_portfolios` and
#'   `percentiles` are provided or if neither is provided. Ensure that you only
#'   use one of these parameters.
compute_breakpoints <- function(data,
                                sorting_variable,
                                n_portfolios = NULL,
                                percentiles = NULL,
                                breakpoint_exchanges = NULL) {

  if (!is.null(n_portfolios) && !is.null(percentiles)) {
    cli::cli_abort("Please provide either 'n_portfolios' or 'percentiles', not both.")
  } else if (is.null(n_portfolios) && is.null(percentiles)) {
    cli::cli_abort("You must provide either 'n_portfolios' or 'percentiles.'")
  }

  if (!is.null(breakpoint_exchanges)) {
    if (!("exchange" %in% colnames(data))) {
      cli::cli_abort("Please provide the column 'exchange' when filtering.")
    }
    data <- data |>
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

  sorting_values <- data[[sorting_variable]]

  breakpoints <- quantile(
    sorting_values, probs = probs, na.rm = TRUE, names = FALSE
  )

  breakpoints[2:(n_portfolios + 1)] <- breakpoints[2:(n_portfolios + 1)] + 1e-20

  breakpoints
}

#' Compute Breakpoints Based on Sorting Variable (with Smoothing)
#'
#' This function computes breakpoints based on a specified sorting. It can
#' optionally filter the data by exchanges before computing the breakpoints. The
#' function requires either the number of portfolios to be created or specific
#' percentiles for the breakpoints, but not both. The function also handles
#' cases where the sorting variable clusters on the edges, by assigning all
#' extreme values to the edges and attempting to compute equally populated
#' breakpoints with the remaining values (i.e., smooth the breakpoints' size).
#'
#' @param data A data frame containing the dataset for breakpoint computation.
#' @param sorting_variable A string specifying the column name in `data` to be
#'   used for determining breakpoints.
#' @param n_portfolios An optional integer specifying the number of equally
#'   sized portfolios to create. This parameter is mutually exclusive with
#'   `percentiles`.
#' @param percentiles An optional numeric vector specifying the percentiles for
#'   determining the breakpoints of the portfolios. This parameter is mutually
#'   exclusive with `n_portfolios`.
#' @param breakpoint_exchanges An optional character vector specifying exchange
#'   names to filter the data before computing breakpoints. Exchanges must be
#'   stored in a column named `exchange` in `data`. If `NULL`, no filtering is
#'   applied.
#'
#' @return A vector of breakpoints of the desired length.
#'
#' @examples
#' data <- data.frame(
#'   id = 1:100,
#'   exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
#'   market_cap = 1:100
#' )
#' compute_breakpoints_smoothed(data, "market_cap", n_portfolios = 5)
#' compute_breakpoints_smoothed(data, "market_cap", percentiles = c(0.2, 0.4, 0.6, 0.8), breakpoint_exchanges = c("NYSE"))
#'
#' @export
#'
#' @note This function will stop and throw an error if both `n_portfolios` and
#'   `percentiles` are provided or if neither is provided. Ensure that you only
#'   use one of these parameters.
compute_breakpoints_smoothed <- function(data,
                                         sorting_variable,
                                         n_portfolios = NULL,
                                         percentiles = NULL,
                                         breakpoint_exchanges = NULL) {
  if (!is.null(n_portfolios) && !is.null(percentiles)) {
    cli::cli_abort("Please provide either 'n_portfolios' or 'percentiles', not both.")
  } else if (is.null(n_portfolios) && is.null(percentiles)) {
    cli::cli_abort("You must provide either 'n_portfolios' or 'percentiles.'")
  }

  if (!is.null(breakpoint_exchanges)) {
    if (!("exchange" %in% colnames(data))) {
      cli::cli_abort("Please provide the column 'exchange' when filtering.")
    }
    data <- data |>
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

  sorting_values <- data[[sorting_variable]]

  breakpoints <- quantile(
    sorting_values, probs = probs, na.rm = TRUE, names = FALSE
  )

  # Portfolio 1 and n are overpopulated
  if (breakpoints[1] == breakpoints[2] && breakpoints[n_portfolios] == breakpoints[n_portfolios + 1]) {
    if (!is.null(percentiles)) {
      cli::cli_warn("Equally-spaced portfolios are returned for non-edge portfolios.")
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
      cli::cli_warn("Equally-spaced portfolios are returned for non-edge portfolios.")
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
      cli::cli_warn("Equally-spaced portfolios are returned for non-edge portfolios.")
    }

    sorting_values_new <- sorting_values[which(sorting_values < breakpoints[n_portfolios])]

    probs_new <- seq(0, 1, length.out = n_portfolios)

    breakpoints_new <- quantile(
      sorting_values_new, probs = probs_new, na.rm = TRUE, names = FALSE
    )

    breakpoints_new[n_portfolios] <- breakpoints_new[n_portfolios] + 1e-15

    breakpoints <- c(breakpoints_new, breakpoints[n_portfolios + 1])
  }

  breakpoints[2:(n_portfolios + 1)] <- breakpoints[2:(n_portfolios + 1)] + 1e-20

  breakpoints
}
