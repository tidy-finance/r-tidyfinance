#' Compute Breakpoints Based on Sorting Variable
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Computes breakpoints based on a specified sorting. It can optionally filter
#' the data by exchanges or lagged size quantiles before computing the
#' breakpoints. The function requires either the number of portfolios to be
#' created or specific percentiles for the breakpoints, but not both. The
#' function also optionally handles cases where the sorting variable clusters
#' on the edges, by assigning all extreme values to the edges and attempting
#' to compute equally populated breakpoints with the remaining values.
#'
#' @param data A data frame containing the dataset for breakpoint computation.
#' @param sorting_variable A character string specifying the column name in
#'   `data` to be used for determining breakpoints.
#' @param breakpoint_options A named list of [breakpoint_options()] for the
#'   breakpoints. The arguments include
#'   \itemize{
#'     \item `n_portfolios` An optional integer specifying the number of
#'       equally sized portfolios to create. This parameter is mutually
#'       exclusive with `percentiles`.
#'     \item `percentiles` An optional numeric vector specifying the
#'       percentiles for determining the breakpoints of the portfolios.
#'       This parameter is mutually exclusive with `n_portfolios`.
#'     \item `breakpoints_exchanges` An optional character vector specifying
#'       exchange names to filter the data before computing breakpoints.
#'       Exchanges must be stored in a column given by `data_options` (defaults
#'       to `exchange`). If `NULL`, no filtering is applied.
#'     \item `smooth_bunching` An optional logical parameter specifying if
#'       to attempt smoothing non-extreme portfolios if the sorting variable
#'       bunches on the extremes (`TRUE`), or not (`FALSE`, the default).
#'       In some cases, smoothing will not result in equal-sized portfolios
#'       off the edges due to multiple clusters. If sufficiently large
#'       bunching is detected, `percentiles` is ignored and equally-spaced
#'       portfolios are returned for these cases with a warning.
#'     \item `breakpoints_min_size_threshold` An optional numeric value between
#'       0 and 1 (exclusive). When set, stocks with market capitalization below
#'       this quantile are excluded from breakpoint computation. The quantile
#'       is computed among `breakpoints_exchanges` stocks if specified,
#'       otherwise among all stocks. Requires a market capitalization column
#'       in the data (column name determined by `data_options`).
#'   }
#' @param data_options A list of class `tidyfinance_data_options` (created via
#'   [data_options()]) specifying column name mappings. The `exchange` element
#'   is used to specify the exchange column, and `mktcap_lag` is used to
#'   specify the market capitalization. Uses [data_options()] default if
#'   `NULL`: `"exchange" = "exchange"` and `"mktcap_lag" = "mktcap_lag"`.
#'
#' @note This function will stop and throw an error if both `n_portfolios` and
#'   `percentiles` are provided or missing simultaneously.
#'
#' @returns A numeric vector of breakpoints of the desired length.
#'
#' @family portfolio functions
#' @export
#'
#' @examples
#' set.seed(42)
#' data <- data.frame(
#'   id = 1:100,
#'   exchange = sample(c("NYSE", "NASDAQ"), 100, replace = TRUE),
#'   market_cap = 1:100
#' )
#'
#' compute_breakpoints(data, "market_cap", breakpoint_options(n_portfolios = 5))
#' compute_breakpoints(
#'   data,
#'   "market_cap",
#'   breakpoint_options(
#'     percentiles = c(0.2, 0.4, 0.6, 0.8),
#'     breakpoints_exchanges = c("NYSE")
#'   )
#'  )
#'
compute_breakpoints <- function(
  data,
  sorting_variable,
  breakpoint_options,
  data_options = NULL
) {
  if (!is.list(breakpoint_options)) {
    cli::cli_abort("Please provide a named list with breakpoint options.")
  }

  n_portfolios <- breakpoint_options$n_portfolios
  percentiles <- breakpoint_options$percentiles
  breakpoints_exchanges <- breakpoint_options$breakpoints_exchanges
  smooth_bunching <- breakpoint_options$smooth_bunching
  breakpoints_min_size_threshold <-
    breakpoint_options$breakpoints_min_size_threshold

  if (is.null(data_options)) {
    data_options <- data_options()
  }

  if (!is.null(n_portfolios) && !is.null(percentiles)) {
    cli::cli_abort(
      paste(
        "Please provide either {.arg n_portfolios}",
        "or {.arg percentiles}, not both."
      )
    )
  } else if (is.null(n_portfolios) && is.null(percentiles)) {
    cli::cli_abort(
      "You must provide either {.arg n_portfolios} or {.arg percentiles}"
    )
  }

  # Extract only the sorting column, filter as a vector
  sorting_values <- data[[sorting_variable]]

  if (!is.null(breakpoints_exchanges)) {
    exchange_col <- data_options$exchange
    if (!(exchange_col %in% colnames(data))) {
      cli::cli_abort(
        paste(
          "Please provide the column {exchange_col}",
          "when filtering using {.arg breakpoints_exchanges}."
        )
      )
    }
    keep <- data[[exchange_col]] %in% breakpoints_exchanges
    sorting_values <- sorting_values[keep]
  }

  if (!is.null(breakpoints_min_size_threshold)) {
    mktcap_col <- data_options$mktcap_lag
    if (!(mktcap_col %in% colnames(data))) {
      cli::cli_abort(
        paste(
          "Column {.val {mktcap_col}} is required",
          "when using {.arg breakpoints_min_size_threshold}."
        )
      )
    }
    if (!is.null(breakpoints_exchanges)) {
      mktcap_ref <- data[[mktcap_col]][keep]
    } else {
      mktcap_ref <- data[[mktcap_col]]
    }
    size_cutoff <- quantile(
      mktcap_ref,
      breakpoints_min_size_threshold,
      na.rm = TRUE
    )
    above_size <- !is.na(data[[mktcap_col]]) & data[[mktcap_col]] > size_cutoff
    if (!is.null(breakpoints_exchanges)) {
      sorting_values <- data[[sorting_variable]][keep & above_size]
    } else {
      sorting_values <- data[[sorting_variable]][above_size]
    }
  }

  if (length(sorting_values) == 0L) {
    cli::cli_warn(
      paste(
        "No breakpoints were calculated, likely due to an insufficient number",
        "of observations after filtering for breakpoint exchanges."
      )
    )
    return(NA_real_)
  }

  if (!is.null(n_portfolios)) {
    if (n_portfolios <= 1L) {
      cli::cli_abort("{.arg n_portfolios} must be larger than 1.")
    }
    probs <- seq(0, 1, length.out = n_portfolios + 1L)
  } else {
    probs <- c(0, percentiles, 1)
    n_portfolios <- length(probs) - 1L
  }

  breakpoints <- quantile(
    sorting_values,
    probs = probs,
    na.rm = TRUE,
    names = FALSE
  )

  if (isTRUE(smooth_bunching)) {
    both_edges <- breakpoints[1] == breakpoints[2] &&
      breakpoints[n_portfolios] == breakpoints[n_portfolios + 1]
    lower_edge <- breakpoints[1] == breakpoints[2]
    upper_edge <- breakpoints[n_portfolios] == breakpoints[n_portfolios + 1]

    if (both_edges) {
      if (!is.null(percentiles)) {
        cli::cli_warn(
          paste(
            "{.arg smooth_bunching} is TRUE and equally-spaced portfolios",
            "are returned for non-edge portfolios."
          )
        )
      }
      mask <- sorting_values > breakpoints[1] &
        sorting_values < breakpoints[n_portfolios + 1]
      sorting_values_new <- sorting_values[mask]
      probs_new <- seq(0, 1, length.out = n_portfolios - 1L)
      breakpoints_new <- quantile(
        sorting_values_new,
        probs = probs_new,
        na.rm = TRUE,
        names = FALSE
      )
      breakpoints_new[n_portfolios - 1L] <- breakpoints_new[n_portfolios - 1L] +
        1e-15
      breakpoints <- c(
        breakpoints[1],
        breakpoints_new,
        breakpoints[n_portfolios + 1]
      )
    } else if (lower_edge) {
      if (!is.null(percentiles)) {
        cli::cli_warn(
          paste(
            "{.arg smooth_bunching} is TRUE and equally-spaced portfolios",
            "are returned for non-edge portfolios."
          )
        )
      }

      sorting_values_new <- sorting_values[sorting_values > breakpoints[1]]
      probs_new <- seq(0, 1, length.out = n_portfolios)
      breakpoints_new <- quantile(
        sorting_values_new,
        probs = probs_new,
        na.rm = TRUE,
        names = FALSE
      )
      breakpoints <- c(breakpoints[1], breakpoints_new)
    } else if (upper_edge) {
      if (!is.null(percentiles)) {
        cli::cli_warn(
          paste(
            "{.arg smooth_bunching} is TRUE and equally-spaced portfolios",
            "are returned for non-edge portfolios."
          )
        )
      }
      sorting_values_new <- sorting_values[
        sorting_values < breakpoints[n_portfolios]
      ]
      probs_new <- seq(0, 1, length.out = n_portfolios)
      breakpoints_new <- quantile(
        sorting_values_new,
        probs = probs_new,
        na.rm = TRUE,
        names = FALSE
      )
      breakpoints_new[n_portfolios] <- breakpoints_new[n_portfolios] + 1e-15
      breakpoints <- c(breakpoints_new, breakpoints[n_portfolios + 1])
    }
  }

  breakpoints[2:(n_portfolios + 1L)] <- breakpoints[2:(n_portfolios + 1L)] +
    1e-20
  breakpoints
}
