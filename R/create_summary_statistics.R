#' Create Summary Statistics for Specified Variables
#'
#' Computes a set of summary statistics for numeric and integer variables in a
#' data frame. This function allows users to select specific variables for
#' summarization and can calculate statistics for the whole dataset or within
#' groups specified by the `by` argument. Additional detail levels for quantiles
#' can be included.
#'
#' @details The function first checks that all specified variables are of type
#' numeric, integer, or logical. If any variables do not meet this criterion,
#' the function stops and returns an error message indicating the non-conforming
#' variables.
#'
#' The basic set of summary statistics includes the count of non-NA values (n),
#' mean, standard deviation (sd), minimum (min), median (q50), and maximum
#' (max). If `detail` is TRUE, the function also computes the 1st, 5th, 10th,
#' 25th, 75th, 90th, 95th, and 99th percentiles.
#'
#' Summary statistics are computed for each variable specified in `...`. If a
#' `by` variable is provided, statistics are computed within each level of the
#' `by` variable.
#'
#' @param data A data frame containing the variables to be summarized.
#' @param ... Comma-separated list of unquoted variable names in the data frame
#'   to summarize. These variables must be either numeric, integer, or logical.
#' @param by An optional unquoted variable name to group the data before
#'   summarizing. If `NULL` (the default), summary statistics are computed across
#'   all observations.
#' @param detail A logical flag indicating whether to compute detailed summary
#'   statistics including additional quantiles. Defaults to FALSE, which
#'   computes basic statistics (n, mean, sd, min, median, max). When TRUE,
#'   additional quantiles (1%, 5%, 10%, 25%, 75%, 90%, 95%, 99%) are computed.
#' @param drop_na A logical flag indicating whether to drop missing values for
#'   each variabl (default is FALSE).
#'
#' @returns A tibble with summary statistics for each selected variable. If `by`
#'   is specified, the output includes the grouping variable as well. Each row
#'   represents a variable (and a group if `by` is used), and columns include
#'   the computed statistics.
#'
#' @export
create_summary_statistics <- function(
    data, ..., by = NULL, detail = FALSE, drop_na = FALSE
  ) {
  by <- enquo(by)
  # Check that all variables to summarize are numeric or integer
  col_types <- data |>
    select(...) |>
    sapply(class)

  if (sum(col_types %in% c("numeric", "integer", "logical")) < length(col_types)) {
    cli::cli_abort(sprintf(
      "The following columns are neither numeric nor integer: %s",
      toString(names(col_types[!col_types %in% c("numeric", "integer", "logical")]))
    ))
  }

  # Determine set of summary statistics to compute
  if (!detail) {
    funs <- list(
      n = function(x) sum(!is.na(x)),
      mean = mean, sd = stats::sd,
      min = min,
      q50 = partial(quantile_na_handler, probs = 0.50),
      max = max
    )
  } else {
    funs <- list(
      n = function(x) sum(!is.na(x)),
      mean = mean, sd = stats::sd,
      min = min,
      q01 = partial(quantile_na_handler, probs = 0.01),
      q05 = partial(quantile_na_handler, probs = 0.05),
      q10 = partial(quantile_na_handler, probs = 0.10),
      q25 = partial(quantile_na_handler, probs = 0.25),
      q50 = partial(quantile_na_handler, probs = 0.50),
      q75 = partial(quantile_na_handler, probs = 0.75),
      q90 = partial(quantile_na_handler, probs = 0.90),
      q95 = partial(quantile_na_handler, probs = 0.95),
      q99 = partial(quantile_na_handler, probs = 0.99),
      max = max
    )
  }

  if (rlang::quo_is_null(by)) {
    # Summarize across all observations if no "by" column is specified
    data_long <- data |>
      select(...) |>
      tidyr::pivot_longer(cols = everything(), names_to = "variable")

    if (drop_na) {
      data_long <- tidyr::drop_na(data_long)
    }

    data_long |>
      group_by(.data$variable) |>
      summarize(across(everything(), funs, .names =  "{.fn}"),
                .groups = "drop")
  } else {
    # Summarize by group column if "by" column is specified
    data_long <- data |>
      select(!!by, ...) |>
      tidyr::pivot_longer(cols = -!!by, names_to = "variable")

    if (drop_na) {
      data_long <- tidyr::drop_na(data_long)
    }

    data_long |>
      group_by(.data$variable, !!by) |>
      summarize(across(everything(), funs, .names =  "{.fn}"),
                .groups = "drop")
  }
}

quantile_na_handler <- function(x, probs) {
  if (anyNA(x)) {
    NA_real_
  } else {
    unname(quantile(x, probs = probs))
  }
}
