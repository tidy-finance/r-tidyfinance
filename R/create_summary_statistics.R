#' Create Summary Statistics for Specified Variables
#'
#' Computes a set of summary statistics for numeric and integer variables in a
#' data frame. This function allows users to select specific variables for
#' summarization and can calculate statistics for the whole dataset or within
#' groups specified by the `by` argument. Additional detail levels for quantiles
#' can be included.
#'
#' @param data A data frame containing the variables to be summarized.
#' @param ... Comma-separated list of unquoted variable names in the data frame
#'   to summarize. These variables must be either numeric, integer, or logical.
#' @param by An optional unquoted variable name to group the data before
#'   summarizing. If NULL (the default), summary statistics are computed across
#'   all observations.
#' @param detail A logical flag indicating whether to compute detailed summary
#'   statistics including additional quantiles. Defaults to FALSE, which
#'   computes basic statistics (n, mean, sd, min, median, max). When TRUE,
#'   additional quantiles (1%, 5%, 10%, 25%, 75%, 90%, 95%, 99%) are computed.
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
#' @return A tibble with summary statistics for each selected variable. If `by`
#'   is specified, the output includes the grouping variable as well. Each row
#'   represents a variable (and a group if `by` is used), and columns include
#'   the computed statistics.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom purrr partial
#'
#' @export
create_summary_statistics <- function(data, ..., by = NULL, detail = FALSE) {
  # Check that all variables to summarize are numeric or integer
  col_types <- data |>
    select(...) |>
    sapply(class)

  if(sum(col_types %in% c("numeric", "integer", "logical")) < length(col_types)) {
    stop(paste0("The following columns are neither numeric nor integer: ",
                paste(names(col_types[!col_types %in% c("numeric", "integer", "logical")]), collapse = ", ")))
  }

  # Determine set of summary statistics to compute
  if (detail == FALSE) {
    funs <- list(
      n = function(x) {sum(!is.na(x))},
      mean = mean, sd = stats::sd,
      min = min,
      q50 = partial(stats::quantile, probs = 0.50),
      max = max
    )
  } else {
    funs <- list(
      n = function(x) {sum(!is.na(x))},
      mean = mean, sd = stats::sd,
      min = min,
      q01 = partial(stats::quantile, probs = 0.01),
      q05 = partial(stats::quantile, probs = 0.05),
      q10 = partial(stats::quantile, probs = 0.10),
      q25 = partial(stats::quantile, probs = 0.25),
      q50 = partial(stats::quantile, probs = 0.50),
      q75 = partial(stats::quantile, probs = 0.75),
      q90 = partial(stats::quantile, probs = 0.90),
      q95 = partial(stats::quantile, probs = 0.95),
      q99 = partial(stats::quantile, probs = 0.99),
      max = max
    )
  }

  if (missing(by)) {
    # Summarize across all observations if no "by" column is specified
    data |>
      select(...) |>
      tidyr::pivot_longer(cols = everything(), names_to = "variable") |>
      stats::na.omit() |>
      group_by(.data$variable) |>
      summarize(across(everything(), funs),
                .groups = "drop")
  } else {
    # Summarize by group column if "by" column is specified
    data |>
      select({{ by }}, ...) |>
      tidyr::pivot_longer(cols = -{{ by }}, names_to = "variable") |>
      stats::na.omit() |>
      group_by(.data$variable, {{ by }}) |>
      summarize(across(everything(), funs),
                .groups = "drop")
  }
}
