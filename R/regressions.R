#' Winsorize a Numeric Vector
#'
#' Replaces the values in a numeric vector that are beyond the specified quantiles
#' with the boundary values of those quantiles. This is done for both tails of the
#' distribution based on the `cut` parameter.
#'
#' @param x A numeric vector to be winsorized.
#' @param cut The proportion of data to be winsorized from both ends of the distribution.
#'   For example, a `cut` of 0.05 will winsorize the lowest and highest 5% of the data.
#' @return A numeric vector with the extreme values replaced by the corresponding
#'   quantile values.
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(100)
#' winsorized_data <- winsorize(data, 0.05)
#'
#' @export
#'
#' @importFrom stats quantile
winsorize <- function(x, cut) {
  x <- replace(
    x,
    x > quantile(x, 1 - cut, na.rm = T),
    quantile(x, 1 - cut, na.rm = T)
  )
  x <- replace(
    x,
    x < quantile(x, cut, na.rm = T),
    quantile(x, cut, na.rm = T)
  )

  x
}

#' Trim a Numeric Vector
#'
#' Removes the values in a numeric vector that are beyond the specified quantiles,
#' effectively trimming the distribution based on the `cut` parameter. This process
#' reduces the length of the vector, excluding extreme values from both tails of the
#' distribution.
#'
#' @param x A numeric vector to be trimmed.
#' @param cut The proportion of data to be trimmed from both ends of the distribution.
#'   For example, a `cut` of 0.05 will remove the lowest and highest 5% of the data.
#' @return A numeric vector with the extreme values removed.
#' @examples
#' set.seed(123)
#' data <- rnorm(100)
#' trimmed_data <- trim(x = data, cut = 0.05)
#' @export
#'
trim <- function(x, cut) {
  lb <- quantile(x, cut, na.rm = TRUE)
  up <- quantile(x, 1 - cut, na.rm = TRUE)

  x_trimmed <- x[x > lb & x < up]

  x_trimmed
}

```r
#' Create Custom Summary Statistics with Optional Grouping
#'
#' Computes and returns custom summary statistics for specified columns in a data frame,
#' based on the options provided. Allows for grouping by one or more variables.
#'
#' @param data A data frame containing the data to summarize.
#' @param stats A list specifying the summary statistics to compute for each column specified in `...`.
#'   Supported statistics include "mean", "sd", "min", "max", "n", and "quantile" (with probabilities).
#' @param .by Optional grouping variables for calculating grouped summary statistics.
#' @param ... Columns for which summary statistics are to be calculated.
#' @return A data frame containing the requested summary statistics, optionally grouped.
#' @examples
#' data <- data.frame(
#'   group = rep(1:2, each = 50),
#'   value1 = rnorm(100),
#'   value2 = runif(100)
#' )
#' summary_stats <- create_summary(data, list(mean = TRUE, sd = TRUE, quantile = c(0.25, 0.75)), .by = group, value1, value2)
#' @export
#'
#' @import dplyr
#' @importFrom rlang ensym enquo quo
create_summary <- function(data, stats, .by = NULL, ...) {
  summarize_exprs <- list()
  columns <- enquos(...)

  if (!is.null(stats$mean) && stats$mean) {
    for (col in columns) {
      summarize_exprs[[paste0(rlang::as_string(col), "_mean")]] <- ~mean(.data[[rlang::as_string(col)]], na.rm = TRUE)
    }
  }
  if (!is.null(stats$sd) && stats$sd) {
    for (col in columns) {
      summarize_exprs[[paste0(rlang::as_string(col), "_sd")]] <- ~sd(.data[[rlang::as_string(col)]], na.rm = TRUE)
    }
  }
  if (!is.null(stats$min) && stats$min) {
    for (col in columns) {
      summarize_exprs[[paste0(rlang::as_string(col), "_min")]] <- ~min(.data[[rlang::as_string(col)]], na.rm = TRUE)
    }
  }
  if (!is.null(stats$max) && stats$max) {
    for (col in columns) {
      summarize_exprs[[paste0(rlang::as_string(col), "_max")]] <- ~max(.data[[rlang::as_string(col)]], na.rm = TRUE)
    }
  }
  if (!is.null(stats$n) && stats$n) {
    summarize_exprs$n <- ~dplyr::n()
  }
  if (!is.null(stats$quantile)) {
    probs <- stats$quantile
    for (col in columns) {
      for (prob in probs) {
        prob_name <- paste0(rlang::as_string(col), "_q", prob * 100)
        summarize_exprs[[prob_name]] <- ~quantile(.data[[rlang::as_string(col)]], prob, na.rm = TRUE)
      }
    }
  }

  if (!is.null(.by)) {
    data %>%
      dplyr::group_by(!!!enquos(.by)) %>%
      dplyr::summarize(!!!summarize_exprs, .groups = 'drop')
  } else {
    data %>%
      dplyr::summarize(!!!summarize_exprs, .groups = 'drop')
  }
}
