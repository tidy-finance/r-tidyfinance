#' Trim a Numeric Vector
#'
#' Sets the values in a numeric vector that are beyond the specified quantiles
#' to `NA`, based on the `cut` parameter. Values below the `cut` quantile and
#' above the `1 - cut` quantile become `NA`; the length of `x` is unchanged,
#' which is the useful behaviour inside `dplyr::mutate()`.
#'
#' @param x A numeric vector to be trimmed.
#' @param cut The proportion of data to be trimmed from both ends of the
#'   distribution. For example, a `cut` of 0.05 will remove the lowest and
#'   highest 5% of the data. Must be in \[0, 0.5\].
#'
#' @returns A numeric vector the same length as `x`, with the values outside
#'   the `cut` and `1 - cut` quantiles set to `NA`.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(100)
#' trimmed_data <- trim(x = data, cut = 0.05)
#'
trim <- function(x, cut) {
  if (cut < 0 || cut > 0.5) {
    cli::cli_abort("{.arg cut} must be inside [0, 0.5].")
  }

  lb <- quantile(x, cut, na.rm = TRUE)
  up <- quantile(x, 1 - cut, na.rm = TRUE)
  x <- replace(x, x > up, NA_real_)
  x <- replace(x, x < lb, NA_real_)
  x
}
