#' Winsorize a Numeric Vector
#'
#' Replaces the values in a numeric vector that are beyond the specified
#' quantiles with the boundary values of those quantiles. This is done for both
#' tails of the distribution based on the `cut` parameter.
#'
#' @param x A numeric vector to be winsorized.
#' @param cut The proportion of data to be winsorized from both ends of the
#'   distribution. For example, a `cut` of 0.05 will winsorize the lowest and
#'   highest 5% of the data. Must be inside \[0, 0.5\].
#'
#' @returns A numeric vector with the extreme values replaced by the
#'   corresponding quantile values.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(100)
#' winsorized_data <- winsorize(data, 0.05)
#'
winsorize <- function(x, cut) {
  if (cut < 0 || cut > 0.5) {
    cli::cli_abort("{.arg cut} must be inside [0, 0.5].")
  }

  quantiles <- quantile(x, probs = c(cut, 1 - cut), na.rm = TRUE)
  lb <- quantiles[1]
  ub <- quantiles[2]
  x <- replace(x, x > ub, ub)
  x <- replace(x, x < lb, lb)
  x
}
