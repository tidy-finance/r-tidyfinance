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
#' @return A numeric vector with the extreme values replaced by the
#'   corresponding quantile values.
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(100)
#' winsorized_data <- winsorize(data, 0.05)
#'
#' @importFrom stats quantile
#'
#' @export
winsorize <- function(x, cut) {
  if (cut < 0 || cut > 0.5) {
    stop("The parameter 'cut' must be inside [0, 0.5].")
  }

  lb <- stats::quantile(x, cut, na.rm = TRUE)
  up <- stats::quantile(x, 1 - cut, na.rm = TRUE)
  x <- replace(x, x > up, up)
  x <- replace(x, x < lb, lb)
  x
}
