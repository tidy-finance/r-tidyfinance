#' Lag a Column Based on Date and Time Range
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function generates a lagged version of a given column based on a date variable, with the
#' ability to specify a range of lags. It also allows for the optional removal of `NA` values.
#'
#' @param column A numeric vector or column to be lagged.
#' @param date A vector representing dates corresponding to the `column`. This should be in a
#'  date or datetime format.
#' @param lag An integer specifying the minimum lag (in days, hours, etc.) to apply to `column`.
#' @param max_lag An integer specifying the maximum lag (in days, hours, etc.) to apply to `column`.
#'  Defaults to `lag`.
#' @param drop_na A logical value indicating whether to drop `NA` values from the resulting lagged
#'  column. Defaults to `TRUE`.
#'
#' @return A vector of the same length as `column`, containing the lagged values.
#'  If no matching dates are found within the lag window, `NA` is returned for that position.
#'
#' @export
#'
#' @examples
#' # Basic example with a vector
#' dates <- as.Date("2023-01-01") + 0:9
#' values <- rnorm(10)
#' lagged_values <- lag_column(values, dates, lag = 1, max_lag = 3)
#'
#' # Example using a tibble and dplyr::group_by
#' data <- tibble::tibble(
#'   permno = rep(1:2, each = 10),
#'   date = rep(seq.Date(as.Date('2023-01-01'), by = "month", length.out = 10), 2),
#'   size = runif(20, 100, 200),
#'   bm = runif(20, 0.5, 1.5)
#' )
#'
#' data |>
#'   dplyr::group_by(permno) |>
#'   dplyr::mutate(
#'     across(c(size, bm),
#'            \(x) lag_column(x, date, months(3), months(6), drop_na = TRUE))
#'   ) |>
#' dplyr::ungroup()
#'
lag_column <- function(
  column,
  date,
  lag,
  max_lag = lag,
  drop_na = TRUE
) {
  if (lag < 0 || max_lag < lag) {
    cli::cli_abort(
      "{.arg lag} and {.arg max_lag} must be non-negative and {.arg max_lag} must be greater than or equal to {.arg lag}"
    )
  }

  tmp_data <- tibble(date = date, value = column) |>
    mutate(lower_bound = date + lag, upper_bound = date + max_lag) |>
    select(contains("bound"), value)

  if (drop_na) {
    tmp_data <- tmp_data[!is.na(tmp_data$value), ]
  }

  tibble(date = date) |>
    left_join(
      tmp_data,
      join_by(closest(date >= lower_bound), date <= upper_bound)
    ) |>
    pull(value)
}
