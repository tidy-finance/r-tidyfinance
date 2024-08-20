#' Compute a Lagged Version of a Time Series Based on Date Ranges
#'
#' This function computes a lagged version of a time series column based on a flexible date range.
#' The lag is defined by lower and upper date bounds rather than by a fixed number of observations.
#'
#' @param column A numeric vector representing the time series values that need to be lagged.
#' @param date_id A Date or POSIXct vector representing the dates corresponding to `column`.
#' @param lag A period (such as `months(3)`) representing the lower bound of the lag window.
#' @param max_lag A period representing the upper bound of the lag window. Must be greater than or equal to `lag`.
#' @param na.rm Logical; if `TRUE`, `NA` values in the `column` are removed before computation.
#'
#' @return A numeric vector with the same length as `column`, containing the lagged values.
#' If no matching dates are found within the lag window, `NA` is returned for that position.
#'
#' @details
#' The function works by creating a flexible date range for each date in `date_id`, defined by adding the `lag` and `max_lag` periods to the original date.
#' It then finds the closest matching date within this range and returns the corresponding value from `column`.
#' If there are multiple matches, the function selects the value from the closest date.
#'
#' @examples
#' data <- data.frame(
#' date = seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 10),
#' permno = rep(1:5, each = 10),
#' size = runif(10, 50, 100),
#' bm = runif(10, 0.1, 1.5)
#' )
#' data |> dplyr::group_by(permno) |>
#'   dplyr::mutate(across(c(size, bm), \(x) compute_ts_lag(x, date, months(3), months(6), na.rm = TRUE)))
#'
#' @export


compute_ts_lag <- function(column, date_id,
                           lag, max_lag,
                           na.rm = TRUE) {

  # Ensure that lag and max_lag are non-negative and max_lag >= lag
  if (lag < 0 || max_lag < lag) {
    cli::cli_abort("{.arg lag} and {.arg max_lag} must be non-negative and {.arg max_lag} must be greater than or equal to {.arg lag}")
  }

  tmp_data <- data.frame(date = date_id, value = column) |>
    mutate(lower_bound = date + lag,
           upper_bound = date + max_lag) |>
    select(contains("bound"), value)

  if(na.rm){tmp_data <- tmp_data[!is.na(tmp_data$value),]}

  data.frame(date=date_id) |>
    left_join(tmp_data,
              join_by(closest(date>=lower_bound), date<=upper_bound)) |>
    pull(value)
}



data %>%
  group_by(permno) |>
  mutate(across(c(size, bm), \(x)compute_ts_lag(x, date, months(3), months(6), na.rm = TRUE))) |>
  print(n=50)
