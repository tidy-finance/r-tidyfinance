#' Compute a rolling standard deviation by period
#'
#' @description
#' Calculates a rolling standard deviation of a specified column over a given
#' number of time periods (e.g., quarters, months, years). Windows with fewer
#' than `min_obs` non-missing observations return `NA_real_`.
#'
#' @param data A data frame with a `date` column of class `Date`.
#' @param column A column name (unquoted) to compute the rolling standard deviation on.
#' @param period A string specifying the period for rolling windows.
#'   Common options include `"quarter"`, `"month"`, or `"year"`.
#' @param periods Number of periods to include in the rolling window.
#' @param min_obs Minimum number of non-missing observations required to compute the statistic.
#'
#' @return A numeric vector of rolling standard deviations, aligned with `data$date`.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(slider)
#'
#' df <- tibble(
#'   date = seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 24),
#'   value = rnorm(24)
#' )
#'
#' df |> mutate(rolling_sd = compute_rolling_sd(df, value, period = "quarter", periods = 4, min_obs = 2))
compute_rolling_sd <- function(
  data,
  column,
  period = "quarter",
  periods = 4,
  min_obs = 2
) {
  stopifnot("date" %in% names(data))
  stopifnot(inherits(data$date, "Date"))
  stopifnot(is.character(period), length(period) == 1)

  x <- data |> dplyr::pull({{ column }})

  slider::slide_period_vec(
    .x = x,
    .i = data$date,
    .period = period,
    .f = ~ {
      if (sum(!is.na(.x)) < min_obs) NA_real_ else stats::sd(.x, na.rm = TRUE)
    },
    .before = periods - 1,
    .complete = FALSE
  )
}
