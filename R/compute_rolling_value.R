#' Compute a rolling value by period
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Applies an arbitrary summary function over rolling time-period windows.
#' Each window is defined by `period` and `periods`. The function `.f`
#' receives the complete-cases subset of each window and must return a
#' single scalar. Windows with fewer than `min_obs` complete rows
#' return `NA_real_`.
#'
#' @param data A data frame with a `date` column of class `Date`.
#' @param .f A function applied to each window. Receives a data-frame
#'   slice (complete cases only) and must return a single scalar value.
#' @param period A string specifying the period for rolling windows
#'   (e.g., `"month"`, `"quarter"`, `"year"`).
#' @param periods Number of periods to include in the rolling window.
#' @param min_obs Minimum number of non-missing rows required per window.
#'   Defaults to `periods`.
#'
#' @return A numeric vector aligned with the rows of `data`.
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' # Rolling standard deviation
#' set.seed(42)
#' df <- tibble(
#'   date = seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 24),
#'   value = rnorm(24)
#' )
#'
#' df |>
#'   mutate(
#'     rolling_sd = compute_rolling_value(
#'       pick(everything()),
#'       .f = ~ sd(.x$value, na.rm = TRUE),
#'       period = "month",
#'       periods = 4,
#'       min_obs = 2
#'     )
#'   )
#'
#' # Rolling last residual from a regression
#' set.seed(42)
#' df_reg <- tibble(
#'   date = seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 60),
#'   ret_excess = rnorm(60, 0, 0.05),
#'   mkt_excess = rnorm(60, 0, 0.04),
#'   smb = rnorm(60, 0, 0.03),
#'   hml = rnorm(60, 0, 0.03)
#' )
#'
#' df_reg |>
#'   mutate(
#'     residual = compute_rolling_value(
#'       pick(everything()),
#'       .f = ~ last(lm(ret_excess ~ mkt_excess + smb + hml, data = .x)$residuals),
#'       period = "month",
#'       periods = 24,
#'       min_obs = 12
#'     )
#'   )
#'
#' # Rolling cumulative-return-to-SD ratio
#' set.seed(42)
#' df_resid <- tibble(
#'   date = seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 24),
#'   int_roll_residual = rnorm(24, 0, 0.02)
#' )
#'
#' df_resid |>
#'   mutate(
#'     return_to_sd = compute_rolling_value(
#'       pick(everything()),
#'       .f = ~ (prod(1 + .x$int_roll_residual) - 1) / sd(.x$int_roll_residual),
#'       period = "month",
#'       periods = 12,
#'       min_obs = 12
#'     )
#'   )
#'
compute_rolling_value <- function(
  data,
  .f,
  period = "month",
  periods = 12,
  min_obs = periods
) {
  stopifnot("date" %in% names(data))
  stopifnot(inherits(data$date, "Date"))
  stopifnot(is.character(period), length(period) == 1)

  .f <- rlang::as_function(.f)

  slider::slide_period_vec(
    .x = data,
    .i = data$date,
    .period = period,
    .f = function(.x) {
      complete <- tidyr::drop_na(.x)
      if (nrow(complete) < min_obs) {
        NA_real_
      } else {
        .f(complete)
      }
    },
    .before = periods - 1,
    .complete = FALSE
  )
}
