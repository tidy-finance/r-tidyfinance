#' Download Risk-Free Rate Data
#'
#' Downloads and processes risk-free rate data from FRED. For monthly
#' data, splices the 3-Month Treasury Bill Secondary Market Rate
#' (TB3MS, pre-2001) with the 4-Week Treasury Bill Secondary Market
#' Rate (DTB4WK, from 2001 onwards). For daily data, uses the 3-Month
#' Treasury Bill Secondary Market Rate (DTB3) forward-filled to cover
#' weekends and holidays.
#'
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If
#'   not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the end date for the data. If not
#'   provided, the full dataset is returned.
#' @param frequency A character string, either `"monthly"` (default)
#'   or `"daily"`, specifying the frequency of the returned data.
#'
#' @details
#' For monthly rates, the function splices two FRED series at
#' 2001-01-01:
#' - **Pre-2001**: TB3MS (3-month T-bill, monthly). The annualised
#'   discount rate `d` is converted to a monthly holding-period return
#'   via `(1 + d * 90/360 / (1 - d * 90/360))^(1/3) - 1`.
#' - **From 2001**: DTB4WK (4-week T-bill, daily). The last non-NA
#'   observation per calendar month is taken and the annualised
#'   discount rate `d` is converted via
#'   `(1 + d * 28/360 / (1 - d * 28/360))^(365/28/12) - 1`.
#'
#' For daily rates, DTB3 (3-month T-bill, daily) is forward-filled and
#' the annualised discount rate `d` is converted to a daily
#' holding-period return via
#' `(1 + d * 90/360 / (1 - d * 90/360))^(1/63) - 1`,
#' where 63 is the approximate number of trading days per quarter.
#'
#' @returns A tibble with two columns:
#' \describe{
#'   \item{date}{The date of the observation.}
#'   \item{risk_free}{The risk-free rate for the period.}
#' }
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_rf("2020-01-01", "2020-12-31")
#'   download_data_rf(
#'     "2020-01-01", "2020-12-31", frequency = "daily"
#'   )
#' }
download_data_rf <- function(
  start_date = NULL,
  end_date = NULL,
  frequency = "monthly"
) {
  if (!(frequency %in% c("monthly", "daily"))) {
    cli::cli_abort(
      "{.arg frequency} must be {.str monthly} or {.str daily}."
    )
  }

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  if (frequency == "monthly") {
    splice_date <- as.Date("2001-01-01")

    fred_tb3ms <- suppressMessages(download_data_fred("TB3MS"))
    fred_dtb4wk <- suppressMessages(download_data_fred("DTB4WK"))

    rf_tb3ms <- fred_tb3ms |>
      mutate(
        ret_3m = (value / 100) * (90 / 360) /
          (1 - (value / 100) * (90 / 360)),
        risk_free = (1 + ret_3m)^(1 / 3) - 1
      ) |>
      select(date, risk_free)

    rf_dtb4wk <- fred_dtb4wk |>
      tidyr::drop_na(value) |>
      mutate(date = floor_date(date, "month")) |>
      group_by(date) |>
      slice_tail(n = 1) |>
      ungroup() |>
      mutate(
        ret_4wk = (value / 100) * (28 / 360) /
          (1 - (value / 100) * (28 / 360)),
        risk_free = (1 + ret_4wk)^(365 / 28 / 12) - 1
      ) |>
      select(date, risk_free)

    risk_free_data <- bind_rows(
      rf_tb3ms |> filter(date < splice_date),
      rf_dtb4wk |> filter(date >= splice_date)
    ) |>
      arrange(date)
  } else {
    risk_free_data <- suppressMessages(
      download_data_fred("DTB3")
    ) |>
      arrange(date) |>
      tidyr::fill(value, .direction = "down") |>
      mutate(
        ret_3m = (value / 100) * (90 / 360) /
          (1 - (value / 100) * (90 / 360)),
        risk_free = (1 + ret_3m)^(1 / 63) - 1
      ) |>
      select(date, risk_free)
  }

  if (!is.null(start_date)) {
    risk_free_data <- risk_free_data |>
      filter(between(date, start_date, end_date))
  }

  risk_free_data
}
