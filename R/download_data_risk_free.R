#' Download Risk-Free Rate Data
#'
#' Downloads and processes risk-free rate data from FRED. Splices the
#' 3-Month Treasury Bill Secondary Market Rate (pre-2001) with the
#' 4-Week Treasury Bill Secondary Market Rate (from 2001 onwards). For
#' monthly data, the monthly TB3MS series is spliced with the daily
#' DTB4WK series aggregated to month-end. For daily data, the daily
#' DTB3 series is spliced with the daily DTB4WK series, both at the
#' business-day frequency provided by FRED.
#'
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If
#'   not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the end date for the data. If not
#'   provided, the full dataset is returned.
#' @param frequency A character string, either `"monthly"` (default)
#'   or `"daily"`, specifying the frequency of the returned data. Daily
#'   data starts in 1954-01-04 because of availabiity of DTB3, while
#'   monthly data starts in 1934-01-01.
#'
#' @details
#' Both series are quoted as annualised bank discount rates on a
#' 360-day basis. Given an annualised discount rate `d` and a T-bill
#' with `n` days to maturity, the holding-period return is
#' `HPR = d * n/360 / (1 - d * n/360)`, which is then converted to the
#' target period length via `(1 + HPR)^(target/source) - 1`.
#'
#' The series are spliced at 2001-07-01:
#' - **Pre-2001**: TB3MS (monthly) or DTB3 (daily), 3-month T-bill
#'   with n = 90. Monthly conversion uses exponent `1/3`; daily
#'   conversion uses exponent `1/63` (approx. trading days per
#'   quarter).
#' - **From 2001**: DTB4WK, 4-week T-bill with n = 28. For monthly
#'   data, the last non-NA observation per calendar month is taken
#'   and the exponent is `365/(28*12)`. For daily data, observations
#'   are used as-is and the exponent is `1/20` (approx. trading days
#'   per 4-week period).
#'
#' Business-day gaps in the daily series (e.g. holidays) are handled
#' by forward-filling the most recent available rate.
#'
#' Monthly data starts in 1934-01-01 (TB3MS). Daily data starts in
#' 1954-01-04 due to the availability of DTB3.
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
#'   download_data_risk_free("2020-01-01", "2020-12-31")
#'   download_data_risk_free(
#'     "2020-01-01", "2020-12-31", frequency = "daily"
#'   )
#' }
download_data_risk_free <- function(
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
    splice_date <- as.Date("2001-07-01")

    fred_tb3ms <- suppressMessages(download_data_fred("TB3MS"))
    fred_dtb4wk <- suppressMessages(download_data_fred("DTB4WK"))

    rf_tb3ms <- fred_tb3ms |>
      tidyr::drop_na(value) |>
      mutate(
        ret_3m = (value / 100) * (90 / 360) / (1 - (value / 100) * (90 / 360)),
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
        ret_4wk = (value / 100) * (28 / 360) / (1 - (value / 100) * (28 / 360)),
        risk_free = (1 + ret_4wk)^(365 / 28 / 12) - 1
      ) |>
      select(date, risk_free)

    risk_free_data <- bind_rows(
      rf_tb3ms |> filter(date < splice_date),
      rf_dtb4wk |> filter(date >= splice_date)
    ) |>
      arrange(date)
  } else {
    splice_date <- as.Date("2001-07-31")

    fred_dtb3 <- suppressMessages(download_data_fred("DTB3"))
    fred_dtb4wk <- suppressMessages(download_data_fred("DTB4WK"))

    rf_dtb3 <- fred_dtb3 |>
      arrange(date) |>
      tidyr::fill(value, .direction = "down") |>
      tidyr::drop_na(value) |>
      mutate(
        ret_3m = (value / 100) * (90 / 360) / (1 - (value / 100) * (90 / 360)),
        risk_free = (1 + ret_3m)^(1 / 63) - 1
      ) |>
      select(date, risk_free)

    rf_dtb4wk <- fred_dtb4wk |>
      arrange(date) |>
      tidyr::fill(value, .direction = "down") |>
      tidyr::drop_na(value) |>
      mutate(
        ret_4wk = (value / 100) * (28 / 360) / (1 - (value / 100) * (28 / 360)),
        risk_free = (1 + ret_4wk)^(1 / 20) - 1
      ) |>
      select(date, risk_free)

    risk_free_data <- bind_rows(
      rf_dtb3 |> filter(date < splice_date),
      rf_dtb4wk |> filter(date >= splice_date)
    ) |>
      arrange(date)
  }

  if (!is.null(start_date)) {
    risk_free_data <- risk_free_data |>
      filter(between(date, start_date, end_date))
  }

  risk_free_data
}
