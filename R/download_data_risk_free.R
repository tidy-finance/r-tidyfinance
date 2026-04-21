#' Download Risk-Free Rate Data
#'
#' Downloads pre-processed risk-free rate data from the
#' `tidy-finance/risk-free` dataset on HuggingFace. The dataset is
#' updated monthly via a scheduled GitHub Actions workflow that splices the
#' 3-Month Treasury Bill Secondary Market Rate (pre-2001) with the 4-Week
#' Treasury Bill Secondary Market Rate (from 2001 onwards) sourced from FRED.
#' For monthly data, the monthly TB3MS series is spliced with the daily DTB4WK
#' series aggregated to month-end. For daily data, the daily DTB3 series is
#' spliced with the daily DTB4WK series, both at the business-day frequency
#' provided by FRED.
#'
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If
#'   not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the end date for the data. If not
#'   provided, the full dataset is returned.
#' @param frequency A character string, either `"monthly"` (default)
#'   or `"daily"`, specifying the frequency of the returned data. Daily
#'   data starts in 1954-01-04 because of availability of DTB3, while
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

  url <- paste0(
    "https://huggingface.co/datasets/tidy-finance/risk-free/",
    "resolve/main/",
    frequency,
    ".parquet"
  )

  risk_free_data <- tryCatch(
    arrow::read_parquet(url),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to download risk-free rate data from HuggingFace.",
        "i" = "URL attempted: {url}",
        "x" = conditionMessage(e)
      ))
    }
  )

  if (!is.null(start_date)) {
    risk_free_data <- risk_free_data |>
      filter(between(date, start_date, end_date))
  }

  risk_free_data
}
