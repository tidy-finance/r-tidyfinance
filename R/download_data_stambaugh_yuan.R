#' Download and Process Stambaugh-Yuan Mispricing Factors
#'
#' Downloads and processes the mispricing factor data of Stambaugh and Yuan
#' (2017) from
#' [Robert Stambaugh's data library](https://finance.wharton.upenn.edu/~stambaug/).
#' The four-factor model (M4) combines the market and size factors with two
#' mispricing factors, `mgmt` (management) and `perf` (performance). The
#' function downloads the requested frequency, aligns the date, renames the
#' columns to the package conventions, and optionally filters the data based on
#' a provided date range.
#'
#' Returns are already expressed as plain numeric (decimal) values in the
#' source data, so no rescaling is applied. The source files currently end in
#' December 2016; a requested date range that lies entirely outside the
#' available data emits a warning and returns an empty tibble.
#'
#' @param dataset The data frequency to download, either `"monthly"` (the
#'   default) or `"daily"`.
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If not
#'   provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the end date for the data. If not provided, the full
#'   dataset is returned.
#' @param url The base URL from which to download the dataset files. The file
#'   name (`M4.csv` or `M4d.csv`) is appended based on `dataset`.
#'
#' @returns A tibble with the columns `date` (aligned to the beginning of the
#'   month for monthly data), `mkt_excess` (the market excess return), `smb`
#'   (size), `mgmt` (the management mispricing factor), `perf` (the performance
#'   mispricing factor), and `risk_free` (the risk-free rate). All returns are
#'   plain numeric (decimal) values, filtered by the specified date range if
#'   `start_date` and `end_date` are provided.
#'
#' @references
#'   Stambaugh, R. F., & Yuan, Y. (2017). Mispricing factors. *Review of
#'   Financial Studies*, 30(4), 1270-1315. \doi{10.1093/rfs/hhw107}
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_stambaugh_yuan(
#'     start_date = "2015-01-01", end_date = "2016-12-31"
#'   )
#'   download_data_stambaugh_yuan(
#'     dataset = "daily", start_date = "2016-01-01", end_date = "2016-12-31"
#'   )
#' }
download_data_stambaugh_yuan <- function(
  dataset = "monthly",
  start_date = NULL,
  end_date = NULL,
  url = "https://finance.wharton.upenn.edu/~stambaug/"
) {
  if (!(dataset %in% c("monthly", "daily"))) {
    cli::cli_abort(
      "{.arg dataset} must be {.str monthly} or {.str daily}."
    )
  }

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  file <- if (dataset == "daily") "M4d.csv" else "M4.csv"

  raw_data <- handle_download_error(
    function() {
      suppressWarnings(
        as_tibble(read.csv(paste0(url, file)))
      )
    },
    fallback = tibble()
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(raw_data)
  }

  # The monthly file keys rows by YYYYMM, the daily file by YYYYMMDD.
  if (dataset == "daily") {
    processed_data <- raw_data |>
      mutate(date = ymd(.data$DATE))
  } else {
    processed_data <- raw_data |>
      mutate(date = floor_date(ym(.data$YYYYMM), "month"))
  }

  processed_data <- processed_data |>
    select(
      "date",
      mkt_excess = "MKTRF",
      smb = "SMB",
      mgmt = "MGMT",
      perf = "PERF",
      risk_free = "RF"
    )

  if (!is.null(start_date) && !is.null(end_date)) {
    available_range <- range(processed_data$date)
    filtered_data <- processed_data |>
      filter(between(.data$date, start_date, end_date))

    if (nrow(filtered_data) == 0) {
      cli::cli_warn(c(
        paste0(
          "The requested date range lies outside the available ",
          "Stambaugh-Yuan data."
        ),
        "i" = "Available data range: {available_range[1]} to {available_range[2]}."
      ))
    }

    processed_data <- filtered_data
  }

  processed_data
}
