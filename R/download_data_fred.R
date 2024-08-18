#' Download and Process Data from FRED
#'
#' This function downloads a specified data series from the Federal Reserve Economic Data (FRED) website,
#' processes the data, and returns it as a tibble.
#'
#' @param series A character string specifying the FRED series ID to download.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD" format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#'
#' @return A tibble containing the processed data with three columns:
#' \describe{
#'   \item{date}{The date corresponding to the data point.}
#'   \item{value}{The value of the data series at that date.}
#'   \item{series}{The FRED series ID corresponding to the data.}
#' }
#'
#' @details
#' This function constructs the URL based on the provided FRED series ID, performs an HTTP GET request
#' to download the data in CSV format, and processes it to a tidy tibble format. The resulting tibble
#' includes the date, value, and the series ID.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_data_fred("CPIAUCNS")
#' download_data_fred("CPIAUCNS", "2020-01-01", "2020-12-31")
#' }
#'
download_data_fred <- function(series, start_date = NULL, end_date = NULL) {

  rlang::check_installed("httr2", reason = "to download data from FRED.")

  if (is.null(start_date) || is.null(end_date)) {
    cli::cli_inform(
      "No {.arg start_date} or {.arg end_date} provided. Returning the full data set."
    )
  } else {
    if (start_date > end_date) {
      cli::cli_abort("{.arg start_date} cannot be after {.arg end_date}.")
    }
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
  }

  url <- paste0("https://fred.stlouisfed.org/series/", series, "/downloaddata/", series, ".csv")

  response <- httr2::request(url) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (response$status_code != 200) {
    cli::cli_abort("Failed to download data from FRED for {.arg series}. Please check the series ID or try again later.")
  }

  fred_raw <- suppressWarnings(httr2::resp_body_string(response)) |>
    textConnection() |>
    read.csv() |>
    as_tibble()

  fred_processed <- fred_raw |>
    mutate(date = as.Date(DATE),
           value = VALUE,
           series = series,
           .keep = "none")

  if (!is.null(start_date) && !is.null(end_date)) {
    fred_processed <- fred_processed |>
      filter(date >= start_date & date <= end_date)
  }

  return(fred_processed)
}
