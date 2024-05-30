#' Download and Process Factor Data
#'
#' Downloads and processes factor data based on the specified type (Fama-French
#' or Global Q), and date range. This function delegates to specific functions
#' based on the type of factors requested: Fama-French or Global Q. It checks if
#' the specified type is supported before proceeding with the download and
#' processing.
#'
#' @param type The type of dataset to download, indicating the factor model and
#'   frequency.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#'
#' @return A tibble with processed factor data, including dates, risk-free
#'   rates, market excess returns, and other factors, filtered by the specified
#'   date range.
#'
#' @examples
#' \donttest{
#'   download_data_factors("factors_ff3_monthly", "2000-01-01", "2020-12-31")
#'   download_data_factors("factors_q5_daily", "2020-01-01", "2020-12-31")
#' }
#'
#' @export
download_data_factors <- function(type, start_date, end_date) {

  check_supported_type(type)

  if (grepl("factors_ff", type, fixed = TRUE)) {
    processed_data <- download_data_factors_ff(type, start_date, end_date)
  }
  if (grepl("factors_q", type, fixed = TRUE)) {
    processed_data <- download_data_factors_q(type, start_date, end_date)
  }

  processed_data
}

#' Download and Process Fama-French Factor Data
#'
#' Downloads and processes Fama-French factor data based on the specified type
#' (e.g., "factors_ff3_monthly"), and date range. The function first checks if
#' the specified type is supported and requires the 'frenchdata' package to
#' download the data. It processes the raw data into a structured format,
#' including date conversion, scaling factor values, and filtering by the
#' specified date range.
#'
#' @param type The type of dataset to download, corresponding to the specific
#'   Fama-French model and frequency.
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#'
#' @return A tibble with processed factor data, including the date, risk-free
#'   rate, market excess return, and other factors, filtered by the specified
#'   date range.
#'
#' @examples
#' \donttest{
#'   download_data_factors_ff("factors_ff3_monthly", "2000-01-01", "2020-12-31")
#'   download_data_factors_ff("factors_ff_industry_10_monthly", "2000-01-01", "2020-12-31")
#' }
#'
#' @import dplyr
#' @importFrom lubridate ymd floor_date
#'
#' @export
download_data_factors_ff <- function(type, start_date, end_date) {

  check_supported_type(type)

  check_if_package_installed("frenchdata", type)

  download_french_data <- getNamespace("frenchdata")$download_french_data

  factors_ff_types <- list_supported_types_ff()
  dataset <- factors_ff_types$dataset_name[factors_ff_types$type == type]

  raw_data <- suppressMessages(download_french_data(dataset))
  raw_data <- raw_data$subsets$data[[1]]

  if (grepl("monthly", type, fixed = TRUE)) {
    processed_data <- raw_data |>
      mutate(date = lubridate::floor_date(lubridate::ymd(paste0(date, "01")), "month"))
  } else {
    processed_data <- raw_data |>
      mutate(date = lubridate::ymd(date))
  }

  processed_data <- processed_data |>
    mutate(
      across(-date, ~na_if(.,-99.99)),
      across(-date, ~ . / 100),
    ) |>
    rename_with(tolower) |>
    filter(date >= start_date & date <= end_date)

  processed_data <- if (grepl("industry", type, fixed = TRUE)) {
    processed_data |>
      select(date, everything())
  } else {
    processed_data |>
      select(date, risk_free = rf, mkt_excess = `mkt-rf`, everything())
  }

  processed_data
}

#' Download and Process Global Q Factor Data
#'
#' Downloads and processes Global Q factor data based on the specified type
#' (daily, monthly, etc.), date range, and source URL. The function first checks
#' if the specified type is supported, identifies the dataset name from the
#' supported types, then downloads and processes the data from the provided URL.
#' The processing includes date conversion, renaming variables to a standardized
#' format, scaling factor values, and filtering by the specified date range.
#'
#' @param type The type of dataset to download (e.g., "factors_q5_daily",
#'   "factors_q5_monthly").
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#' @param url The base URL from which to download the dataset files, with a
#'   specific path for Global Q datasets.
#'
#' @return A tibble with processed factor data, including the date, risk-free
#'   rate, market excess return, and other factors, filtered by the specified
#'   date range.
#'
#' @examples
#' \donttest{
#'   download_data_factors_q("factors_q5_daily", "2020-01-01", "2020-12-31")
#' }
#'
#' @import dplyr
#' @importFrom lubridate ymd
#' @importFrom utils read.csv
#'
#' @export
download_data_factors_q <- function(
    type, start_date, end_date, url = "http://global-q.org/uploads/1/2/2/6/122679606/"
) {

  check_supported_type(type)

  factors_q_types <- list_supported_types_q()
  dataset <- factors_q_types$dataset_name[factors_q_types$type == type]
  raw_data <- suppressMessages(utils::read.csv(paste0(url, dataset)) |> as_tibble())

  if (grepl("monthly", type, fixed = TRUE)) {
    processed_data <- raw_data |>
      mutate(date = lubridate::ymd(paste(year, month, "01", sep = "-"))) |>
      select(-c(year, month))
  }
  if (grepl("daily", type, fixed = TRUE)) {
    processed_data <- raw_data |>
      mutate(DATE = lubridate::ymd(DATE))
  }

  processed_data <- processed_data |>
    rename_with(~sub("R_", "", ., fixed = TRUE)) |>
    rename_with(tolower) |>
    mutate(across(-date, ~. / 100)) |>
    filter(date >= start_date & date <= end_date) |>
    select(date, risk_free = f, mkt_excess = mkt, everything())

  processed_data
}
