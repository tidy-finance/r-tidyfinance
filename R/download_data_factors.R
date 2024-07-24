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
#' @returns A tibble with processed factor data, including dates, risk-free
#'   rates, market excess returns, and other factors, filtered by the specified
#'   date range.
#'
#' @export
#' @examples
#' \donttest{
#'   download_data_factors("factors_ff_3_monthly", "2000-01-01", "2020-12-31")
#'   download_data_factors("factors_ff_3_daily")
#'   download_data_factors("factors_q5_daily", "2020-01-01", "2020-12-31")
#' }
download_data_factors <- function(
    type, start_date, end_date
  ) {

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
#' (e.g., "factors_ff_3_monthly"), and date range. The function first checks if
#' the specified type is supported and requires the 'frenchdata' package to
#' download the data. It processes the raw data into a structured format,
#' including date conversion, scaling factor values, and filtering by the
#' specified date range.
#'
#' If there are multiple tables in the raw Fama-French data (e.g., value-weighted
#' and equal-weighted returns), then the function only returns the first table
#' because these are the most popular. Please use the `frenchdata` package
#' directly if you need less commonly used tables.
#'
#' @param type The type of dataset to download, corresponding to the specific
#'   Fama-French model and frequency.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, the full dataset is returned.
#'
#' @returns A tibble with processed factor data, including the date, risk-free
#'   rate, market excess return, and other factors, filtered by the specified
#'   date range.
#'
#' @export
#' @examples
#' \donttest{
#'   download_data_factors_ff("factors_ff_3_monthly", "2000-01-01", "2020-12-31")
#'   download_data_factors_ff("factors_ff_10_industry_portfolios_monthly", "2000-01-01", "2020-12-31")
#' }
download_data_factors_ff <- function(
    type, start_date, end_date
  ) {

  check_supported_type(type)

  check_if_package_installed("frenchdata", type)

  if (missing(start_date) || missing(end_date)) {
    cli::cli_inform(
      "No {.arg start_date} or {.arg end_date} provided. Returning the full data set."
    )
  } else {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
  }

  factors_ff_types <- list_supported_types_ff()
  dataset <- factors_ff_types$dataset_name[factors_ff_types$type == type]

  raw_data <- suppressMessages(frenchdata::download_french_data(dataset))
  raw_data <- raw_data$subsets$data[[1]]

  if (grepl("monthly", type, fixed = TRUE)) {
    processed_data <- raw_data |>
      mutate(date = floor_date(ymd(paste0(date, "01")), "month"))
  } else if (grepl("daily|weekly", type)) {
    processed_data <- raw_data |>
      mutate(date = ymd(date))
  } else {
    cli::cli_abort(
      "This data type has neither daily, weekly, nor monthly frequency."
    )
  }

  # Transform column values
  processed_data <- processed_data |>
    mutate(
      across(-date, ~na_if(., -99.99)),
      across(-date, ~na_if(., -999)),
      across(-date, ~ . / 100)
    )

  # Clean column names
  colnames_clean <- colnames(processed_data) |>
    tolower() |>
    gsub("-rf", "_excess", x = _) |>
    gsub("rf", "risk_free", x = _)

  colnames(processed_data) <- colnames_clean

  if (!missing(start_date) && !missing(end_date)) {
    processed_data <- processed_data |>
      filter(between(date, start_date, end_date))
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
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, the full dataset is returned.
#' @param url The base URL from which to download the dataset files, with a
#'   specific path for Global Q datasets.
#'
#' @returns A tibble with processed factor data, including the date, risk-free
#'   rate, market excess return, and other factors, filtered by the specified
#'   date range.
#'
#' @export
#' @examples
#' \donttest{
#'   download_data_factors_q("factors_q5_daily", "2020-01-01", "2020-12-31")
#' }
download_data_factors_q <- function(
    type, start_date, end_date, url = "http://global-q.org/uploads/1/2/2/6/122679606/"
) {

  check_supported_type(type)

  if (missing(start_date) || missing(end_date)) {
    cli::cli_inform(
      "No {.arg start_date} or {.arg end_date} provided. Returning the full data set."
    )
  } else {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
  }

  factors_q_types <- list_supported_types_q()
  dataset <- factors_q_types$dataset_name[factors_q_types$type == type]
  raw_data <- suppressMessages(utils::read.csv(paste0(url, dataset)) |> as_tibble())

  if (grepl("monthly", type, fixed = TRUE)) {
    processed_data <- raw_data |>
      mutate(date = ymd(paste(year, month, "01", sep = "-"))) |>
      select(-c(year, month))
  }
  if (grepl("daily", type, fixed = TRUE)) {
    processed_data <- raw_data |>
      mutate(DATE = ymd(DATE))
  }

  processed_data <- processed_data |>
    rename_with(~sub("R_", "", ., fixed = TRUE)) |>
    rename_with(tolower) |>
    mutate(across(-date, ~. / 100)) |>
    select(date, risk_free = f, mkt_excess = mkt, everything())

  if (!missing(start_date) && !missing(end_date)) {
    processed_data <- processed_data |>
      filter(between(date, start_date, end_date))
  }

  processed_data
}
