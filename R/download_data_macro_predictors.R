#' Download and Process Macro Predictor Data
#'
#' Downloads and processes macroeconomic predictor data based on the specified
#' type (monthly, quarterly, or annual), date range, and source URL. The
#' function first checks if the specified type is supported, then downloads the
#' data from the provided URL (defaulting to a Google Sheets export link). It
#' processes the raw data into a structured format, calculating additional
#' financial metrics and filtering by the specified date range.
#'
#' @param type The type of dataset to download ("macro_predictors_monthly",
#'   "macro_predictors_quarterly", "macro_predictors_annual").
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, the full dataset is returned.
#' @param sheet_id The Google Sheets ID from which to download the dataset, with the
#'   default "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG".
#'
#' @returns A tibble with processed data, filtered by the specified date range
#'   and including financial metrics.
#'
#' @export
#' @examples
#' \donttest{
#'   macro_predictors_monthly <- download_data_macro_predictors("macro_predictors_monthly")
#' }
download_data_macro_predictors <- function(
    type, start_date = NULL, end_date = NULL, sheet_id = "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG"
  ) {

  check_supported_type(type)

  if (is.null(start_date) || is.null(end_date)) {
    cli::cli_inform(
      "No {.arg start_date} or {.arg end_date} provided. Returning the full data set."
    )
  } else {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
  }

  build_macro_predictors_url <- function(sheet_name) {
    paste0(
      "https://docs.google.com/spreadsheets/d/", sheet_id,
      "/gviz/tq?tqx=out:csv&sheet=", sheet_name
    )
  }

  if (grepl("monthly", type, fixed = TRUE)) {
    raw_data <- as_tibble(read.csv(build_macro_predictors_url("Monthly")))
    processed_data <- raw_data |>
      mutate(date = ym(yyyymm))
  }
  if (grepl("quarterly", type, fixed = TRUE)) {
    raw_data <- as_tibble(read.csv(build_macro_predictors_url("Quarterly")))
    processed_data <- raw_data |>
      mutate(
        year = substr(yyyyq, 1, 4),
        quarter = substr(yyyyq, 5, 5),
        month = as.integer(quarter) * 3 - 2,
        date = as.Date(paste0(year, "-", month, "-01"))
      )
  }
  if (grepl("annual", type, fixed = TRUE)) {
    raw_data <- as_tibble(read.csv(build_macro_predictors_url("Annual")))
    processed_data <- raw_data |>
      mutate(date = as.Date(paste0(yyyy, "-01-01")))
  }

  processed_data <- processed_data |>
    mutate(
      across(where(is.character), as.numeric),
      IndexDiv = Index + D12,
      logret = log(IndexDiv) - log(lag(IndexDiv)),
      rp_div = lead(logret - Rfree, 1),
      log_d12 = log(D12),
      log_e12 = log(E12),
      dp = log_d12 - log(Index),
      dy = log_d12 - log(lag(Index)),
      ep = log_e12 - log(Index),
      de = log_d12 - log_e12,
      tms = lty - tbl,
      dfy = BAA - AAA
    ) |>
    select(date, rp_div, dp, dy, ep, de, svar, bm = `b.m`, ntis, tbl, lty, ltr,
           tms, dfy, infl
    ) |>
    tidyr::drop_na()

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(date, start_date, end_date))
  }

  processed_data
}
