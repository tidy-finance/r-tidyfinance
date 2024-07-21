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
#' @param url The URL from which to download the dataset, with a default Google
#'   Sheets export link.
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
    type, start_date, end_date, url = "https://docs.google.com/spreadsheets/d/1g4LOaRj4TvwJr9RIaA_nwrXXWTOy46bP"
  ) {

  check_supported_type(type)

  check_if_package_installed("readxl", type)

  if (missing(start_date) || missing(end_date)) {
    message("No start_date or end_date provided. Returning the full data set.")
  } else {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
  }

  temporary_file <- tempfile()
  on.exit(unlink(temporary_file), add = TRUE)

  utils::download.file(
    url = paste0(url, "/export?format=xlsx"),
    destfile = temporary_file,
    mode = "wb",
    quiet = TRUE
  )

  if (grepl("monthly", type, fixed = TRUE)) {
    raw_data <- suppressMessages(readxl::read_xlsx(temporary_file, sheet = "Monthly"))
    processed_data <- raw_data |>
      mutate(date = ym(yyyymm))
  }
  if (grepl("quarterly", type, fixed = TRUE)) {
    raw_data <- suppressMessages(readxl::read_xlsx(temporary_file, sheet = "Quarterly"))
    processed_data <- raw_data |>
      mutate(
        year = substr(yyyyq, 1, 4),
        quarter = substr(yyyyq, 5, 5),
        month = as.integer(quarter)*3-2,
        date = as.Date(paste0(year, "-", month, "-01"))
      )
  }
  if (grepl("annual", type, fixed = TRUE)) {
    raw_data <- suppressMessages(readxl::read_xlsx(temporary_file, sheet = "Annual"))
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
    select(date, rp_div, dp, dy, ep, de, svar, bm = `b/m`, ntis, tbl, lty, ltr,
           tms, dfy, infl
    ) |>
    tidyr::drop_na()

  if (!missing(start_date) && !missing(end_date)) {
    processed_data <- processed_data |>
      filter(between(date, start_date, end_date))
  }

  processed_data
}
