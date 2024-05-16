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
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#' @param url The URL from which to download the dataset, with a default Google
#'   Sheets export link.
#'
#' @return A tibble with processed data, filtered by the specified date range
#'   and including financial metrics.
#'
#' @examples
#' \donttest{
#'   download_data_macro_predictors("macro_predictors_monthly", "2000-01-01", "2020-12-31")
#' }
#'
#' @import dplyr
#' @importFrom tidyr drop_na
#' @importFrom lubridate ym
#' @importFrom utils download.file
#'
#' @export
download_data_macro_predictors <- function(type, start_date, end_date, url = "https://docs.google.com/spreadsheets/d/1g4LOaRj4TvwJr9RIaA_nwrXXWTOy46bP") {

  check_supported_type(type)

  check_if_package_installed("readxl", type)

  read_xlsx <- getNamespace("readxl")$read_xlsx

  temporary_file <- tempfile()

  utils::download.file(
    url = paste0(url, "/export?format=xlsx"),
    destfile = temporary_file,
    mode = "wb",
    quiet = TRUE
  )

  if (grepl("monthly", type)) {
    raw_data <- suppressMessages(read_xlsx(temporary_file, sheet = "Monthly"))
    processed_data <- raw_data |>
      mutate(date = lubridate::ym(yyyymm))
  }
  if (grepl("quarterly", type)) {
    raw_data <- suppressMessages(read_xlsx(temporary_file, sheet = "Quarterly"))
    processed_data <- raw_data |>
      mutate(
        year = substr(yyyyq, 1, 4),
        quarter = substr(yyyyq, 5, 5),
        month = as.integer(quarter)*3-2,
        date = as.Date(paste0(year, "-", month, "-01"))
      )
  }
  if (grepl("annual", type)) {
    raw_data <- suppressMessages(read_xlsx(temporary_file, sheet = "Annual"))
    processed_data <- raw_data |>
      mutate(date = as.Date(paste0(yyyy, "-01-01")))
  }

  processed_data <- processed_data |>
    mutate(across(where(is.character), as.numeric)) |>
    mutate(
      IndexDiv = Index + D12,
      logret = log(IndexDiv) - log(lag(IndexDiv)),
      rp_div = lead(logret - Rfree, 1),
      dp = log(D12) - log(Index),
      dy = log(D12) - log(lag(Index)),
      ep = log(E12) - log(Index),
      de = log(D12) - log(E12),
      tms = lty - tbl,
      dfy = BAA - AAA
    ) |>
    select(date, rp_div, dp, dy, ep, de, svar, bm = `b/m`, ntis, tbl, lty, ltr,
           tms, dfy, infl
    ) |>
    filter(date >= start_date & date <= end_date) |>
    tidyr::drop_na()

  file.remove(temporary_file)

  processed_data
}
