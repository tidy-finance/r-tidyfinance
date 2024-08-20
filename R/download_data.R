# Main function -----------------------------------------------------------
#' Download and Process Data Based on Type
#'
#' Downloads and processes data based on the specified type (e.g., Fama-French
#' factors, Global Q factors, or macro predictors), and date range. This
#' function checks if the specified type is supported and then delegates to the
#' appropriate function for downloading and processing the data.
#'
#' @param type The type of dataset to download, indicating either factor data or
#'   macroeconomic predictors.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset or a subset is returned,
#'   dependening on the dataset type.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, the full dataset or a subset is returned,
#'   depending on the dataset type.
#' @param ... Additional arguments passed to specific download functions depending on the `type`.
#'   For instance, if `type` is "constituents", this might include parameters specific to `download_data_constituents`.
#'
#' @returns A tibble with processed data, including dates and the relevant
#'   financial metrics, filtered by the specified date range.
#'
#' @export
#' @examples
#' \donttest{
#'   download_data("factors_ff_3_monthly", "2000-01-01", "2020-12-31")
#'   download_data("macro_predictors_monthly", "2000-01-01", "2020-12-31")
#'   download_data("constituents", index = "DAX")
#'   download_data("fred", series = c("GDP", "CPIAUCNS"))
#' }
download_data <- function(type, start_date = NULL, end_date = NULL, ...) {

  check_supported_type(type)

  if (grepl("factors", type, fixed = TRUE)) {
    processed_data <- download_data_factors(type, start_date, end_date)
  } else if (grepl("macro_predictors", type, fixed = TRUE)) {
    processed_data <- download_data_macro_predictors(type, start_date, end_date)
  } else if (grepl("wrds", type, fixed = TRUE)) {
    processed_data <- download_data_wrds(type, start_date, end_date)
  } else if (grepl("constituents", type, fixed = TRUE)) {
    processed_data <- download_data_constituents(...)
  } else if (grepl("fred", type, fixed = TRUE)) {
    processed_data <- download_data_fred(..., start_date, end_date)
  } else if (grepl("stocks", type, fixed = TRUE)) {
    processed_data <- download_data_stocks(type, ..., start_date, end_date)
  } else if (grepl("osap", type, fixed = TRUE)) {
    processed_data <- download_data_osap(start_date, end_date)
  }
  processed_data
}
