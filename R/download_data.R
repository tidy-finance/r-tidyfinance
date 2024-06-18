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
#' @param start_date The start date for filtering the data, in "YYYY-MM-DD"
#'   format.
#' @param end_date The end date for filtering the data, in "YYYY-MM-DD" format.
#'
#' @return A tibble with processed data, including dates and the relevant
#'   financial metrics, filtered by the specified date range.
#'
#' @examples
#' \donttest{
#'   download_data("factors_ff3_monthly", "2000-01-01", "2020-12-31")
#'   download_data("macro_predictors_monthly", "2000-01-01", "2020-12-31")
#' }
#'
#' @export
download_data <- function(type, start_date, end_date) {

  check_supported_type(type)

  if (grepl("factors", type, fixed = TRUE)) {
    processed_data <- download_data_factors(type, start_date, end_date)
  }

  if (grepl("macro_predictors", type, fixed = TRUE)) {
    processed_data <- download_data_macro_predictors(type, start_date, end_date)
  }

  if (grepl("wrds", type, fixed = TRUE)) {
    processed_data <- download_data_wrds(type, start_date, end_date)
  }

  processed_data
}
