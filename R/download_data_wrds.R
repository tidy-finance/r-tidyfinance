#' Download Data from WRDS
#'
#' This function acts as a wrapper to download data from various WRDS datasets
#' including CRSP, Compustat, and CCM links based on the specified type. It is
#' designed to handle different data types by redirecting to the appropriate
#' specific data download function.
#'
#' @param type A string specifying the type of data to download. It should match
#'   one of the predefined patterns to indicate the dataset: "wrds_crsp" for
#'   CRSP data, "wrds_compustat" for Compustat data, or "wrds_ccm_links" for CCM
#'   links data.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subste of the dataset is returned.
#'
#' @returns A data frame containing the requested data, with the structure and
#'   contents depending on the specified `type`.
#'
#' @export
#' @examples
#' \donttest{
#'   crsp_monthly <- download_data_wrds("wrds_crsp_monthly", "2020-01-01", "2020-12-31")
#'   compustat_annual <- download_data_wrds("wrds_compustat_annual", "2020-01-01", "2020-12-31")
#'   ccm_links <- download_data_wrds("wrds_ccm_links", "2020-01-01", "2020-12-31")
#' }
download_data_wrds <- function(type, start_date, end_date) {

  check_supported_type(type)

  if (grepl("wrds_crsp", type, fixed = TRUE)) {
    processed_data <- download_data_wrds_crsp(type, start_date, end_date)
  } else if (grepl("wrds_compustat", type, fixed = TRUE)) {
    processed_data <- download_data_wrds_compustat(type, start_date, end_date)
  } else if (grepl("wrds_ccm_links", type, fixed = TRUE)) {
    processed_data <- download_data_wrds_ccm_links()
  }

  processed_data

}
