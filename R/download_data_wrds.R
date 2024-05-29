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
#' @param start_date A date in 'YYYY-MM-DD' format indicating the start of the
#'   period for which data is requested.
#' @param end_date A date in 'YYYY-MM-DD' format indicating the end of the
#'   period for which data is requested.
#'
#' @return A data frame containing the requested data, with the structure and
#'   contents depending on the specified `type`.
#'
#' @examples
#' \donttest{
#'   crsp_monthly <- download_data_wrds("wrds_crsp_monthly", "2020-01-01", "2020-12-31")
#'   compustat_annual <- download_data_wrds("wrds_compustat_annual", "2020-01-01", "2020-12-31")
#'   ccm_links <- download_data_wrds("wrds_ccm_links", "2020-01-01", "2020-12-31")
#' }
#'
#' @export
download_data_wrds <- function(type, start_date, end_date) {

  check_supported_type(type)

  if (grepl("wrds_crsp", type)) {
    processed_data <- download_data_wrds_crsp(type, start_date, end_date)
  }
  if (grepl("wrds_compustat", type)) {
    processed_data <- download_data_wrds_compustat(type, start_date, end_date)
  }
  if (grepl("wrds_ccm_links", type)) {
    processed_data <- download_data_wrds_ccm_links()
  }

  processed_data

}
