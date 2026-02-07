#' Download Data from WRDS
#'
#' This function acts as a wrapper to download data from various WRDS datasets
#' including CRSP, Compustat, and CCM links based on the specified dataset. It is
#' designed to handle different datasets by redirecting to the appropriate
#' specific data download function.
#'
#' @param dataset A string specifying the dataset to download. Supported values:
#'   "crsp_monthly", "crsp_daily" for CRSP data, "compustat_annual",
#'   "compustat_quarterly" for Compustat data, "ccm_links" for CCM links data,
#'   "fisd" for FISD data, or "trace_enhanced" for TRACE data.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subset of the dataset is returned.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#' @param ... Additional arguments passed to specific download functions depending on the `dataset`.
#'
#' @returns A data frame containing the requested data, with the structure and
#'   contents depending on the specified `dataset`.
#'
#' @export
#' @examples
#' \dontrun{
#'   crsp_monthly <- download_data_wrds("crsp_monthly", "2020-01-01", "2020-12-31")
#'   compustat_annual <- download_data_wrds("compustat_annual", "2020-01-01", "2020-12-31")
#'   ccm_links <- download_data_wrds("ccm_links")
#'   fisd <- download_data_wrds("fisd")
#'   trace_enhanced <- download_data_wrds("trace_enhanced", cusips = "00101JAH9")
#' }
download_data_wrds <- function(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  ...
) {
  # Handle explicit type argument
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_wrds(type)",
      details = "Use the `dataset` argument instead."
    )
    dataset <- sub("^wrds_", "", type)
  }

  # Handle legacy type passed as dataset argument
  if (!is.null(dataset) && is_legacy_type_wrds(dataset)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_wrds(type)",
      details = paste0(
        "The `type` argument is deprecated. ",
        "Use `dataset` instead (e.g., 'crsp_monthly' instead of 'wrds_crsp_monthly')."
      )
    )
    dataset <- sub("^wrds_", "", dataset)
  }

  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  check_supported_dataset_wrds(dataset)

  if (grepl("^crsp", dataset)) {
    processed_data <- download_data_wrds_crsp(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (grepl("^compustat", dataset)) {
    processed_data <- download_data_wrds_compustat(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (dataset == "ccm_links") {
    processed_data <- download_data_wrds_ccm_links(...)
  } else if (dataset == "fisd") {
    processed_data <- download_data_wrds_fisd(...)
  } else if (dataset == "trace_enhanced") {
    processed_data <- download_data_wrds_trace_enhanced(
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else {
    cli::cli_abort("Unsupported WRDS dataset: {.val {dataset}}")
  }

  processed_data
}

#' Check if a string is a legacy WRDS type
#' @noRd
is_legacy_type_wrds <- function(x) {
  grepl("^wrds_", x)
}

#' Check if WRDS dataset is supported
#' @noRd
check_supported_dataset_wrds <- function(dataset) {
  supported_datasets <- c(
    "crsp_monthly",
    "crsp_daily",
    "compustat_annual",
    "compustat_quarterly",
    "ccm_links",
    "fisd",
    "trace_enhanced"
  )

  if (!dataset %in% supported_datasets) {
    cli::cli_abort(c(
      "Unsupported WRDS dataset: {.val {dataset}}",
      "i" = "Supported datasets: {.val {supported_datasets}}"
    ))
  }
}
