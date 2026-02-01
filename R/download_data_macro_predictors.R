#' Download and Process Macro Predictor Data
#'
#' Downloads and processes macroeconomic predictor data based on the specified
#' dataset (monthly, quarterly, or annual), date range, and source URL. The
#' function downloads the data from a Google Sheets export link. It
#' processes the raw data into a structured format, calculating additional
#' financial metrics and filtering by the specified date range.
#'
#' @param dataset The dataset to download ("monthly", "quarterly", "annual").
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, the full dataset is returned.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#' @param sheet_id The Google Sheets ID from which to download the dataset, with the
#'   default "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG".
#'
#' @returns A tibble with processed data, filtered by the specified date range
#'   and including financial metrics.
#'
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_macro_predictors("monthly")
#'   download_data_macro_predictors("quarterly", "2000-01-01", "2020-12-31")
#' }
#'
download_data_macro_predictors <- function(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  sheet_id = "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG"
) {
  # Handle explicit type argument
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_macro_predictors(type)",
      details = "Use the `dataset` argument instead."
    )
    dataset <- sub("^macro_predictors_", "", type)
  }

  # Handle legacy type passed as dataset argument
  if (!is.null(dataset) && is_legacy_type_macro_predictors(dataset)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_macro_predictors(type)",
      details = paste0(
        "The `type` argument is deprecated. ",
        "Use `dataset` instead (e.g., 'monthly' instead of 'macro_predictors_monthly')."
      )
    )
    dataset <- sub("^macro_predictors_", "", dataset)
  }

  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  check_supported_dataset_macro_predictors(dataset)

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  build_macro_predictors_url <- function(sheet_name) {
    paste0(
      "https://docs.google.com/spreadsheets/d/",
      sheet_id,
      "/gviz/tq?tqx=out:csv&sheet=",
      sheet_name
    )
  }

  if (dataset == "monthly") {
    raw_data <- handle_download_error(
      function() {
        suppressWarnings(
          as_tibble(read.csv(build_macro_predictors_url("Monthly")))
        )
      },
      fallback = tibble()
    )

    if (nrow(raw_data) == 0) {
      cli::cli_inform("Returning an empty data set due to download failure.")
      return(raw_data)
    }

    processed_data <- raw_data |>
      mutate(date = ym(yyyymm))
  } else if (dataset == "quarterly") {
    raw_data <- handle_download_error(
      function() {
        suppressWarnings(
          as_tibble(read.csv(build_macro_predictors_url("Quarterly")))
        )
      },
      fallback = tibble()
    )

    if (nrow(raw_data) == 0) {
      cli::cli_inform("Returning an empty data set due to download failure.")
      return(raw_data)
    }

    processed_data <- raw_data |>
      mutate(
        year = substr(yyyyq, 1, 4),
        quarter = substr(yyyyq, 5, 5),
        month = as.integer(quarter) * 3 - 2,
        date = as.Date(paste0(year, "-", month, "-01"))
      )
  } else if (dataset == "annual") {
    raw_data <- handle_download_error(
      function() {
        suppressWarnings(
          as_tibble(read.csv(build_macro_predictors_url("Annual")))
        )
      },
      fallback = tibble()
    )

    if (nrow(raw_data) == 0) {
      cli::cli_inform("Returning an empty data set due to download failure.")
      return(raw_data)
    }

    processed_data <- raw_data |>
      mutate(date = as.Date(paste0(yyyy, "-01-01")))
  }

  processed_data <- processed_data |>
    mutate(
      across(where(is.character), ~ as.numeric(gsub("\\,", "", .))),
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
    select(
      date,
      rp_div,
      dp,
      dy,
      ep,
      de,
      svar,
      bm = `b.m`,
      ntis,
      tbl,
      lty,
      ltr,
      tms,
      dfy,
      infl
    ) |>
    tidyr::drop_na()

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(date, start_date, end_date))
  }

  processed_data
}

#' Check if a string is a legacy macro predictors type
#' @noRd
is_legacy_type_macro_predictors <- function(x) {
  grepl("^macro_predictors_", x)
}

#' Check if macro predictors dataset is supported
#' @noRd
check_supported_dataset_macro_predictors <- function(dataset) {
  supported_datasets <- c("monthly", "quarterly", "annual")

  if (!dataset %in% supported_datasets) {
    cli::cli_abort(c(
      "Unsupported macro predictors dataset: {.val {dataset}}",
      "i" = "Supported datasets: {.val {supported_datasets}}"
    ))
  }
}
