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
  type,
  start_date = NULL,
  end_date = NULL
) {
  check_supported_type(type)

  if (grepl("factors_ff", type, fixed = TRUE)) {
    processed_data <- download_data_factors_ff(type, start_date, end_date)
  } else if (grepl("factors_q", type, fixed = TRUE)) {
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
  type,
  start_date = NULL,
  end_date = NULL
) {
  check_supported_type(type)

  rlang::check_installed(
    "frenchdata",
    reason = paste0("to download type ", type, ".")
  )

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  factors_ff_types <- list_supported_types_ff()
  dataset <- factors_ff_types$dataset_name[factors_ff_types$type == type]

  raw_data <- handle_download_error(
    function() suppressMessages(frenchdata::download_french_data(dataset)),
    fallback = tibble(
      date = Date()
    )
  )

  if (!inherits(raw_data, "french_dataset")) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(raw_data)
  }

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

  processed_data <- processed_data |>
    mutate(
      across(-date, ~ na_if(., -99.99)),
      across(-date, ~ na_if(., -999)),
      across(-date, ~ . / 100)
    )

  colnames_lower <- tolower(colnames(processed_data))
  colnames_clean <- gsub("-rf", "_excess", colnames_lower, fixed = TRUE)
  colnames_clean <- gsub("rf", "risk_free", colnames_clean, fixed = TRUE)
  colnames(processed_data) <- colnames_clean

  if (!is.null(start_date) && !is.null(end_date)) {
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
#'   download_data_factors_q("factors_q5_annual")
#' }
download_data_factors_q <- function(
  type,
  start_date = NULL,
  end_date = NULL,
  url = "https://global-q.org/uploads/1/2/2/6/122679606/"
) {
  check_supported_type(type)

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  factors_q_types <- list_supported_types_q()
  dataset <- factors_q_types$dataset_name[factors_q_types$type == type]

  raw_data <- handle_download_error(
    function(url)
      suppressWarnings(
        suppressMessages(utils::read.csv(url)) |> as_tibble()
      ),
    paste0(url, dataset),
    fallback = tibble(
      date = Date()
    )
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(raw_data)
  }

  if (grepl("monthly", type, fixed = TRUE)) {
    processed_data <- raw_data |>
      mutate(date = ymd(paste(year, month, "01", sep = "-"))) |>
      select(-c(year, month))
  } else if (grepl("daily", type, fixed = TRUE)) {
    processed_data <- raw_data |>
      mutate(DATE = ymd(DATE))
  } else if (grepl("annual", type, fixed = TRUE)) {
    processed_data <- raw_data |>
      mutate(date = year)
  }

  processed_data <- processed_data |>
    rename_with(~ sub("R_", "", ., fixed = TRUE)) |>
    rename_with(tolower) |>
    mutate(across(-date, ~ . / 100)) |>
    select(date, risk_free = f, mkt_excess = mkt, everything())

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(date, start_date, end_date))
  }

  processed_data
}
