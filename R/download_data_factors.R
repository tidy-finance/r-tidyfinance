#' Download and Process Fama-French Factor Data
#'
#' Downloads and processes Fama-French factor data based on the specified dataset
#' name and date range. The function requires the 'frenchdata' package to
#' download the data. It processes the raw data into a structured format,
#' including date conversion, scaling factor values, and filtering by the
#' specified date range.
#'
#' If there are multiple tables in the raw Fama-French data (e.g., value-weighted
#' and equal-weighted returns), then the function only returns the first table
#' because these are the most popular. Please use the `frenchdata` package
#' directly if you need less commonly used tables.
#'
#' @param dataset The name of the Fama-French dataset to download
#'   (e.g., "Fama/French 3 Factors").
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, the full dataset is returned.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#'
#' @returns A tibble with processed factor data, including the date, risk-free
#'   rate, market excess return, and other factors, filtered by the specified
#'   date range.
#'
#' @export
#' @examples
#' \donttest{
#'   download_data_factors_ff("Fama/French 3 Factors", "2000-01-01", "2020-12-31")
#'   download_data_factors_ff("10 Industry Portfolios", "2000-01-01", "2020-12-31")
#' }
download_data_factors_ff <- function(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated()
) {
  # Handle explicit type argument
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_factors_ff(type)",
      with = "download_data_factors_ff(dataset)"
    )
    parsed <- parse_type_to_domain_dataset(type)
    dataset <- parsed$dataset
  }

  # Handle legacy type passed as dataset argument
  if (!is.null(dataset) && is_legacy_type_ff(dataset)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_factors_ff(type)",
      with = "download_data_factors_ff(dataset)",
      details = paste0(
        "Column type should be replaced with dataset name. ",
        "Use `list_supported_types(domain = 'Fama-French')` to see the mapping."
      )
    )
    parsed <- parse_type_to_domain_dataset(dataset)
    dataset <- parsed$dataset
  }

  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  check_supported_dataset_ff(dataset)

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

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

  frequency <- determine_frequency_ff(dataset)

  if (frequency == "monthly") {
    processed_data <- raw_data |>
      mutate(date = floor_date(ymd(paste0(date, "01")), "month"))
  } else if (frequency %in% c("daily", "weekly")) {
    processed_data <- raw_data |>
      mutate(date = ymd(date))
  } else {
    cli::cli_abort(
      "This dataset has neither daily, weekly, nor monthly frequency."
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
#' Downloads and processes Global Q factor data based on the specified dataset,
#' date range, and source URL. The processing includes date conversion, renaming
#' variables to a standardized format, scaling factor values, and filtering by
#' the specified date range.
#'
#' @param dataset The name of the dataset to download
#'   (e.g., "q5_factors_daily_2023.csv", "q5_factors_monthly_2023.csv").
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, the full dataset is returned.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#' @param url The base URL from which to download the dataset files.
#'
#' @returns A tibble with processed factor data, including the date, risk-free
#'   rate, market excess return, and other factors, filtered by the specified
#'   date range.
#'
#' @export
#' @examples
#' \donttest{
#'   download_data_factors_q("q5_factors_daily_2023.csv", "2020-01-01", "2020-12-31")
#'   download_data_factors_q("q5_factors_annual_2023.csv")
#' }
download_data_factors_q <- function(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  url = "https://global-q.org/uploads/1/2/2/6/122679606/"
) {
  # Handle explicit type argument
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_factors_q(type)",
      with = "download_data_factors_q(dataset)"
    )
    parsed <- parse_type_to_domain_dataset(type)
    dataset <- parsed$dataset
  }

  # Handle legacy type passed as dataset argument
  if (!is.null(dataset) && is_legacy_type_q(dataset)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_factors_q(type)",
      with = "download_data_factors_q(dataset)",
      details = paste0(
        "Column type should be replaced with dataset name. ",
        "Use `list_supported_types(domain = 'Global Q')` to see the mapping."
      )
    )
    parsed <- parse_type_to_domain_dataset(dataset)
    dataset <- parsed$dataset
  }

  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  check_supported_dataset_q(dataset)

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  raw_data <- handle_download_error(
    function(url) {
      suppressWarnings(
        suppressMessages(utils::read.csv(url)) |> as_tibble()
      )
    },
    paste0(url, dataset),
    fallback = tibble(
      date = Date()
    )
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(raw_data)
  }

  frequency <- determine_frequency_q(dataset)

  if (frequency == "monthly") {
    processed_data <- raw_data |>
      mutate(date = ymd(paste(year, month, "01", sep = "-"))) |>
      select(-c(year, month))
  } else if (frequency == "daily") {
    processed_data <- raw_data |>
      mutate(DATE = ymd(DATE))
  } else if (frequency == "annual") {
    processed_data <- raw_data |>
      mutate(date = year)
  } else if (frequency %in% c("weekly", "quarterly")) {
    processed_data <- raw_data |>
      mutate(date = ymd(paste(year, month, day, sep = "-"))) |>
      select(-c(year, month, day))
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

# Helper functions --------------------------------------------------------

#' Check if a string is a legacy Fama-French type
#' @noRd
is_legacy_type_ff <- function(x) {
  ff_types <- dplyr::bind_rows(
    list_supported_types_ff(),
    list_supported_types_ff_legacy()
  )
  x %in% ff_types$type
}

#' Check if a string is a legacy Global Q type
#' @noRd
is_legacy_type_q <- function(x) {
  q_types <- list_supported_types_q()
  x %in% q_types$type
}

#' Determine frequency from Fama-French dataset name
#' @noRd
determine_frequency_ff <- function(dataset) {
  if (grepl("\\[Daily\\]", dataset)) {
    return("daily")
  } else if (grepl("\\[Weekly\\]", dataset)) {
    return("weekly")
  } else {
    return("monthly")
  }
}

#' Determine frequency from Global Q dataset name
#' @noRd
determine_frequency_q <- function(dataset) {
  if (grepl("daily", dataset, ignore.case = TRUE)) {
    return("daily")
  } else if (grepl("weekly", dataset, ignore.case = TRUE)) {
    return("weekly")
  } else if (grepl("monthly", dataset, ignore.case = TRUE)) {
    return("monthly")
  } else if (grepl("quarterly", dataset, ignore.case = TRUE)) {
    return("quarterly")
  } else if (grepl("annual", dataset, ignore.case = TRUE)) {
    return("annual")
  } else {
    cli::cli_abort(
      "Cannot determine frequency from dataset name: {.val {dataset}}"
    )
  }
}

#' Check if Fama-French dataset is supported
#' @noRd
check_supported_dataset_ff <- function(dataset) {
  ff_types <- dplyr::bind_rows(
    list_supported_types_ff(),
    list_supported_types_ff_legacy()
  )

  if (!dataset %in% ff_types$dataset_name) {
    cli::cli_abort(c(
      "Unsupported Fama-French dataset: {.val {dataset}}",
      "i" = "Use {.fn list_supported_types} with {.arg domain = 'Fama-French'} to see available datasets."
    ))
  }
}

#' Check if Global Q dataset is supported
#' @noRd
check_supported_dataset_q <- function(dataset) {
  q_types <- list_supported_types_q()

  if (!dataset %in% q_types$dataset_name) {
    cli::cli_abort(c(
      "Unsupported Global Q dataset: {.val {dataset}}",
      "i" = "Use {.fn list_supported_types} with {.arg domain = 'Global Q'} to see available datasets."
    ))
  }
}
