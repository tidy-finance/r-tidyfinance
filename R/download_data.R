# Main function -----------------------------------------------------------
#' Download and Process Data Based on Domain and Dataset
#'
#' Downloads and processes data based on the specified domain (e.g., Fama-French
#' factors, Global Q factors, or macro predictors), dataset, and date range. This
#' function checks if the specified domain is supported and then delegates to the
#' appropriate function for downloading and processing the data.
#'
#' @param domain The domain of the dataset to download (e.g., "famafrench",
#'   "globalq", "macro_predictors", "wrds", "constituents", "fred",
#'   "stock_prices", "osap", "hf").
#' @param dataset Optional. The specific dataset to download within the domain.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, the full dataset or a subset is returned,
#'   depending on the dataset type.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, the full dataset or a subset is returned,
#'   depending on the dataset type.
#' @param type `r lifecycle::badge("deprecated")` Use `domain` and `dataset` instead.
#' @param ... Additional arguments passed to specific download functions depending on the `domain`.
#'   For instance, if `domain` is "constituents", this might include parameters specific to `download_data_constituents`.
#'
#' @returns A tibble with processed data, including dates and the relevant
#'   financial metrics, filtered by the specified date range.
#'
#' @export
#' @examples
#' \donttest{
#'   download_data("famafrench", "F-F_Research_Data_5_Factors_2x3_daily", "2000-01-01", "2020-12-31")
#'   download_data("macro_predictors", "monthly", "2000-01-01", "2020-12-31")
#'   download_data("constituents", index = "DAX")
#'   download_data("fred", series = c("GDP", "CPIAUCNS"))
#'   download_data("stock_prices", symbols = c("AAPL", "MSFT"))
#'   download_data("hf", "high_frequency_sp500", "2007-07-26", "2007-07-27")
#' }
download_data <- function(
  domain = NULL,
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
      what = "download_data(type)",
      details = "Use the `domain` and `dataset` arguments instead."
    )
    parsed <- parse_type_to_domain_dataset(type)
    domain <- parsed$domain
    dataset <- parsed$dataset
  }

  # Handle legacy type passed as domain argument
  if (!is.null(domain) && is_legacy_type(domain)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data(type)",
      details = paste0(
        "Column type should be replaced with domain and dataset. ",
        "Use `list_supported_types()` to see the mapping."
      )
    )
    parsed <- parse_type_to_domain_dataset(domain)
    domain <- parsed$domain
    dataset <- parsed$dataset
  }

  if (is.null(domain)) {
    cli::cli_abort("Argument {.arg domain} is required.")
  }

  check_supported_domain(domain)

  if (domain %in% c("famafrench", "factors_ff")) {
    processed_data <- download_data_factors_ff(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain %in% c("globalq", "factors_q")) {
    processed_data <- download_data_factors_q(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "macro_predictors") {
    processed_data <- download_data_macro_predictors(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "wrds") {
    processed_data <- download_data_wrds(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "constituents") {
    processed_data <- download_data_constituents(...)
  } else if (domain == "fred") {
    processed_data <- download_data_fred(
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "stock_prices") {
    processed_data <- download_data_stock_prices(
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "osap") {
    processed_data <- download_data_osap(
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "hf") {
    processed_data <- download_data_hf(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date
    )
  } else {
    cli::cli_abort("Unsupported domain: {.val {domain}}")
  }

  processed_data
}

#' Check if a string is a legacy type
#' @noRd
is_legacy_type <- function(x) {
  # Check all known legacy type patterns
  ff_types <- dplyr::bind_rows(
    list_supported_types_ff(),
    list_supported_types_ff_legacy()
  )
  q_types <- list_supported_types_q()
  macro_types <- list_supported_types_macro_predictors()
  wrds_types <- list_supported_types_wrds()
  other_types <- list_supported_types_other() |>
    dplyr::filter(type != "osap")

  all_types <- dplyr::bind_rows(
    ff_types,
    q_types,
    macro_types,
    wrds_types,
    other_types
  )

  x %in% all_types$type
}

#' Parse legacy type parameter to domain and dataset
#' @noRd
parse_type_to_domain_dataset <- function(type) {
  # Check Fama-French types (both current and legacy)
  ff_types <- dplyr::bind_rows(
    list_supported_types_ff(),
    list_supported_types_ff_legacy()
  )

  if (type %in% ff_types$type) {
    dataset_name <- ff_types$dataset_name[ff_types$type == type]
    return(list(domain = "factors_ff", dataset = dataset_name))
  }

  # Global Q factors: "factors_q5_*" -> domain = "factors_q"
  q_types <- list_supported_types_q()

  if (type %in% q_types$type) {
    dataset_name <- q_types$dataset_name[q_types$type == type]
    return(list(
      domain = "factors_q",
      dataset = gsub("\\.csv$", "", dataset_name)
    ))
  }

  # Macro predictors: "macro_predictors_*" -> domain = "macro_predictors"
  macro_types <- list_supported_types_macro_predictors()

  if (type %in% macro_types$type) {
    dataset <- sub("^macro_predictors_", "", type)
    return(list(domain = "macro_predictors", dataset = dataset))
  }

  # WRDS: "wrds_*" -> domain = "wrds"
  wrds_types <- list_supported_types_wrds()

  if (type %in% wrds_types$type) {
    dataset <- sub("^wrds_", "", type)
    return(list(domain = "wrds", dataset = dataset))
  }

  # High frequency: "hf_*" -> domain = "hf"
  if (grepl("^hf_", type)) {
    dataset <- sub("^hf_", "", type)
    return(list(domain = "hf", dataset = dataset))
  }

  # Simple domain-only types (no dataset component)
  simple_domains <- c("constituents", "fred", "stock_prices", "osap")
  if (type %in% simple_domains) {
    return(list(domain = type, dataset = NULL))
  }

  cli::cli_abort(c(
    "Cannot parse legacy type: {.val {type}}",
    "i" = "Use {.fn list_supported_types} to see available types."
  ))
}

#' Check if domain is supported
#' @noRd
check_supported_domain <- function(domain) {
  supported_domains <- c(
    "famafrench",
    "factors_ff",
    "globalq",
    "factors_q",
    "macro_predictors",
    "wrds",
    "constituents",
    "fred",
    "stock_prices",
    "osap",
    "hf"
  )

  if (!domain %in% supported_domains) {
    cli::cli_abort(c(
      "Unsupported domain: {.val {domain}}",
      "i" = "Supported domains: {.val {supported_domains}}"
    ))
  }
}
