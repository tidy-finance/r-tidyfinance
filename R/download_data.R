#' Download and Process Data Based on Domain and Dataset
#'
#' Downloads and processes data based on the specified domain (e.g.,
#' Fama-French factors, Global Q factors, or macro predictors), dataset,
#' and date range. This function checks if the specified domain is supported
#' and then delegates to the appropriate function for downloading and
#' processing the data.
#'
#' @param domain The domain of the dataset to download, using the names
#'   returned by [list_supported_datasets()]: `"Fama-French"`, `"Global Q"`,
#'   `"Goyal-Welch"`, `"WRDS"`, `"Pseudo Data"`, `"Index Constituents"`,
#'   `"FRED"`, `"Stock Prices"`, `"Open Source Asset Pricing"`,
#'   `"Global Factor Data"`, or `"Tidy Finance"`. Use `"Pseudo Data"` to obtain
#'   pseudo data with the
#'   same schema as `"WRDS"` for testing or rendering without a WRDS
#'   subscription. The previous machine-readable names (e.g., `"famafrench"`,
#'   `"wrds"`, `"pseudo"`, `"tidyfinance"`) are soft-deprecated but still
#'   accepted.
#' @param dataset Optional. The specific dataset to download within the
#'   domain.
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If not
#'   provided, the full dataset or a subset is returned, depending on the
#'   dataset type.
#' @param end_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the end date for the data. If not
#'   provided, the full dataset or a subset is returned, depending on the
#'   dataset type.
#' @param type `r lifecycle::badge("deprecated")` Use `domain` and
#'   `dataset` instead.
#' @param ... Additional arguments passed to specific download functions
#'   depending on the `domain`. For instance, if `domain` is
#'   `"Index Constituents"`, arguments are passed to
#'   `download_data_constituents()`. If `domain` is `"Global Factor Data"`,
#'   arguments such as `region`, `factors`, `frequency`, and `weighting` are
#'   passed to `download_data_jkp()`. If `domain` is `"Tidy Finance"` and
#'   `dataset` is `"factor_library"`, arguments are either filter inputs
#'   (e.g., `sorting_variable`, `rebalancing`, `fill_all`) or an explicit
#'   `ids` vector that bypasses the grid filter and downloads the
#'   specified portfolios directly via `download_factor_library_ids()`;
#'   see `download_data_huggingface()` for details.
#'
#' @returns A tibble with processed data, including dates and the relevant
#'   financial metrics, filtered by the specified date range.
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#' download_data(
#'   "Fama-French",
#'   "Fama/French 5 Factors (2x3) [Daily]",
#'   "2000-01-01",
#'   "2020-12-31"
#' )
#' download_data("Goyal-Welch", "monthly", "2000-01-01", "2020-12-31")
#' download_data("Index Constituents", index = "DAX")
#' download_data("FRED", series = c("GDP", "CPIAUCNS"))
#' download_data("Stock Prices", symbols = c("AAPL", "MSFT"))
#' download_data(
#'   "Tidy Finance",
#'   "risk_free",
#'   "2020-01-01",
#'   "2020-12-31"
#' )
#' download_data(
#'   "Tidy Finance",
#'   "high_frequency_sp500",
#'   "2007-07-26",
#'   "2007-07-27"
#' )
#' download_data(
#'   "Tidy Finance",
#'   "factor_library",
#'   sorting_variable = "52w",
#'   rebalancing = "annual"
#' )
#' download_data("Tidy Finance", "factor_library", ids = c(1L, 2L, 3L))
#' download_data("Tidy Finance", "factor_library_grid")
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
  } else if (!is.null(domain) && is_legacy_type(domain)) {
    # Handle legacy type passed as domain argument
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data(type)",
      details = paste0(
        "Column type should be replaced with domain and dataset. ",
        "Use `list_supported_datasets()` to see the mapping."
      )
    )
    parsed <- parse_type_to_domain_dataset(domain)
    domain <- parsed$domain
    dataset <- parsed$dataset
  } else if (!is.null(domain)) {
    # Map deprecated machine-readable domain names to canonical names
    domain <- resolve_domain_alias(domain)
  }

  if (is.null(domain)) {
    cli::cli_abort("Argument {.arg domain} is required.")
  }

  check_supported_domain(domain)

  if (domain == "Fama-French") {
    processed_data <- download_data_factors_ff(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "Global Q") {
    processed_data <- download_data_factors_q(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "Goyal-Welch") {
    processed_data <- download_data_macro_predictors(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "WRDS") {
    processed_data <- download_data_wrds(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "Pseudo Data") {
    processed_data <- simulate_pseudo_data(
      dataset = dataset,
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "Index Constituents") {
    processed_data <- download_data_constituents(...)
  } else if (domain == "FRED") {
    processed_data <- download_data_fred(
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "Stock Prices") {
    processed_data <- download_data_stock_prices(
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "Open Source Asset Pricing") {
    processed_data <- download_data_osap(
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "Global Factor Data") {
    processed_data <- download_data_jkp(
      start_date = start_date,
      end_date = end_date,
      ...
    )
  } else if (domain == "Tidy Finance") {
    if (!is.null(dataset) && dataset == "risk_free") {
      processed_data <- download_data_risk_free(
        start_date = start_date,
        end_date = end_date,
        ...
      )
    } else {
      processed_data <- download_data_huggingface(
        dataset = dataset,
        start_date = start_date,
        end_date = end_date,
        ...
      )
    }
  }

  processed_data
}
#' Check if a string is a legacy type
#' @noRd
is_legacy_type <- function(x) {
  # These strings are valid domain names, not legacy datasets
  valid_domains <- c("constituents", "fred", "stock_prices", "osap", "jkp")
  if (x %in% valid_domains) {
    return(FALSE)
  }

  # Check all known legacy type patterns
  ff_datasets <- dplyr::bind_rows(
    list_supported_datasets_ff(),
    list_supported_datasets_ff_legacy()
  )
  q_datasets <- list_supported_datasets_q()
  macro_datasets <- list_supported_datasets_macro_predictors()
  wrds_datasets <- list_supported_datasets_wrds()
  other_datasets <- list_supported_datasets_other() |>
    dplyr::filter(
      .data$domain != "Tidy Finance",
      !.data$type %in% c("osap", "jkp")
    )

  all_datasets <- dplyr::bind_rows(
    ff_datasets,
    q_datasets,
    macro_datasets,
    wrds_datasets,
    other_datasets
  )

  x %in% all_datasets$type
}

#' Parse legacy type parameter to domain and dataset
#' @noRd
parse_type_to_domain_dataset <- function(type) {
  # Check Fama-French datasets (both current and legacy)
  ff_datasets <- dplyr::bind_rows(
    list_supported_datasets_ff(),
    list_supported_datasets_ff_legacy()
  )

  if (type %in% ff_datasets$type) {
    dataset_name <- ff_datasets$dataset_name[ff_datasets$type == type]
    return(list(domain = "Fama-French", dataset = dataset_name))
  }

  # Global Q factors: "factors_q5_*" -> domain = "Global Q"
  q_datasets <- list_supported_datasets_q()

  if (type %in% q_datasets$type) {
    dataset_name <- q_datasets$dataset_name[q_datasets$type == type]
    return(list(
      domain = "Global Q",
      dataset = gsub("\\.csv$", "", dataset_name)
    ))
  }

  # Macro predictors: "macro_predictors_*" -> domain = "Goyal-Welch"
  macro_datasets <- list_supported_datasets_macro_predictors()

  if (type %in% macro_datasets$type) {
    dataset <- sub("^macro_predictors_", "", type)
    return(list(domain = "Goyal-Welch", dataset = dataset))
  }

  wrds_datasets <- list_supported_datasets_wrds()

  if (type %in% wrds_datasets$type) {
    dataset <- sub("^wrds_", "", type)
    return(list(domain = "WRDS", dataset = dataset))
  }

  # High frequency: "hf_*" -> domain = "Tidy Finance"
  if (grepl("^hf_", type)) {
    dataset <- sub("^hf_", "", type)
    return(list(domain = "Tidy Finance", dataset = dataset))
  }

  # Simple domain-only datasets (no dataset component)
  simple_domains <- c(
    "constituents" = "Index Constituents",
    "fred" = "FRED",
    "stock_prices" = "Stock Prices",
    "osap" = "Open Source Asset Pricing"
  )
  if (type %in% names(simple_domains)) {
    return(list(domain = unname(simple_domains[type]), dataset = NULL))
  }

  cli::cli_abort(c(
    "Cannot parse legacy type: {.val {type}}",
    "i" = "Use {.fn list_supported_datasets} to see available datasets."
  ))
}

#' Map deprecated machine-readable domain names to canonical domain names
#'
#' Earlier releases used short machine-readable domain names (e.g.,
#' `"famafrench"`, `"wrds"`, `"pseudo"`, `"tidyfinance"`). These are now
#' soft-deprecated in favour of the human-readable domain names returned by
#' [list_supported_datasets()]. This helper translates a deprecated alias to
#' its canonical name, emitting a deprecation warning. Unknown values are
#' returned unchanged so that `check_supported_domain()` can report them.
#' @noRd
resolve_domain_alias <- function(domain) {
  alias_map <- c(
    "famafrench" = "Fama-French",
    "factors_ff" = "Fama-French",
    "globalq" = "Global Q",
    "factors_q" = "Global Q",
    "macro_predictors" = "Goyal-Welch",
    "wrds" = "WRDS",
    "pseudo" = "Pseudo Data",
    "constituents" = "Index Constituents",
    "fred" = "FRED",
    "stock_prices" = "Stock Prices",
    "osap" = "Open Source Asset Pricing",
    "jkp" = "Global Factor Data",
    "tidyfinance" = "Tidy Finance"
  )

  if (domain %in% names(alias_map)) {
    canonical <- unname(alias_map[domain])
    lifecycle::deprecate_warn(
      when = "0.6.1",
      what = I(paste0('download_data(domain = "', domain, '")')),
      with = I(paste0('download_data(domain = "', canonical, '")'))
    )
    return(canonical)
  }

  domain
}

#' Check if domain is supported
#' @noRd
check_supported_domain <- function(domain) {
  supported_domains <- c(
    "Fama-French",
    "Global Q",
    "Goyal-Welch",
    "WRDS",
    "Pseudo Data",
    "Index Constituents",
    "FRED",
    "Stock Prices",
    "Open Source Asset Pricing",
    "Global Factor Data",
    "Tidy Finance"
  )

  if (!domain %in% supported_domains) {
    cli::cli_abort(c(
      "Unsupported domain: {.val {domain}}",
      "i" = "Supported domains: {.val {supported_domains}}"
    ))
  }
}
