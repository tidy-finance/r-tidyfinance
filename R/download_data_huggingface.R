#' List Parquet Files in a Hugging Face Dataset
#'
#' Query the Hugging Face Datasets API and return a tibble of files with a
#' `.parquet` suffix. The function follows pagination links returned in the
#' response `Link` header and returns path, size, and a resolved URL.
#'
#' @details
#' Uses httr2 to perform HTTP requests. Requires internet access and the
#' dataset to be publicly accessible or accessible with appropriate
#' authentication.
#'
#' @param organization Character(1). Hugging Face organization or user name.
#' @param dataset Character(1). Dataset name under the organization.
#'
#' @returns A tibble with columns: `path` (character), `size` (numeric), and
#'   `url` (character).
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \dontrun{
#' get_available_huggingface_files("voigtstefan", "sp500")
#' }
get_available_huggingface_files <- function(organization, dataset) {
  api_url <- paste0(
    "https://huggingface.co/api/datasets/",
    organization,
    "/",
    dataset,
    "/tree/main?recursive=1"
  )

  out <- tibble::tibble(NULL)
  next_url <- api_url

  repeat {
    resp <- httr2::request(next_url) |>
      httr2::req_user_agent("httr2") |>
      httr2::req_perform()

    body <- resp |>
      httr2::resp_body_string() |>
      jsonlite::fromJSON(simplifyDataFrame = TRUE) |>
      tibble::tibble() |>
      dplyr::filter(
        .data$type == "file" &
          grepl("\\.parquet$", .data$path, ignore.case = TRUE)
      ) |>
      dplyr::select("path", "size")

    out <- out |>
      dplyr::bind_rows(tibble::tibble(data = list(body)))
    link <- httr2::resp_headers(resp)$link
    if (is.null(link) || !grepl('rel="next"', link)) {
      break
    }
    next_url <- sub(".*<([^>]+)>; rel=\"next\".*", "\\1", link)
  }

  out |>
    tidyr::unnest("data") |>
    dplyr::mutate(
      url = glue::glue(
        "https://huggingface.co/datasets/",
        "{organization}/{dataset}/resolve/main/{path}"
      )
    )
}

#' Download data from a Hugging Face dataset
#'
#' Downloads data from a supported Hugging Face dataset. For
#' `"high_frequency_sp500"`, parquet files are filtered by date range and
#' row-bound. For `"factor_library"`, portfolio characteristics are selected via
#' `filter_factor_library_grid()` and the matching return data is downloaded.
#'
#' @param dataset Character(1). The dataset to download. Supported values are
#'   `"high_frequency_sp500"` and `"factor_library"`.
#' @param start_date Date or character. Start date (inclusive) in
#'   `"YYYY-MM-DD"` format. Only used for `"high_frequency_sp500"`.
#' @param end_date Date or character. End date (inclusive) in `"YYYY-MM-DD"`
#'   format. Only used for `"high_frequency_sp500"`.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#' @param ... For `dataset = "factor_library"`: named arguments used to filter
#'   the portfolio grid. Each argument takes the form `column = value`, where
#'   `value` may be a vector to match multiple levels. Optionally pass
#'   `fill_all = TRUE` to leave unspecified columns unrestricted (default:
#'   `FALSE`, i.e. unspecified columns are fixed at the defaults listed below).
#'   Passing an unrecognised column name raises an error listing the supported
#'   names. Ignored when `dataset != "factor_library"`. See the Details section
#'   for supported columns and their defaults.
#'
#' @details
#' **Note on `dataset = "factor_library"` defaults:** The defaults below reflect
#' one common portfolio construction choice, but may not suit every research
#' question. Always verify that the selected combination matches your intended
#' design.
#'
#' Supported columns and their defaults for `...`:
#'   \itemize{
#'     \item `sorting_variable`: **Required.** The firm characteristic used
#'       to sort stocks into portfolios (e.g., `"me"` for market equity,
#'       `"bm"` for book-to-market). No default is applied.
#'     \item `exclude_size` (defaults to `0.2`): Fraction of the smallest
#'       stocks (by market cap) excluded from the portfolio universe. `0.2`
#'       drops the bottom 20%.
#'     \item `exclude_financials` (defaults to `FALSE`): Whether to drop
#'       financial-sector stocks (SIC 6000-6999) from the universe.
#'     \item `exclude_utilities` (default: `FALSE`): Whether to drop
#'       utility-sector stocks (SIC 4900-4999) from the universe.
#'     \item `exclude_negative_earnings` (defaults to `FALSE`): Whether to
#'       drop firms with negative earnings before sorting.
#'     \item `sorting_variable_lag` (defaults to `"6m"`): Lag applied to the
#'       sorting variable before portfolio assignment (e.g., `"6m"` = 6-month
#'       lag).
#'     \item `rebalancing` (defaults to `"monthly"`): How frequently portfolios
#'       are reformed: `"monthly"` or `"annual"`.
#'     \item `breakpoints_main` (defaults to `10`): Number of quantile groups
#'       (e.g., `10` for decile portfolios).
#'     \item `sorting_method` (defaults to `"univariate"`): Whether portfolios
#'       are formed on a single sort (`"univariate"`) or a sequential double
#'       sort (`"sequential"`).
#'     \item `breakpoints_secondary` (defaults to `NA` for univariate sorts):
#'       Number of groups for the secondary sort variable. Required when
#'       `sorting_method` is not `"univariate"`.
#'     \item `breakpoints_exchanges` (defaults to: `"NYSE"`): Exchange(s) used
#'       to compute breakpoints. `"NYSE"` uses only NYSE-listed stocks to
#'       define quantile cutoffs (the conventional Fama-French approach).
#'     \item `breakpoints_min_size` (defaults to `NA`): Minimum market-cap
#'       threshold (in USD) applied when computing breakpoints. `NA` means no
#'       minimum-size screen is applied. Pass `NULL` to remove this filter
#'       entirely and include all portfolios regardless of size screen.
#'     \item `weighting_scheme` (defaults to `"VW"`): Return weighting within
#'       portfolios: `"VW"` for value-weighted or `"EW"` for equal-weighted.
#'   }
#'
#' @returns A tibble with the downloaded data. For `"high_frequency_sp500"`,
#'   contains 5-second aggregated orderbook snapshots filtered to the requested
#'   date range. For `"factor_library"`, contains portfolio return data joined
#'   with the full grid metadata for the matched portfolio IDs.
#'
#' @family download functions
#' @export
#'
#' @examples
#' \dontrun{
#'   download_data_huggingface(
#'     "high_frequency_sp500", "2007-07-26", "2007-07-27"
#'   )
#'   download_data_huggingface(
#'     "factor_library",
#'     sorting_variable = "52w",
#'     rebalancing = "annual"
#'   )
#'   download_data_huggingface(
#'     "factor_library", sorting_variable = "ag", fill_all = TRUE
#'   )
#' }
download_data_huggingface <- function(
  dataset = NULL,
  start_date = "2007-06-27",
  end_date = "2007-07-27",
  type = deprecated(),
  ...
) {
  # Handle explicit type argument
  if (lifecycle::is_present(type)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_huggingface(type)",
      details = "Use the `dataset` argument instead."
    )
    dataset <- sub("^hf_", "", type)
  }

  # Handle legacy type passed as dataset argument
  if (!is.null(dataset) && is_legacy_type_hf(dataset)) {
    lifecycle::deprecate_warn(
      when = "0.5.0",
      what = "download_data_huggingface(type)",
      details = paste0(
        "The `type` argument is deprecated. ",
        "Use `dataset` instead (e.g., 'high_frequency_sp500' instead of",
        "'hf_high_frequency_sp500')."
      )
    )
    dataset <- sub("^hf_", "", dataset)
  }

  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  check_supported_dataset_huggingface(dataset)

  if (dataset == "high_frequency_sp500") {
    organization <- "voigtstefan"
    dataset_name <- "sp500"

    date_pattern <- "date=([0-9]{4}-[0-9]{2}-[0-9]{2})"
    available_files <- get_available_huggingface_files(
      organization,
      dataset_name
    ) |>
      dplyr::mutate(
        date = as.Date(stringr::str_match(.data$path, date_pattern)[, 2])
      )

    tibble::tibble(
      date = seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
    ) |>
      dplyr::inner_join(available_files, dplyr::join_by(date)) |>
      dplyr::transmute(
        data = purrr::map(url, ~ arrow::read_parquet(.x))
      ) |>
      tidyr::unnest("data")
  } else if (dataset == "factor_library") {
    download_data_hugging_face_factor_library(...)
  } else {
    cli::cli_abort("Unsupported dataset: {.val {dataset}}")
  }
}

#' Check if a string is a legacy Hugging Face type
#' @noRd
is_legacy_type_hf <- function(x) {
  grepl("^hf_", x)
}

#' Check if Hugging Face dataset is supported
#' @noRd
check_supported_dataset_huggingface <- function(dataset) {
  supported_datasets <- c("high_frequency_sp500", "factor_library")

  if (!dataset %in% supported_datasets) {
    cli::cli_abort(c(
      "Unsupported Hugging Face dataset: {.val {dataset}}",
      "i" = "Supported datasets: {.val {supported_datasets}}"
    ))
  }
}

#' Filter the factor library grid and return matching IDs
#'
#' Downloads the `tidy-finance/factor-library-grid` from Hugging Face and
#' filters it by the provided column-value pairs. Any grid column not explicitly
#' specified is held at a sensible default when `fill_all = FALSE`. The function
#' returns the integer vector of matching portfolio IDs.
#'
#' @param ... Named arguments of the form `column = value` used to filter the
#'   grid. Supported columns and their defaults are:
#'   \describe{
#'     \item{`sorting_variable`}{No default.}
#'     \item{`exclude_size`}{`0.2`}
#'     \item{`exclude_financials`}{`FALSE`}
#'     \item{`exclude_utilities`}{`FALSE`}
#'     \item{`exclude_negative_earnings`}{`FALSE`}
#'     \item{`sorting_variable_lag`}{`"6m"`}
#'     \item{`rebalancing`}{`"monthly"`}
#'     \item{`breakpoints_main`}{`10`}
#'     \item{`sorting_method`}{`"univariate"`}
#'     \item{`breakpoints_secondary`}{`NA` for univariate sorts; required otherwise}
#'     \item{`breakpoints_exchanges`}{`"NYSE"`}
#'     \item{`breakpoints_min_size`}{`NA`}
#'     \item{`weighting_scheme`}{`"VW"`}
#'   }
#'   Each value can be a vector to match multiple levels.
#' @param fill_all Logical(1). If `FALSE` (default), columns not present in
#'   `...` are filled with their defaults before filtering. If `TRUE`, only the
#'   explicitly provided filters are applied and all other columns are left
#'   unrestricted.
#'
#' @return A vector of portfolio IDs matching the specified criteria. Returns
#'   an empty vector when no rows satisfy the filters.
#' @noRd
filter_factor_library_grid <- function(..., fill_all = FALSE) {
  filters <- list(...)

  defaults <- list(
    exclude_size = 0.2,
    exclude_financials = FALSE,
    exclude_utilities = FALSE,
    exclude_negative_earnings = FALSE,
    sorting_variable_lag = "6m",
    rebalancing = "monthly",
    breakpoints_main = 10,
    sorting_method = "univariate",
    breakpoints_secondary = NULL,
    breakpoints_exchanges = "NYSE",
    breakpoints_min_size = NA_real_,
    weighting_scheme = "VW"
  )

  supported_names <- c("sorting_variable", names(defaults))
  unsupported <- setdiff(names(filters), supported_names)
  if (length(unsupported) > 0) {
    cli::cli_abort(c(
      "{length(unsupported)} unsupported filter name{?s}: {.val {unsupported}}",
      "i" = "Supported filters: {.val {supported_names}}"
    ))
  }

  if (!fill_all) {
    for (col in names(defaults)) {
      if (!col %in% names(filters)) {
        filters[col] <- list(defaults[[col]])
      }
    }

    if (is.null(filters[["breakpoints_secondary"]])) {
      sorting_methods <- filters[["sorting_method"]]
      if (!all(sorting_methods == "univariate")) {
        cli::cli_abort(c(
          "{.arg breakpoints_secondary} must be specified for bivariate sorts.",
          "i" = "Provide a value for {.arg breakpoints_secondary} or use {.code fill_all = TRUE} to skip all defaults."
        ))
      }
      filters["breakpoints_secondary"] <- list(NA_real_)
    }
  }

  result <- get_available_huggingface_files(
    "tidy-finance",
    "factor-library-grid"
  ) |>
    dplyr::pull(.data$url) |>
    arrow::read_parquet() |>
    dplyr::mutate(
      sorting_variable = stringr::str_replace(.data$sorting_variable, "sv_", "")
    )

  filters <- purrr::compact(filters)

  for (col in names(filters)) {
    result <- dplyr::filter(result, .data[[col]] %in% filters[[col]])
  }

  dplyr::pull(result, .data$id)
}

#' Download factor library returns for a vector of portfolio IDs
#'
#' Given a vector of portfolio IDs from the `tidy-finance/factor-library-grid`,
#' this function downloads the corresponding return data from the
#' `tidy-finance/factor-library` dataset on Hugging Face. It identifies the
#' unique `(sorting_variable, sorting_variable_lag)` combinations for the
#' requested IDs, downloads one parquet file per combination in full, and
#' then inner-joins to retain only the requested IDs. The grid metadata is
#' joined back onto the result.
#'
#' Raises an error if `ids` is empty or contains IDs that cannot be matched to
#' a parquet file (listing the affected IDs and their key columns).
#'
#' @param ids Vector of portfolio IDs to download, as returned by
#'   `filter_factor_library_grid()`.
#'
#' @return A tibble of portfolio returns with the grid metadata columns for the
#'   requested IDs appended.
#' @noRd
download_factor_library_ids <- function(ids) {
  organization <- "tidy-finance"
  dataset_name <- "factor-library"

  available_files <- get_available_huggingface_files(
    organization,
    dataset_name
  ) |>
    tidyr::extract(
      col = "path",
      into = c("sorting_variable", "sorting_variable_lag"),
      regex = "sorting_variable=([^/]+)/sorting_variable_lag=([^/]+)/",
      remove = FALSE
    )

  id_values <- data.frame(id = ids)

  id_grid <- get_available_huggingface_files(
    organization,
    "factor-library-grid"
  ) |>
    dplyr::pull(.data$url) |>
    arrow::read_parquet() |>
    dplyr::inner_join(id_values, dplyr::join_by(id)) |>
    dplyr::mutate(
      sorting_variable = stringr::str_replace(.data$sorting_variable, "sv_", "")
    ) |>
    dplyr::left_join(
      available_files,
      dplyr::join_by(sorting_variable, sorting_variable_lag)
    )

  relevant_urls <- id_grid |>
    dplyr::distinct(url, sorting_variable, sorting_variable_lag)

  if (nrow(relevant_urls) == 0) {
    cli::cli_abort(c(
      "No parquet files found for the requested portfolio IDs.",
      "i" = paste(
        "Check that the provided {.arg ids} are valid",
        "and exist in the factor library grid."
      )
    ))
  }

  missing_urls <- relevant_urls |>
    dplyr::filter(is.na(.data$url))

  if (nrow(missing_urls) > 0) {
    missing_keys <- missing_urls |> # nolint: object_usage_linter
      dplyr::inner_join(
        id_grid |>
          dplyr::select("id", "sorting_variable", "sorting_variable_lag"),
        dplyr::join_by(sorting_variable, sorting_variable_lag)
      ) |>
      dplyr::mutate(
        key = paste0(
          "id=",
          .data$id,
          " (",
          .data$sorting_variable,
          " / ",
          .data$sorting_variable_lag,
          ")"
        )
      ) |>
      dplyr::pull(.data$key)

    cli::cli_abort(c(
      "No parquet file found for {length(missing_keys)} portfolio ID{?s}.",
      "x" = "Affected ID{?s}: {.val {missing_keys}}",
      "i" = paste(
        "Check that the {.arg sorting_variable} and",
        "{.arg sorting_variable_lag} values exist in the factor library."
      )
    ))
  }

  relevant_files <- relevant_urls$url |>
    purrr::map(~ arrow::read_parquet(.x)) |>
    dplyr::bind_rows()

  relevant_files |>
    dplyr::inner_join(
      id_grid |> dplyr::select(-c(url, path, size)),
      by = dplyr::join_by(id)
    )
}

#' Download factor library data from Hugging Face
#'
#' A thin wrapper that combines `filter_factor_library_grid()` and
#' `download_factor_library_ids()`: it resolves matching portfolio IDs
#' from the grid and then downloads the corresponding return data.
#'
#' @param ... Named filter arguments forwarded to
#'   `filter_factor_library_grid()`. See
#'   `filter_factor_library_grid()` for the full list of supported
#'   columns and their defaults.
#' @param fill_all Logical(1). Forwarded to
#'   `filter_factor_library_grid()`. When `TRUE`, columns not
#'   specified in `...` are left unrestricted rather than set to
#'   their defaults.
#'
#' @return A tibble of portfolio returns with grid metadata columns
#'   appended, one row per portfolio-period observation for the
#'   matched IDs.
#' @noRd
download_data_hugging_face_factor_library <- function(..., fill_all = FALSE) {
  ids <- filter_factor_library_grid(..., fill_all = fill_all)
  download_factor_library_ids(ids)
}
