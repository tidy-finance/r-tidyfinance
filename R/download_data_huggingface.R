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
#' `filter_factor_library_grid()`, the matching return data is downloaded, and
#' the result is filtered to `start_date`/`end_date` when both are supplied.
#' For `"factor_library_grid"`, the grid itself is returned via
#' [download_factor_library_grid()].
#'
#' @param dataset Character(1). The dataset to download. Supported values are
#'   `"high_frequency_sp500"`, `"factor_library"`, and `"factor_library_grid"`.
#' @param start_date Date or character. Start date (inclusive) in
#'   `"YYYY-MM-DD"` format. Used for `"high_frequency_sp500"` and
#'   `"factor_library"`. When omitted for `"factor_library"`, the full return
#'   history is returned; `"high_frequency_sp500"` falls back to a built-in
#'   sample window.
#' @param end_date Date or character. End date (inclusive) in `"YYYY-MM-DD"`
#'   format. See `start_date`.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#' @param ... For `dataset = "factor_library"`: either named arguments used
#'   to filter the portfolio grid, or `ids = <vector>` to bypass the grid
#'   filter and download specific portfolios directly via
#'   [download_factor_library_ids()]. Filter arguments take the form
#'   `column = value`, where `value` may be a vector to match multiple
#'   levels. Optionally pass `fill_all = TRUE` to leave unspecified columns
#'   unrestricted (default: `FALSE`, i.e. unspecified columns are fixed at
#'   the defaults listed below). Passing `NULL` for any parameter removes
#'   that filter entirely, returning all values for that column (e.g.,
#'   `min_size_quantile = NULL` includes all size groups). Passing an
#'   unrecognised column name raises an error listing the supported names.
#'   `ids` cannot be combined with filter arguments. Ignored when
#'   `dataset != "factor_library"`. See the Details section for supported
#'   columns and their defaults.
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
#'     \item `min_size_quantile` (defaults to `0.2`): Fraction of the smallest
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
#'     \item `n_portfolios_main` (defaults to `10`): Number of quantile groups
#'       (e.g., `10` for decile portfolios).
#'     \item `sorting_method` (defaults to `"univariate"`): Whether portfolios
#'       are formed on a single sort (`"univariate"`) or a sequential double
#'       sort (`"sequential"`).
#'     \item `n_portfolios_secondary` (defaults to `NULL`): Number of groups
#'       for the secondary sort variable.
#'       Required when `sorting_method` is not `"univariate"`.
#'     \item `breakpoints_exchanges` (defaults to: `"NYSE"`): Exchange(s) used
#'       to compute breakpoints. `"NYSE"` uses only NYSE-listed stocks to
#'       define quantile cutoffs (the conventional Fama-French approach).
#'     \item `breakpoints_min_size_threshold` (defaults to `NULL`): Minimum
#'       market-cap threshold (in USD) applied when computing breakpoints.
#'       `NULL` means no minimum-size screen is applied.
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
#'   download_data_huggingface(
#'     "factor_library",
#'     sorting_variable = "me",
#'     start_date = "2000-01-01",
#'     end_date = "2020-12-31"
#'   )
#'   download_data_huggingface("factor_library", ids = c(1L, 2L, 3L))
#' }
download_data_huggingface <- function(
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

  if (dataset == "factor_library_grid") {
    return(download_factor_library_grid())
  }

  if (dataset == "high_frequency_sp500") {
    if (is.null(start_date)) {
      start_date <- "2007-06-27"
    }
    if (is.null(end_date)) {
      end_date <- "2007-07-27"
    }

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
    download_data_hugging_face_factor_library(
      ...,
      start_date = start_date,
      end_date = end_date
    )
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
  supported_datasets <- c(
    "high_frequency_sp500",
    "factor_library",
    "factor_library_grid"
  )

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
#'     \item{`min_size_quantile`}{`0.2`}
#'     \item{`exclude_financials`}{`FALSE`}
#'     \item{`exclude_utilities`}{`FALSE`}
#'     \item{`exclude_negative_earnings`}{`FALSE`}
#'     \item{`sorting_variable_lag`}{`"6m"`}
#'     \item{`rebalancing`}{`"monthly"`}
#'     \item{`n_portfolios_main`}{`10`}
#'     \item{`sorting_method`}{`"univariate"`}
#'     \item{`n_portfolios_secondary`}{`NULL` for univariate sorts;
#'       required otherwise}
#'     \item{`breakpoints_exchanges`}{`"NYSE"`}
#'     \item{`breakpoints_min_size_threshold`}{`NULL`}
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
    min_size_quantile = 0.2,
    exclude_financials = FALSE,
    exclude_utilities = FALSE,
    exclude_negative_earnings = FALSE,
    sorting_variable_lag = "6m",
    rebalancing = "monthly",
    n_portfolios_main = 10,
    sorting_method = "univariate",
    n_portfolios_secondary = NULL,
    breakpoints_exchanges = "NYSE",
    breakpoints_min_size_threshold = NA_real_,
    weighting_scheme = "VW"
  )

  supported_names <- c("sorting_variable", names(defaults))
  unsupported <- setdiff(names(filters), supported_names)
  if (length(unsupported) > 0) {
    cli::cli_abort(c(
      paste0(
        "{length(unsupported)} unsupported filter ",
        "name{?s}: {.val {unsupported}}"
      ),
      "i" = "Supported filters: {.val {supported_names}}"
    ))
  }

  if (!fill_all) {
    for (col in names(defaults)) {
      if (!col %in% names(filters)) {
        filters[col] <- list(defaults[[col]])
      }
    }

    if (is.null(filters[["n_portfolios_secondary"]])) {
      sorting_methods <- filters[["sorting_method"]]
      if (!all(sorting_methods == "univariate")) {
        cli::cli_abort(c(
          paste0(
            "{.arg n_portfolios_secondary} must be specified ",
            "for bivariate sorts."
          ),
          "i" = paste(
            "Provide a value for {.arg n_portfolios_secondary} or",
            "use {.code fill_all = TRUE} to skip all defaults."
          )
        ))
      }
      filters["n_portfolios_secondary"] <- list(NA_real_)
    }
  }

  result <- download_factor_library_grid() |>
    dplyr::mutate(
      sorting_variable = stringr::str_replace(.data$sorting_variable, "sv_", "")
    )

  filters <- purrr::compact(filters)

  for (col in names(filters)) {
    result <- dplyr::filter(result, .data[[col]] %in% filters[[col]])
  }

  dplyr::pull(result, .data$id)
}

#' Download the Factor Library Grid from Hugging Face
#'
#' Returns the `tidy-finance/factor-library-grid` dataset, which describes
#' every portfolio construction available in the factor library (one row per
#' construction, identified by `id`). Use the returned tibble to discover
#' which `(sorting_variable, weighting_scheme, rebalancing, ...)` combinations
#' exist before requesting their returns with
#' [download_factor_library_ids()].
#'
#' Equivalent to calling
#' `download_data("tidyfinance", "factor_library_grid")`.
#'
#' @returns A tibble with one row per portfolio construction in the factor
#'   library, including the integer `id` column used by
#'   [download_factor_library_ids()].
#'
#' @family download functions
#' @export
#'
#' @examples
#' \dontrun{
#'   download_factor_library_grid()
#' }
download_factor_library_grid <- function() {
  get_available_huggingface_files(
    "tidy-finance",
    "factor-library-grid"
  ) |>
    dplyr::pull(.data$url) |>
    arrow::read_parquet()
}

#' Download factor library returns for a vector of portfolio IDs
#'
#' Given a vector of portfolio IDs from the `tidy-finance/factor-library-grid`
#' Hugging Face dataset, downloads the corresponding return data from the
#' `tidy-finance/factor-library` dataset on Hugging Face. The function
#' identifies the unique `(sorting_variable, sorting_variable_lag,
#' sorting_method, n_portfolios_main)` combinations for the requested IDs,
#' downloads one parquet file per combination in full, and then inner-joins
#' to retain only the requested IDs. The grid metadata is joined back onto
#' the result.
#'
#' Use this function when you already know the portfolio IDs you want (for
#' example, from a previous call to [download_data_huggingface()] with
#' `dataset = "factor_library"`). To resolve IDs from filter criteria
#' (sorting variable, weighting scheme, breakpoints, etc.) and download in
#' a single call, use [download_data_huggingface()] instead.
#'
#' Raises an error if `ids` is empty or contains IDs that cannot be matched
#' to a parquet file (listing the affected IDs and their key columns).
#'
#' @param ids Integer or numeric vector of portfolio IDs to download. IDs
#'   correspond to rows of the `tidy-finance/factor-library-grid` dataset.
#'
#' @returns A tibble of portfolio returns with the grid metadata columns for
#'   the requested IDs appended.
#'
#' @family download functions
#' @export
#'
#' @examples
#' \dontrun{
#'   download_factor_library_ids(c(1L, 2L, 3L))
#' }
download_factor_library_ids <- function(ids) {
  organization <- "tidy-finance"
  dataset_name <- "factor-library"

  available_files <- get_available_huggingface_files(
    organization,
    dataset_name
  ) |>
    tidyr::extract(
      col = "path",
      into = c(
        "sorting_variable",
        "sorting_variable_lag",
        "sorting_method",
        "n_portfolios_main"
      ),
      regex = paste0(
        "sorting_variable=([^/]+)/sorting_variable_lag=([^/]+)",
        "/sorting_method=([^/]+)/n_portfolios_main=([^/]+)/"
      ),
      remove = FALSE
    )

  id_values <- data.frame(id = ids)

  id_grid <- download_factor_library_grid() |>
    dplyr::inner_join(id_values, dplyr::join_by(id)) |>
    dplyr::mutate(
      sorting_variable = stringr::str_replace(
        .data$sorting_variable,
        "sv_",
        ""
      ),
      n_portfolios_main = as.character(.data$n_portfolios_main)
    ) |>
    dplyr::left_join(
      available_files,
      dplyr::join_by(
        sorting_variable,
        sorting_variable_lag,
        sorting_method,
        n_portfolios_main
      )
    )

  relevant_urls <- id_grid |>
    dplyr::distinct(
      .data$url,
      .data$sorting_variable,
      .data$sorting_variable_lag,
      .data$sorting_method,
      .data$n_portfolios_main
    )

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
          dplyr::select(
            "id",
            "sorting_variable",
            "sorting_variable_lag",
            "sorting_method",
            "n_portfolios_main"
          ),
        dplyr::join_by(
          sorting_variable,
          sorting_variable_lag,
          sorting_method,
          n_portfolios_main
        )
      ) |>
      dplyr::mutate(
        key = paste0(
          "id=",
          .data$id,
          " (",
          .data$sorting_variable,
          " / ",
          .data$sorting_variable_lag,
          " / ",
          .data$sorting_method,
          " / ",
          .data$n_portfolios_main,
          ")"
        )
      ) |>
      dplyr::pull(.data$key)

    cli::cli_abort(c(
      "No parquet file found for {length(missing_keys)} portfolio ID{?s}.",
      "x" = "Affected ID{?s}: {.val {missing_keys}}",
      "i" = paste(
        "Check that the {.arg sorting_variable},",
        "{.arg sorting_variable_lag}, {.arg sorting_method},",
        "and {.arg n_portfolios_main} values exist in the factor library."
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
#' A thin wrapper around `download_factor_library_ids()` that either takes
#' an explicit `ids` vector or resolves matching portfolio IDs from the
#' grid via `filter_factor_library_grid()` before downloading the
#' corresponding return data.
#'
#' @param ... Named filter arguments forwarded to
#'   `filter_factor_library_grid()`. See
#'   `filter_factor_library_grid()` for the full list of supported
#'   columns and their defaults. Ignored when `ids` is provided.
#' @param ids Optional integer or numeric vector of portfolio IDs. When
#'   supplied, the filter arguments in `...` are not allowed and the helper
#'   delegates directly to `download_factor_library_ids()`.
#' @param fill_all Logical(1). Forwarded to
#'   `filter_factor_library_grid()`. When `TRUE`, columns not
#'   specified in `...` are left unrestricted rather than set to
#'   their defaults. Ignored when `ids` is provided.
#' @param start_date Optional. A character string or Date object in
#'   `"YYYY-MM-DD"` format. When both `start_date` and `end_date` are
#'   provided, the returns are filtered to the inclusive range. When either
#'   is `NULL`, the full history is returned.
#' @param end_date Optional. A character string or Date object in
#'   `"YYYY-MM-DD"` format. See `start_date`.
#'
#' @return A tibble of portfolio returns with grid metadata columns
#'   appended, one row per portfolio-period observation for the
#'   matched IDs.
#' @noRd
download_data_hugging_face_factor_library <- function(
  ...,
  ids = NULL,
  fill_all = FALSE,
  start_date = NULL,
  end_date = NULL
) {
  if (!is.null(ids)) {
    if (...length() > 0) {
      cli::cli_abort(c(
        "{.arg ids} cannot be combined with filter arguments.",
        "i" = paste(
          "Pass {.arg ids} alone to download specific portfolios, or",
          "use filter arguments (e.g. {.arg sorting_variable}) to",
          "resolve IDs from the grid."
        )
      ))
    }
  } else {
    ids <- filter_factor_library_grid(..., fill_all = fill_all)
  }

  dates <- validate_dates(start_date, end_date)

  returns <- download_factor_library_ids(ids)

  if (!is.null(dates$start_date) && !is.null(dates$end_date)) {
    returns <- dplyr::filter(
      returns,
      dplyr::between(.data$date, dates$start_date, dates$end_date)
    )
  }

  returns
}
