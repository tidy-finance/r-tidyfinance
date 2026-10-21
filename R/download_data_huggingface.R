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
      url = paste0(
        "https://huggingface.co/datasets/",
        organization, "/", dataset, "/resolve/main/", .data$path
      )
    )
}

#' Download data from a Hugging Face dataset
#'
#' Downloads data from a supported Hugging Face dataset. For
#' `"high_frequency_sp500"`, parquet files are filtered by date range and
#' row-bound. For `"factor_library"` and `"factor_library_costs"`, portfolio
#' characteristics are selected via `filter_factor_library_grid()`, the
#' matching returns or trading costs are downloaded, and the result is filtered
#' to `start_date`/`end_date` when both are supplied. For
#' `"factor_library_grid"`, the grid itself is returned via
#' [download_factor_library_grid()]. For `"factor_library_persistence"`, the
#' persistence of the sorting variables is returned.
#'
#' @param dataset Character(1). The dataset to download. Supported values are
#'   `"high_frequency_sp500"`, `"factor_library"`, `"factor_library_costs"`,
#'   `"factor_library_grid"`, and `"factor_library_persistence"`.
#' @param start_date Date or character. Start date (inclusive) in
#'   `"YYYY-MM-DD"` format. Used for `"high_frequency_sp500"`,
#'   `"factor_library"`, `"factor_library_costs"`, and
#'   `"factor_library_persistence"`. When omitted for the factor library, the
#'   full history is returned; `"high_frequency_sp500"` falls back to a
#'   built-in sample window.
#' @param end_date Date or character. End date (inclusive) in `"YYYY-MM-DD"`
#'   format. See `start_date`.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#' @param ... For `dataset = "factor_library"` and `"factor_library_costs"`:
#'   either named arguments used to filter the portfolio grid, or
#'   `ids = <vector>` to bypass the grid filter and download specific
#'   portfolios directly. Filter arguments take the form
#'   `column = value`, where `value` may be a vector to match multiple
#'   levels. Optionally pass `fill_all = TRUE` to leave unspecified columns
#'   unrestricted (default: `FALSE`, i.e. unspecified columns are fixed at
#'   the defaults listed below). Passing `NULL` for any parameter removes
#'   that filter entirely, returning all values for that column (e.g.,
#'   `min_size_quantile = NULL` includes all size groups). Passing an
#'   unrecognised column name raises an error listing the supported names.
#'   `ids` cannot be combined with filter arguments. See the Details section
#'   for supported columns and their defaults.
#'
#'   For all four factor library datasets: `revision` and `grid_revision`
#'   select the release, see the section Revisions. For
#'   `"factor_library_grid"`: `refresh`, see [download_factor_library_grid()].
#'   For `"factor_library_persistence"`: optionally `sorting_variable`, a
#'   vector of sorting variables; all are returned without it.
#'
#' @section Revisions:
#' The factor library is published in two Hugging Face datasets.
#' `tidy-finance/factor-library` holds the returns and the trading costs.
#' `tidy-finance/factor-library-grid` holds the grid and the persistence. Each
#' release is a commit of both datasets. `revision` selects the commit of
#' `tidy-finance/factor-library`, and `grid_revision` the commit of
#' `tidy-finance/factor-library-grid`. A revision is a commit hash, a branch,
#' or a tag, and both default to `"main"`, the latest release. The grid and the
#' persistence take only `grid_revision`.
#'
#' To make a download reproducible, pin both to the commits of one release.
#' The IDs of one release need not match the grid of another.
#'
#' @section Download limits:
#' Hugging Face allows anonymous users 3,000 file downloads per five minutes
#' and IP address. The returns and the costs are stored in files of 1,000
#' portfolio IDs, so all portfolios of one sorting variable take about 27
#' files of each. The factor library downloads up to 16 files at once and 8
#' files per second on average, also across calls, which keeps it below the
#' limit. If Hugging Face still refuses a download with HTTP 429, e.g. because
#' of other downloads from the same IP address, the download waits until the
#' limit resets and tries again, twice at most.
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
#'       to sort stocks into portfolios, named like the Open Source Asset
#'       Pricing signals (e.g., `"size"` for market equity, `"bm"` for
#'       book-to-market). See [download_factor_library_grid()] for all
#'       values. No default is applied.
#'     \item `min_size_quantile` (defaults to `0.2`): Fraction of the smallest
#'       stocks (by market cap) excluded from the portfolio universe. `0.2`
#'       drops the bottom 20%.
#'     \item `exclude_financials` (defaults to `FALSE`): Whether to drop
#'       financial-sector stocks (SIC 6000-6799) from the universe.
#'     \item `exclude_utilities` (default: `FALSE`): Whether to drop
#'       utility-sector stocks (SIC 4900-4999) from the universe.
#'     \item `exclude_negative_earnings` (defaults to `FALSE`): Whether to
#'       drop firms with negative earnings before sorting.
#'     \item `sorting_variable_lag` (defaults to `"6m"`): Lag applied to the
#'       sorting variable before portfolio assignment: `"1m"` (the timing of
#'       Open Source Asset Pricing), `"3m"`, `"6m"`, or `"ff"`
#'       (Fama-French).
#'     \item `rebalancing` (defaults to `"monthly"`): How frequently portfolios
#'       are reformed: `"monthly"` or `"annual"`.
#'     \item `n_portfolios_main` (defaults to `10`): Number of quantile groups
#'       (e.g., `10` for decile portfolios).
#'     \item `sorting_method` (defaults to `"univariate"`): Whether portfolios
#'       are formed on a single sort (`"univariate"`) or on a double sort with
#'       size as the second variable (`"bivariate-dependent"` or
#'       `"bivariate-independent"`).
#'     \item `n_portfolios_secondary` (defaults to `NULL`): Number of size
#'       groups for the secondary sort.
#'       Required when `sorting_method` is not `"univariate"`.
#'     \item `breakpoints_exchanges` (defaults to: `"NYSE"`): Exchange(s) used
#'       to compute breakpoints. `"NYSE"` uses only NYSE-listed stocks to
#'       define quantile cutoffs (the conventional Fama-French approach).
#'     \item `breakpoints_min_size_threshold` (defaults to `NA`): Minimum
#'       size quantile of the stocks that set the main breakpoints (e.g.,
#'       `0.2`). `NA` means no minimum-size screen is applied.
#'     \item `weighting_scheme` (defaults to `"VW"`): Return weighting within
#'       portfolios: `"VW"` for value-weighted, `"EW"` for equal-weighted, or
#'       `"capped VW"` for value-weighted with capped weights.
#'   }
#'
#' @returns A tibble with the downloaded data. For `"high_frequency_sp500"`,
#'   contains 5-second aggregated orderbook snapshots filtered to the requested
#'   date range. For `"factor_library"`, contains the columns `id`, `date`, and
#'   `ret` joined with the full grid metadata for the matched portfolio IDs.
#'   For `"factor_library_costs"`, contains the columns `id`, `date`,
#'   `turnover`, `cost`, and `microcap_share` joined with the grid metadata in
#'   the same way. The costs start in January 1994; `turnover` and `cost` are
#'   `0` in months without trades and `microcap_share` is `NA` in months
#'   without holdings. For `"factor_library_persistence"`, contains the columns
#'   `sorting_variable`, `sorting_variable_lag`, `rebalancing`, `date`, and
#'   `persistence`, the Spearman correlation across stocks between the value
#'   of the sorting variable under the lag and rebalancing and its up-to-date
#'   value.
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
#'     sorting_variable = "high52",
#'     rebalancing = "annual"
#'   )
#'   download_data_huggingface(
#'     "factor_library", sorting_variable = "assetgrowth", fill_all = TRUE
#'   )
#'   download_data_huggingface(
#'     "factor_library",
#'     sorting_variable = "size",
#'     start_date = "2000-01-01",
#'     end_date = "2020-12-31"
#'   )
#'   download_data_huggingface("factor_library", ids = c(1L, 2L, 3L))
#'
#'   # All portfolios of one sorting variable from the release of
#'   # 20 October 2026
#'   download_data_huggingface(
#'     "factor_library_costs",
#'     sorting_variable = "bm",
#'     fill_all = TRUE,
#'     revision = "e47bd85accd9b9b168ac7f478f12fe89ba5c2430",
#'     grid_revision = "a5a74cdf1df0c2c3d1ce3d7bf6403a8386fbf99d"
#'   )
#'   download_data_huggingface(
#'     "factor_library_persistence",
#'     sorting_variable = "bm",
#'     grid_revision = "a5a74cdf1df0c2c3d1ce3d7bf6403a8386fbf99d"
#'   )
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
    deprecate_legacy_dataset(
      dataset,
      sub("^hf_", "", dataset)
    )
    dataset <- sub("^hf_", "", dataset)
  }

  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  check_supported_dataset_huggingface(dataset)

  # Both are in tidy-finance/factor-library-grid, so `revision`, which pins
  # tidy-finance/factor-library, would pin nothing
  if (
    dataset %in% c("factor_library_grid", "factor_library_persistence") &&
      "revision" %in% ...names()
  ) {
    cli::cli_abort(c(
      "{.arg revision} does not apply to {.val {dataset}}.",
      "i" = paste(
        "{.arg revision} pins the returns and the costs. Pin the grid and",
        "the persistence with {.arg grid_revision}."
      )
    ))
  }

  if (dataset == "factor_library_grid") {
    return(download_factor_library_grid(...))
  }

  if (dataset == "factor_library_persistence") {
    return(download_factor_library_persistence(
      ...,
      start_date = start_date,
      end_date = end_date
    ))
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
        date = as.Date(extract_capture(.data$path, date_pattern))
      )

    tibble::tibble(
      date = seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
    ) |>
      dplyr::inner_join(available_files, by = "date") |>
      dplyr::transmute(
        data = purrr::map(.data$url, ~ read_parquet_url(.x))
      ) |>
      tidyr::unnest("data")
  } else if (dataset %in% c("factor_library", "factor_library_costs")) {
    download_data_hugging_face_factor_library(
      ...,
      table = if (dataset == "factor_library") "returns" else "costs",
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

#' Extract the first capture group of a regex pattern from a character vector
#'
#' Vectorised replacement for `stringr::str_match(x, pattern)[, 2]` using
#' base R `regmatches()` + `regexec()`. Returns `NA_character_` for
#' elements that do not match.
#'
#' @noRd
extract_capture <- function(x, pattern) {
  matches <- regmatches(x, regexec(pattern, x))
  vapply(
    matches,
    function(m) if (length(m) >= 2) m[2] else NA_character_,
    character(1)
  )
}

#' Check if Hugging Face dataset is supported
#' @noRd
check_supported_dataset_huggingface <- function(dataset) {
  supported_datasets <- c(
    "high_frequency_sp500",
    "factor_library",
    "factor_library_costs",
    "factor_library_grid",
    "factor_library_persistence"
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
#' @param grid_revision Character(1). The revision of the grid, see
#'   [download_factor_library_grid()].
#'
#' @return A vector of portfolio IDs matching the specified criteria. Returns
#'   an empty vector when no rows satisfy the filters.
#' @noRd
filter_factor_library_grid <- function(
  ...,
  fill_all = FALSE,
  grid_revision = "main"
) {
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

  # Capture the names the user actually supplied before filling defaults.
  # `list(x = NULL)` keeps `x` in `names()`, so this distinguishes a column
  # the user omitted from one they explicitly set to `NULL`.
  user_supplied <- names(filters)

  if (!fill_all) {
    for (col in names(defaults)) {
      if (!col %in% user_supplied) {
        filters[col] <- list(defaults[[col]])
      }
    }

    # Only default `n_portfolios_secondary` to NA when the user did not
    # supply the argument at all. An explicit `NULL` removes the filter
    # (returning all values), consistent with every other column.
    if (!("n_portfolios_secondary" %in% user_supplied)) {
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

  result <- download_factor_library_grid(grid_revision = grid_revision)

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
#' `download_data("Tidy Finance", "factor_library_grid")`.
#'
#' The grid is downloaded once per R session and revision and cached in
#' memory, so repeated calls (including those made by
#' [download_factor_library_ids()] and
#' `download_data("Tidy Finance", "factor_library", ...)`) reuse it. Set
#' `refresh = TRUE` to download it again. Each revision is cached separately
#' and takes about 450 MB of memory.
#'
#' @param refresh Logical(1). If `TRUE`, the grid is downloaded again and
#'   replaces the copy cached in the current session. Defaults to `FALSE`.
#' @param grid_revision Character(1). The revision of
#'   `tidy-finance/factor-library-grid` to read: a commit hash, a branch, or a
#'   tag. Defaults to `"main"`, the latest release. Pin it to the release of
#'   the returns, see the section Revisions of [download_data_huggingface()].
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
#'   download_factor_library_grid(
#'     grid_revision = "a5a74cdf1df0c2c3d1ce3d7bf6403a8386fbf99d"
#'   )
#' }
download_factor_library_grid <- function(
  refresh = FALSE,
  grid_revision = "main"
) {
  if (!isFALSE(refresh) && !isTRUE(refresh)) {
    cli::cli_abort("{.arg refresh} must be {.code TRUE} or {.code FALSE}.")
  }
  check_revision(grid_revision, "grid_revision")

  if (refresh || is.null(factor_library_cache$grids[[grid_revision]])) {
    # The repo also holds one slice of the grid per sorting variable and the
    # list of sorting variables, so the grid is read by its file name instead
    # of from the file listing of the repo.
    factor_library_cache$grids[[grid_revision]] <- read_factor_library_file(
      "factor-library-grid",
      grid_revision,
      "portfolio_sort_grid.parquet"
    )
  }

  factor_library_cache$grids[[grid_revision]]
}

#' Session cache for the factor library grid
#'
#' The grid has more than four million rows, so it is downloaded at most once
#' per session and revision by `download_factor_library_grid()`. `grids` holds
#' one grid per revision, named after the revision.
#' @noRd
factor_library_cache <- new.env(parent = emptyenv())
factor_library_cache$grids <- list()

#' Download factor library returns for a vector of portfolio IDs
#'
#' Given a vector of portfolio IDs from the `tidy-finance/factor-library-grid`
#' Hugging Face dataset, downloads the corresponding return data from the
#' `tidy-finance/factor-library` dataset on Hugging Face. The returns are
#' stored in files of 1,000 consecutive IDs named after the range they cover
#' (e.g., `id_0000001-0001000.parquet`), so the function downloads only the
#' files that hold the requested IDs. The grid metadata is joined onto the
#' result.
#'
#' Use this function when you already know the portfolio IDs you want (for
#' example, from a previous call to [download_data_huggingface()] with
#' `dataset = "factor_library"`). To resolve IDs from filter criteria
#' (sorting variable, weighting scheme, breakpoints, etc.) and download in
#' a single call, use [download_data_huggingface()] instead.
#'
#' Raises an error if none of the requested IDs exist in the grid. IDs whose
#' portfolio sort produced no portfolios have no returns; they are absent from
#' the result, with a warning, and the result has no rows if none of the
#' requested IDs has returns. Returns are stored in single precision, and
#' months without a valid long-short return are stored as `0`.
#'
#' @param ids Integer or numeric vector of portfolio IDs to download. IDs
#'   correspond to rows of the `tidy-finance/factor-library-grid` dataset.
#' @param revision Character(1). The revision of `tidy-finance/factor-library`
#'   to read the returns from: a commit hash, a branch, or a tag. Defaults to
#'   `"main"`, the latest release.
#' @inheritParams download_factor_library_grid
#'
#' @returns A tibble with the columns `id`, `date`, and `ret` (the monthly
#'   long-short excess return) and the grid metadata columns for the
#'   requested IDs.
#'
#' @family download functions
#' @export
#'
#' @examples
#' \dontrun{
#'   download_factor_library_ids(c(1L, 2L, 3L))
#'   download_factor_library_ids(
#'     c(1L, 2L, 3L),
#'     revision = "e47bd85accd9b9b168ac7f478f12fe89ba5c2430",
#'     grid_revision = "a5a74cdf1df0c2c3d1ce3d7bf6403a8386fbf99d"
#'   )
#' }
download_factor_library_ids <- function(
  ids,
  revision = "main",
  grid_revision = "main"
) {
  download_factor_library_table(ids, "returns", revision, grid_revision)
}

#' Download the returns or the costs of portfolio IDs
#'
#' The costs are stored in the folder `costs/`, in files with the same names
#' as the returns, so both are read the same way.
#'
#' @param ids Integer or numeric vector of portfolio IDs.
#' @param table `"returns"` or `"costs"`.
#' @param revision,grid_revision Character(1). The revisions of
#'   `tidy-finance/factor-library` and `tidy-finance/factor-library-grid`.
#'
#' @returns A tibble with the columns of the table and the grid metadata
#'   columns for the requested IDs.
#' @noRd
download_factor_library_table <- function(ids, table, revision, grid_revision) {
  check_revision(revision, "revision")

  id_grid <- download_factor_library_grid(grid_revision = grid_revision) |>
    dplyr::filter(.data$id %in% ids)

  if (nrow(id_grid) == 0) {
    cli::cli_abort(c(
      "None of the requested portfolio IDs exist in the factor library grid.",
      "i" = paste(
        "Check that the provided {.arg ids} are valid",
        "and exist in the factor library grid."
      )
    ))
  }

  files <- unique(factor_library_file(id_grid$id))
  if (table == "costs") {
    files <- paste0("costs/", files)
  }

  # The library has no file for a range of IDs in which no portfolio sort
  # produced portfolios, so such a file is missing
  data <- read_factor_library_files("factor-library", revision, files) |>
    dplyr::bind_rows()
  # Without any file, there are no columns to join the grid onto
  if (ncol(data) == 0) {
    data <- if (table == "returns") {
      tibble::tibble(
        id = integer(),
        date = as.Date(character()),
        ret = double()
      )
    } else {
      tibble::tibble(
        id = integer(),
        date = as.Date(character()),
        turnover = double(),
        cost = double(),
        microcap_share = double()
      )
    }
  }

  no_data <- sort(setdiff(id_grid$id, data$id))
  if (length(no_data) > 0) {
    shown <- paste(
      no_data[seq_len(min(5, length(no_data)))],
      collapse = ", "
    )
    if (length(no_data) > 5) {
      shown <- paste0(
        shown, ", ... (", format(length(no_data), big.mark = ","), " IDs)"
      )
    }
    cli::cli_warn(paste(
      "Portfolio IDs without {table} in the factor library, because their",
      "portfolio sort produced no portfolios, are absent from the result:",
      "{shown}."
    ))
  }

  dplyr::inner_join(data, id_grid, by = "id")
}

#' Name of the factor library file that holds a portfolio ID
#'
#' The returns are cut into files of 1,000 consecutive IDs named after the
#' range they cover, so the file follows from the ID alone.
#'
#' @param id Integer or numeric vector of portfolio IDs.
#' @returns A character vector of file names.
#' @noRd
factor_library_file <- function(id) {
  id_first <- (id - 1) %/% 1000 * 1000 + 1
  sprintf("id_%07d-%07d.parquet", id_first, id_first + 999)
}

#' Download factor library data from Hugging Face
#'
#' A thin wrapper around `download_factor_library_table()` that either takes
#' an explicit `ids` vector or resolves matching portfolio IDs from the
#' grid via `filter_factor_library_grid()` before downloading the
#' corresponding returns or costs.
#'
#' @param ... Named filter arguments forwarded to
#'   `filter_factor_library_grid()`. See
#'   `filter_factor_library_grid()` for the full list of supported
#'   columns and their defaults. Ignored when `ids` is provided.
#' @param ids Optional integer or numeric vector of portfolio IDs. When
#'   supplied, the filter arguments in `...` are not allowed and the helper
#'   delegates directly to `download_factor_library_table()`.
#' @param fill_all Logical(1). Forwarded to
#'   `filter_factor_library_grid()`. When `TRUE`, columns not
#'   specified in `...` are left unrestricted rather than set to
#'   their defaults. Ignored when `ids` is provided.
#' @param start_date Optional. A character string or Date object in
#'   `"YYYY-MM-DD"` format. When both `start_date` and `end_date` are
#'   provided, the data is filtered to the inclusive range. When either
#'   is `NULL`, the full history is returned.
#' @param end_date Optional. A character string or Date object in
#'   `"YYYY-MM-DD"` format. See `start_date`.
#' @param table `"returns"` (default) or `"costs"`.
#' @param revision,grid_revision Character(1). The revisions of
#'   `tidy-finance/factor-library` and `tidy-finance/factor-library-grid`.
#'
#' @return A tibble of portfolio returns or costs with grid metadata columns
#'   appended, one row per portfolio-period observation for the
#'   matched IDs.
#' @noRd
download_data_hugging_face_factor_library <- function(
  ...,
  ids = NULL,
  fill_all = FALSE,
  start_date = NULL,
  end_date = NULL,
  table = "returns",
  revision = "main",
  grid_revision = "main"
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
    ids <- filter_factor_library_grid(
      ...,
      fill_all = fill_all,
      grid_revision = grid_revision
    )
  }

  dates <- validate_dates(start_date, end_date)

  data <- download_factor_library_table(ids, table, revision, grid_revision)

  if (!is.null(dates$start_date) && !is.null(dates$end_date)) {
    data <- dplyr::filter(
      data,
      dplyr::between(.data$date, dates$start_date, dates$end_date)
    )
  }

  data
}

#' Download the persistence of the sorting variables
#'
#' Reads `sorting_variable_persistence.parquet` from
#' `tidy-finance/factor-library-grid`. It is small, so it is not cached.
#'
#' @param sorting_variable Optional character vector of sorting variables.
#'   `NULL` (default) returns all of them.
#' @param grid_revision Character(1). The revision of
#'   `tidy-finance/factor-library-grid`.
#' @param start_date,end_date Optional. When both are provided, the
#'   persistence is filtered to the inclusive range.
#'
#' @returns A tibble with the columns `sorting_variable`,
#'   `sorting_variable_lag`, `rebalancing`, `date`, and `persistence`.
#' @noRd
download_factor_library_persistence <- function(
  sorting_variable = NULL,
  grid_revision = "main",
  start_date = NULL,
  end_date = NULL
) {
  check_revision(grid_revision, "grid_revision")
  dates <- validate_dates(start_date, end_date)

  persistence <- read_factor_library_file(
    "factor-library-grid",
    grid_revision,
    "sorting_variable_persistence.parquet"
  )

  if (!is.null(sorting_variable)) {
    unknown <- setdiff(sorting_variable, persistence$sorting_variable)
    if (length(unknown) > 0) {
      cli::cli_abort(c(
        "Unknown sorting variable{?s}: {.val {unknown}}.",
        "i" = "See {.fn download_factor_library_grid} for all values."
      ))
    }
    selected <- sorting_variable
    persistence <- dplyr::filter(
      persistence,
      .data$sorting_variable %in% selected
    )
  }

  if (!is.null(dates$start_date) && !is.null(dates$end_date)) {
    persistence <- dplyr::filter(
      persistence,
      dplyr::between(.data$date, dates$start_date, dates$end_date)
    )
  }

  persistence
}

#' Check a revision of a Hugging Face dataset
#' @noRd
check_revision <- function(revision, arg) {
  if (
    !is.character(revision) ||
      length(revision) != 1 ||
      is.na(revision) ||
      !nzchar(revision)
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be a single string: a commit hash, a branch, or a tag."
    )
  }
}

#' URL of a file of the factor library on Hugging Face
#'
#' @param dataset `"factor-library"` or `"factor-library-grid"`.
#' @param revision Character(1). A commit hash, a branch, or a tag.
#' @param path Character vector of paths of files in the dataset.
#' @returns A character vector of URLs.
#' @noRd
factor_library_url <- function(dataset, revision, path) {
  paste0(
    "https://huggingface.co/datasets/tidy-finance/",
    dataset,
    "/resolve/",
    # The Hub expects the slashes of a branch such as refs/pr/1 encoded
    gsub("/", "%2F", revision, fixed = TRUE),
    "/",
    path
  )
}

#' Read one file of the factor library that must exist
#'
#' @inheritParams read_factor_library_files
#' @param path Character(1). The path of the file in the dataset.
#' @returns A data frame.
#' @noRd
read_factor_library_file <- function(dataset, revision, path) {
  data <- read_factor_library_files(dataset, revision, path)[[1]]
  if (is.null(data)) {
    cli::cli_abort(c(
      paste(
        "{.file {path}} is not in revision {.val {revision}} of",
        "{.val tidy-finance/{dataset}}."
      ),
      "i" = "Check that the revision is a commit of this dataset."
    ))
  }
  data
}

#' Read files of the factor library from Hugging Face
#'
#' Downloads the files at once and retries the ones that Hugging Face refused
#' because of its rate limit (HTTP 429) after the limit resets, twice at most.
#'
#' @param dataset `"factor-library"` or `"factor-library-grid"`.
#' @param revision Character(1). A commit hash, a branch, or a tag.
#' @param paths Character vector of paths of files in the dataset.
#' @returns A list with one data frame per path, and `NULL` for a path that is
#'   not in the revision.
#' @noRd
read_factor_library_files <- function(dataset, revision, paths) {
  urls <- factor_library_url(dataset, revision, paths)
  max_tries <- 3

  resps <- vector("list", length(urls))
  pending <- seq_along(urls)
  for (attempt in seq_len(max_tries)) {
    resps[pending] <- perform_factor_library_requests(urls[pending])
    pending <- pending[vapply(
      resps[pending],
      \(resp) inherits(resp, "httr2_http_429"),
      logical(1)
    )]
    if (length(pending) == 0) {
      break
    }

    seconds <- rate_limit_reset(resps[[pending[1]]]$resp)
    if (attempt == max_tries) {
      cli::cli_abort(
        c(
          "Hugging Face refused the download because of its rate limit.",
          "i" = paste(
            "Anonymous users may download 3,000 files per five minutes and",
            "IP address, which includes the downloads of other programs from",
            "the same address."
          ),
          "i" = "The limit resets in {seconds} second{?s}. Try again then."
        ),
        parent = resps[[pending[1]]]
      )
    }
    wait_for_rate_limit(seconds)
  }

  lapply(resps, \(resp) {
    if (inherits(resp, "httr2_http_404")) {
      if (
        identical(
          httr2::resp_header(resp$resp, "X-Error-Code"),
          "RevisionNotFound"
        )
      ) {
        cli::cli_abort(
          "Revision {.val {revision}} is not in {.val tidy-finance/{dataset}}."
        )
      }
      return(NULL)
    }
    if (inherits(resp, "error")) {
      rlang::cnd_signal(resp)
    }

    # nanoparquet reads from a file or a connection, not from a raw vector
    file <- tempfile(fileext = ".parquet")
    on.exit(unlink(file))
    writeBin(httr2::resp_body_raw(resp), file)
    nanoparquet::read_parquet(file)
  })
}

#' Download files from Hugging Face at once
#'
#' Hugging Face takes about a second to answer a request, so up to 16 run at
#' once. It allows anonymous users 3,000 file downloads per five minutes and
#' IP address. The throttle lets 16 requests start at once and then 8 per
#' second, at most 2,416 per five minutes. It is shared by all calls in a
#' session.
#'
#' @param urls Character vector of URLs.
#' @returns A list with one response, or the error of a failed request, per
#'   URL.
#' @noRd
perform_factor_library_requests <- function(urls) {
  requests <- lapply(urls, \(url) {
    httr2::request(url) |>
      httr2::req_user_agent(get_random_user_agent()) |>
      httr2::req_throttle(
        capacity = 16,
        fill_time_s = 2,
        realm = "huggingface.co"
      ) |>
      # Parallel requests retry HTTP 429 without limit, so
      # read_factor_library_files() retries them instead
      httr2::req_retry(max_tries = 1, is_transient = \(resp) FALSE)
  })

  httr2::req_perform_parallel(
    requests,
    on_error = "continue",
    progress = length(urls) > 16,
    max_active = 16
  )
}

#' Seconds until the rate limit of Hugging Face resets
#'
#' Hugging Face sends the remaining requests and the seconds until the limit
#' resets in the `RateLimit` header, e.g. `"resolvers";r=0;t=57`.
#'
#' @param resp An httr2 response.
#' @returns The seconds to wait.
#' @noRd
rate_limit_reset <- function(resp) {
  header <- httr2::resp_header(resp, "RateLimit", default = "")
  seconds <- as.numeric(extract_capture(header, "t=([0-9]+)"))
  # Without the header, a full window of five minutes resets the limit
  if (is.na(seconds)) {
    seconds <- 300
  }
  # One more second avoids a retry just before the reset
  seconds + 1
}

#' Wait for the rate limit of Hugging Face to reset
#' @noRd
wait_for_rate_limit <- function(seconds) {
  cli::cli_inform(paste(
    "Hugging Face limits the downloads per five minutes. Waiting",
    "{seconds} second{?s} for the limit to reset."
  ))
  Sys.sleep(seconds)
}
