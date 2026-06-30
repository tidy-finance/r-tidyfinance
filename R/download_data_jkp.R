#' Download and Process Global Factor Data
#'
#' Downloads and processes data from
#' [Global Factor Data](https://jkpfactors.com/data), the public data library
#' accompanying Jensen, Kelly, and Pedersen (2023). The data are stored as
#' zipped CSV files (and a few plain CSV reference files) in a public AWS S3
#' bucket. For the factor, portfolio, and industry products the function
#' validates the requested selection against the library's live availability
#' manifest, then downloads the matching archive, unzips it, aligns monthly
#' dates to the beginning of the month, and optionally filters by a date range.
#'
#' Returns are already expressed as plain numeric (decimal) values in the
#' source data, so no rescaling is applied. The data are licensed under
#' CC BY-NC 4.0 (non-commercial use).
#'
#' @param dataset The Global Factor Data product to download, one of:
#'   `"factors"` (characteristic-managed portfolio returns, the default),
#'   `"portfolios"` (the underlying low/middle/high portfolios that make up
#'   each long-short factor), `"industry"` (industry returns), `"nyse_cutoffs"`
#'   (NYSE size breakpoints), or `"return_cutoffs"` (return winsorization
#'   cutoffs).
#' @param region A character string with the region or country to download,
#'   using the codes from the availability manifest (e.g., `"usa"`, `"world"`,
#'   `"developed"`, `"emerging"`, or an ISO-3 country code such as `"jpn"`).
#'   Defaults to `"usa"`. Ignored for the reference datasets `"nyse_cutoffs"`
#'   and `"return_cutoffs"`. Call `list_supported_jkp_factors()` to see the
#'   available regions.
#' @param factors A character string selecting the factor content for the
#'   `"factors"` and `"portfolios"` datasets. For `"factors"`: `"mkt"` (the
#'   market factor), `"all_factors"` (all 153 factors), `"all_themes"` (all 13
#'   themes), a single theme (e.g., `"value"`, `"momentum"`), or a single
#'   factor code (e.g., `"be_me"`, `"ret_12_1"`). For `"portfolios"`: a single
#'   factor code. Defaults to `"all_factors"`. Call
#'   `list_supported_jkp_factors(region, dataset)` to see the values available
#'   for a region.
#' @param classification The industry classification for the `"industry"`
#'   dataset, either `"gics"` (default) or `"ff49"` (Fama-French 49
#'   industries).
#' @param frequency The data frequency, either `"monthly"` (default) or
#'   `"daily"`. The `"industry"` dataset is only available at monthly
#'   frequency. For `"return_cutoffs"`, the frequency selects the monthly or
#'   daily cutoff file.
#' @param weighting The portfolio weighting scheme: `"vw_cap"` (capped
#'   value-weighted, the default), `"vw"` (value-weighted), or `"ew"`
#'   (equal-weighted). Ignored for the reference datasets.
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If not
#'   provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the end date for the data. If not provided, the full
#'   dataset is returned.
#'
#' @returns A tibble with the processed data. The `date` column is aligned to
#'   the beginning of the month for monthly data, and all returns are plain
#'   numeric (decimal) values. The remaining columns depend on `dataset`: the
#'   `"factors"` data carry `location`, `name`, `freq`, `weighting`,
#'   `direction`, `n_stocks`, `n_stocks_min`, and `ret`; the `"portfolios"`
#'   data add a `pf` portfolio identifier; the `"industry"` data carry the
#'   classification code alongside `ret`; and the reference datasets carry
#'   breakpoint or cutoff columns.
#'
#' @references
#'   Jensen, T. I., Kelly, B. T., & Pedersen, L. H. (2023). Is there a
#'   replication crisis in finance? *Journal of Finance*, 78(5), 2465-2518.
#'   \doi{10.1111/jofi.13249}
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_jkp(
#'     region = "usa", factors = "mkt",
#'     start_date = "2000-01-01", end_date = "2020-12-31"
#'   )
#'   download_data_jkp(
#'     dataset = "portfolios", region = "usa", factors = "be_me",
#'     start_date = "2000-01-01", end_date = "2020-12-31"
#'   )
#'   download_data_jkp(
#'     dataset = "industry", region = "usa", classification = "gics"
#'   )
#'   download_data_jkp(dataset = "nyse_cutoffs")
#' }
download_data_jkp <- function(
  dataset = "factors",
  region = "usa",
  factors = "all_factors",
  classification = "gics",
  frequency = "monthly",
  weighting = "vw_cap",
  start_date = NULL,
  end_date = NULL
) {
  supported_datasets <- c(
    "factors", "portfolios", "industry", "nyse_cutoffs", "return_cutoffs"
  )
  if (!(dataset %in% supported_datasets)) {
    cli::cli_abort(c(
      "Unsupported {.arg dataset}: {.val {dataset}}.",
      "i" = "Supported datasets: {.val {supported_datasets}}."
    ))
  }

  if (!(frequency %in% c("monthly", "daily"))) {
    cli::cli_abort(
      "{.arg frequency} must be {.str monthly} or {.str daily}."
    )
  }

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  # Reference datasets are plain CSV files that need neither manifest
  # validation, weighting, nor a region selection.
  if (dataset %in% c("nyse_cutoffs", "return_cutoffs")) {
    url <- build_jkp_reference_url(dataset, frequency)
    raw_data <- handle_download_error(
      function() download_jkp_csv(url),
      fallback = tibble(date = Date())
    )
    if (nrow(raw_data) == 0) {
      cli::cli_inform(
        "Returning an empty data set due to a download or parsing failure."
      )
      return(raw_data)
    }
    return(process_jkp_data(
      raw_data,
      date_col = "eom", frequency = "monthly", start_date, end_date
    ))
  }

  if (!(weighting %in% c("vw_cap", "vw", "ew"))) {
    cli::cli_abort(
      "{.arg weighting} must be one of {.val {c('vw_cap', 'vw', 'ew')}}."
    )
  }

  if (dataset == "industry" && frequency == "daily") {
    cli::cli_abort(c(
      "The {.val industry} dataset is only available at monthly frequency.",
      "i" = "Set {.code frequency = \"monthly\"}."
    ))
  }

  availability <- handle_download_error(
    fetch_jkp_availability,
    fallback = NULL
  )

  if (is.null(availability)) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(tibble(date = Date()))
  }

  selector <- if (dataset == "industry") classification else factors
  validate_jkp_selection(availability, dataset, region, selector, frequency)

  url <- build_jkp_url(dataset, region, selector, frequency, weighting)

  raw_data <- handle_download_error(
    function() download_jkp_file(url),
    fallback = tibble(date = Date())
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform(
      "Returning an empty data set due to a download or parsing failure."
    )
    return(raw_data)
  }

  data_frequency <- if (dataset == "industry") "monthly" else frequency
  processed_data <- process_jkp_data(
    raw_data,
    date_col = "date", frequency = data_frequency, start_date, end_date
  )

  if (dataset == "portfolios" && "pf" %in% names(processed_data)) {
    processed_data <- processed_data |>
      mutate(pf = as.integer(.data$pf))
  }

  processed_data
}

#' List Supported Global Factor Data Regions and Factors
#'
#' Queries the live availability manifest of
#' [Global Factor Data](https://jkpfactors.com/data) and returns the regions
#' and selectors that can be passed to [download_data_jkp()].
#'
#' @param region Optional. A region or country code. If provided, the function
#'   returns the selectors (factor codes, or industry classifications when
#'   `dataset = "industry"`) available for that region. If `NULL` (the
#'   default), it returns the available region codes.
#' @param dataset The Global Factor Data product to query, one of `"factors"`
#'   (default), `"portfolios"`, or `"industry"`.
#'
#' @returns A tibble. When `region` is `NULL`, it has a single `region` column
#'   listing the available region codes. When `region` is provided, it has a
#'   `region` column and a `factor` column listing the selectors (factor codes,
#'   or industry classifications when `dataset = "industry"`) available for that
#'   region.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \donttest{
#'   list_supported_jkp_factors()
#'   list_supported_jkp_factors("usa")
#'   list_supported_jkp_factors("usa", dataset = "portfolios")
#' }
list_supported_jkp_factors <- function(region = NULL, dataset = "factors") {
  supported_datasets <- c("factors", "portfolios", "industry")
  if (!(dataset %in% supported_datasets)) {
    cli::cli_abort(c(
      "Unsupported {.arg dataset}: {.val {dataset}}.",
      "i" = "Supported datasets: {.val {supported_datasets}}."
    ))
  }

  availability <- handle_download_error(
    fetch_jkp_availability,
    fallback = NULL
  )

  if (is.null(availability)) {
    cli::cli_inform("Returning an empty tibble due to download failure.")
    return(tibble(region = character()))
  }

  regions <- names(availability[[dataset]])

  if (is.null(region)) {
    return(tibble(region = regions))
  }

  if (!region %in% regions) {
    cli::cli_abort(c(
      "Unsupported {.arg region}: {.val {region}}.",
      "i" = "Use {.code list_supported_jkp_factors()} to see valid regions."
    ))
  }

  tibble(region = region, factor = availability[[dataset]][[region]])
}

#' Fetch the Global Factor Data Availability Manifest
#'
#' Downloads and parses the JSON availability manifest that lists the regions,
#' factors, and frequency restrictions offered by Global Factor Data.
#'
#' @returns A list with the parsed manifest, including the `factors`,
#'   `portfolios`, and `industry` elements (each a named list mapping region
#'   codes to the available selectors) and `factors_monthly_only` (region
#'   codes mapped to factors available at monthly frequency only).
#'
#' @keywords internal
#' @noRd
fetch_jkp_availability <- function(max_tries = 3) {
  url <- paste0(
    "https://jkpfactors-data.s3.amazonaws.com/public/availability.json"
  )

  resp <- httr2::request(url) |>
    httr2::req_user_agent(get_random_user_agent()) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_retry(max_tries = max_tries) |>
    httr2::req_perform()

  jsonlite::fromJSON(
    httr2::resp_body_string(resp),
    simplifyVector = TRUE
  )
}

#' Validate a Global Factor Data Selection Against the Manifest
#'
#' @keywords internal
#' @noRd
validate_jkp_selection <- function(
  availability, dataset, region, selector, frequency
) {
  regions <- names(availability[[dataset]])
  if (!region %in% regions) {
    cli::cli_abort(c(
      paste0(
        "Unsupported {.arg region}: {.val {region}} for dataset ",
        "{.val {dataset}}."
      ),
      "i" = paste0(
        "Use {.code list_supported_jkp_factors(dataset = \"", dataset,
        "\")} to see valid regions."
      )
    ))
  }

  available <- availability[[dataset]][[region]]
  if (!selector %in% available) {
    cli::cli_abort(c(
      paste0(
        "Unsupported selection {.val {selector}} for region {.val {region}} ",
        "in dataset {.val {dataset}}."
      ),
      "i" = paste0(
        "Use {.code list_supported_jkp_factors(\"", region, "\", \"", dataset,
        "\")} to see valid values."
      )
    ))
  }

  if (dataset == "factors") {
    monthly_only <- availability$factors_monthly_only[[region]]
    is_monthly_only <- !is.null(monthly_only) && selector %in% monthly_only
    if (frequency == "daily" && is_monthly_only) {
      cli::cli_abort(c(
        paste0(
          "{.val {selector}} is only available at monthly frequency for ",
          "region {.val {region}}."
        ),
        "i" = "Set {.code frequency = \"monthly\"}."
      ))
    }
  }

  invisible(TRUE)
}

#' Build the S3 Download URL for a Global Factor Data Archive
#'
#' @keywords internal
#' @noRd
build_jkp_url <- function(dataset, region, selector, frequency, weighting) {
  base_url <- "https://jkpfactors-data.s3.amazonaws.com"
  # The S3 object keys wrap each selector in literal square brackets, which are
  # URL-encoded as %5B and %5D.
  b <- function(x) paste0("%5B", x, "%5D")

  if (dataset == "factors") {
    paste0(
      base_url, "/public/",
      b(region), "_", b(selector), "_", b(frequency), "_", b(weighting), ".zip"
    )
  } else if (dataset == "portfolios") {
    paste0(
      base_url, "/public/portfolios/",
      b(region), "_", b(selector), "_", b(frequency), "_", b(weighting), ".zip"
    )
  } else {
    # The industry dataset is only published at monthly frequency.
    paste0(
      base_url, "/public/industry/",
      b(region), "_", b(selector), "_", b("monthly"), "_", b(weighting), ".zip"
    )
  }
}

#' Build the S3 Download URL for a Global Factor Data Reference File
#'
#' @keywords internal
#' @noRd
build_jkp_reference_url <- function(dataset, frequency) {
  base_url <- "https://jkpfactors-data.s3.amazonaws.com/public/other"

  if (dataset == "nyse_cutoffs") {
    paste0(base_url, "/nyse_cutoffs.csv")
  } else if (frequency == "daily") {
    paste0(base_url, "/return_cutoffs_daily.csv")
  } else {
    paste0(base_url, "/return_cutoffs.csv")
  }
}

#' Normalize Dates and Filter a Global Factor Data Table
#'
#' @keywords internal
#' @noRd
process_jkp_data <- function(
  data, date_col, frequency, start_date, end_date
) {
  if (date_col != "date") {
    names(data)[names(data) == date_col] <- "date"
  }

  data <- data |>
    mutate(date = ymd(.data$date))

  if (frequency == "monthly") {
    data <- data |>
      mutate(date = floor_date(.data$date, "month"))
  }

  if (!is.null(start_date) && !is.null(end_date)) {
    data <- data |>
      filter(between(.data$date, start_date, end_date))
  }

  data
}

#' Download and Read a Zipped Global Factor Data CSV File
#'
#' @keywords internal
#' @noRd
download_jkp_file <- function(url, max_tries = 5) {
  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)

  httr2::request(url) |>
    httr2::req_user_agent(get_random_user_agent()) |>
    httr2::req_timeout(seconds = 180) |>
    httr2::req_retry(max_tries = max_tries) |>
    httr2::req_perform(path = tmp_zip)

  tmp_dir <- tempfile()
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(tmp_zip, exdir = tmp_dir)
  csv_files <- list.files(
    tmp_dir,
    pattern = "\\.csv$",
    ignore.case = TRUE,
    full.names = TRUE,
    recursive = TRUE
  )
  if (length(csv_files) == 0) {
    cli::cli_abort("No CSV file found in the downloaded archive.")
  }

  as_tibble(read.csv(csv_files[1], na.strings = "na"))
}

#' Download and Read a Plain Global Factor Data CSV File
#'
#' @keywords internal
#' @noRd
download_jkp_csv <- function(url, max_tries = 5) {
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)

  httr2::request(url) |>
    httr2::req_user_agent(get_random_user_agent()) |>
    httr2::req_timeout(seconds = 180) |>
    httr2::req_retry(max_tries = max_tries) |>
    httr2::req_perform(path = tmp_csv)

  as_tibble(read.csv(tmp_csv, na.strings = "na"))
}
