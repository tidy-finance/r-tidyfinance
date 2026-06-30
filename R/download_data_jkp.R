#' Download and Process Global Factor Data
#'
#' Downloads and processes characteristic-managed portfolio (factor) returns
#' from [Global Factor Data](https://jkpfactors.com/data), the public data
#' library accompanying Jensen, Kelly, and Pedersen (2023). The data are stored
#' as zipped CSV files in a public AWS S3 bucket. The function validates the
#' requested selection against the library's live availability manifest, then
#' downloads the matching archive, unzips it, aligns monthly dates to the
#' beginning of the month, and optionally filters by a date range.
#'
#' Returns are already expressed as plain numeric (decimal) values in the
#' source data, so no rescaling is applied. The data are licensed under
#' CC BY-NC 4.0 (non-commercial use).
#'
#' @param dataset The Global Factor Data product to download. Currently only
#'   `"factors"` (characteristic-managed portfolio returns) is supported.
#'   The argument is reserved for future support of `"portfolios"` and
#'   `"industry"` returns.
#' @param region A character string with the region or country to download,
#'   using the codes from the availability manifest (e.g., `"usa"`, `"world"`,
#'   `"developed"`, `"emerging"`, or an ISO-3 country code such as `"jpn"`).
#'   Defaults to `"usa"`. Call `list_supported_jkp_factors()` to see the
#'   available regions.
#' @param factors A character string selecting the factor content: `"mkt"`
#'   (the market factor), `"all_factors"` (all 153 factors), `"all_themes"`
#'   (all 13 themes), a single theme (e.g., `"value"`, `"momentum"`), or a
#'   single factor code (e.g., `"be_me"`, `"ret_12_1"`). Defaults to
#'   `"all_factors"`. Call `list_supported_jkp_factors(region)` to see the
#'   values available for a region.
#' @param frequency The data frequency, either `"monthly"` (default) or
#'   `"daily"`.
#' @param weighting The portfolio weighting scheme: `"vw_cap"` (capped
#'   value-weighted, the default), `"vw"` (value-weighted), or `"ew"`
#'   (equal-weighted).
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If not
#'   provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the end date for the data. If not provided, the full
#'   dataset is returned.
#'
#' @returns A tibble with the processed factor returns. Columns are
#'   `location`, `name`, `freq`, `weighting`, `direction`, `n_stocks`,
#'   `n_stocks_min`, `date` (aligned to the beginning of the month for monthly
#'   data), and `ret` (the return as a plain numeric value).
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
#' }
download_data_jkp <- function(
  dataset = "factors",
  region = "usa",
  factors = "all_factors",
  frequency = "monthly",
  weighting = "vw_cap",
  start_date = NULL,
  end_date = NULL
) {
  if (!identical(dataset, "factors")) {
    cli::cli_abort(c(
      "Unsupported {.arg dataset}: {.val {dataset}}.",
      "i" = "Only {.val factors} is currently supported."
    ))
  }

  frequency <- rlang::arg_match(frequency, c("monthly", "daily"))
  weighting <- rlang::arg_match(weighting, c("vw_cap", "vw", "ew"))

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  availability <- handle_download_error(
    fetch_jkp_availability,
    fallback = NULL
  )

  if (is.null(availability)) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(tibble(date = Date()))
  }

  validate_jkp_factors(availability, region, factors, frequency)

  url <- build_jkp_factors_url(region, factors, frequency, weighting)

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

  processed_data <- raw_data |>
    mutate(date = ymd(.data$date))

  if (frequency == "monthly") {
    processed_data <- processed_data |>
      mutate(date = floor_date(.data$date, "month"))
  }

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(.data$date, start_date, end_date))
  }

  processed_data
}

#' List Supported Global Factor Data Regions and Factors
#'
#' Queries the live availability manifest of
#' [Global Factor Data](https://jkpfactors.com/data) and returns the regions
#' and factors that can be passed to [download_data_jkp()].
#'
#' @param region Optional. A region or country code. If provided, the function
#'   returns the factor codes available for that region. If `NULL` (the
#'   default), it returns the available region codes.
#'
#' @returns A character vector of region codes (when `region` is `NULL`) or of
#'   factor codes available for the requested region.
#'
#' @family utility functions
#' @export
#'
#' @examples
#' \donttest{
#'   list_supported_jkp_factors()
#'   list_supported_jkp_factors("usa")
#' }
list_supported_jkp_factors <- function(region = NULL) {
  availability <- handle_download_error(
    fetch_jkp_availability,
    fallback = NULL
  )

  if (is.null(availability)) {
    cli::cli_inform("Returning an empty vector due to download failure.")
    return(character())
  }

  regions <- names(availability$factors)

  if (is.null(region)) {
    return(regions)
  }

  if (!region %in% regions) {
    cli::cli_abort(c(
      "Unsupported {.arg region}: {.val {region}}.",
      "i" = "Use {.code list_supported_jkp_factors()} to see valid regions."
    ))
  }

  availability$factors[[region]]
}

#' Fetch the Global Factor Data Availability Manifest
#'
#' Downloads and parses the JSON availability manifest that lists the regions,
#' factors, and frequency restrictions offered by Global Factor Data.
#'
#' @returns A list with the parsed manifest, including the `factors` element
#'   (a named list mapping region codes to the available factor codes) and
#'   `factors_monthly_only` (region codes mapped to factors available at
#'   monthly frequency only).
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
validate_jkp_factors <- function(availability, region, factors, frequency) {
  regions <- names(availability$factors)
  if (!region %in% regions) {
    cli::cli_abort(c(
      "Unsupported {.arg region}: {.val {region}}.",
      "i" = "Use {.code list_supported_jkp_factors()} to see valid regions."
    ))
  }

  available_factors <- availability$factors[[region]]
  if (!factors %in% available_factors) {
    cli::cli_abort(c(
      paste0(
        "Unsupported {.arg factors}: {.val {factors}} for region ",
        "{.val {region}}."
      ),
      "i" = paste0(
        "Use {.code list_supported_jkp_factors(\"", region, "\")} to see ",
        "valid values."
      )
    ))
  }

  monthly_only <- availability$factors_monthly_only[[region]]
  is_monthly_only <- !is.null(monthly_only) && factors %in% monthly_only
  if (frequency == "daily" && is_monthly_only) {
    cli::cli_abort(c(
      paste0(
        "{.val {factors}} is only available at monthly frequency for region ",
        "{.val {region}}."
      ),
      "i" = "Set {.code frequency = \"monthly\"}."
    ))
  }

  invisible(TRUE)
}

#' Build the S3 Download URL for a Global Factor Data Factor File
#'
#' @keywords internal
#' @noRd
build_jkp_factors_url <- function(region, factors, frequency, weighting) {
  base_url <- "https://jkpfactors-data.s3.amazonaws.com"
  # The S3 object keys wrap each selector in literal square brackets, which are
  # URL-encoded as %5B and %5D.
  paste0(
    base_url, "/public/",
    "%5B", region, "%5D_",
    "%5B", factors, "%5D_",
    "%5B", frequency, "%5D_",
    "%5B", weighting, "%5D.zip"
  )
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
