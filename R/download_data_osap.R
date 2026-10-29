#' Download and Process Open Source Asset Pricing Data
#'
#' Downloads data from
#' [Open Source Asset Pricing](https://www.openassetpricing.com/data/)
#' (OSAP): the monthly long-short returns of the predictor portfolios, the
#' signal documentation, or the monthly firm-level predictors (signals).
#' The column names are converted to snake_case, dates are aligned to the
#' beginning of the month, and the data are optionally filtered by a date
#' range.
#'
#' The data come from the [OSAP dataset on Hugging
#' Face](https://huggingface.co/datasets/OpenSourceAP/CrossSection), which
#' keeps every data release on its own branch. `release` selects the
#' release. Releases up to October 2025 were also distributed on Google
#' Drive. Pass the Google Drive file ID of a wide CSV of long-short returns
#' to `sheet_id` to download it from Google Drive instead.
#'
#' The datasets are:
#'
#' - `"long_short_returns"`: monthly long-short returns of the predictor
#'   portfolios, constructed as in the original papers, with one column per
#'   predictor. The returns are in percent in the source file, so they are
#'   divided by 100 to obtain plain numeric (decimal) returns.
#' - `"signal_doc"`: the documentation of all predictors and placebos, one
#'   row per signal, e.g., its category, original paper, and sign. The
#'   `acronym` column holds the OSAP names of the signals, which `signals`
#'   takes. Available from release `"2022.03"`.
#' - `"predictors"`: the monthly firm-level predictors, with columns
#'   `permno`, `date`, and one column per predictor. The values are not
#'   scaled. `Price`, `Size`, and `STreversal` are not included because they
#'   require a WRDS subscription.
#' - `"signed_predictors"`: the same as `"predictors"`, but each predictor
#'   is multiplied by its sign in `"signal_doc"` so that a higher value
#'   implies a higher expected return.
#'
#' The files of `"predictors"` and `"signed_predictors"` are about 2 GB.
#' The whole file is downloaded in either case, but `signals` reads only
#' the selected predictors into memory.
#'
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If
#'   not provided, the full dataset is returned. Ignored for
#'   `"signal_doc"`.
#' @param end_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the end date for the data. If not
#'   provided, the full dataset is returned. Ignored for `"signal_doc"`.
#' @param dataset A character string with the dataset to download:
#'   `"long_short_returns"` (the default), `"signal_doc"`, `"predictors"`,
#'   or `"signed_predictors"`.
#' @param release A character string with the OSAP data release to
#'   download from Hugging Face. The default, `"2026.10"`, is the October
#'   2026 release (v2.1.0). The other supported releases are `"2025.10"`,
#'   `"2024.10"`, `"2024.08"`, `"2023.08"`, `"2022.04"`, `"2022.03"`,
#'   `"2021.04"`, and `"2021.03"`. The releases of 2020 have no wide file of
#'   long-short returns or firm-level predictors.
#' @param sheet_id Optional. A character string with the Google Drive file
#'   ID of a wide CSV of monthly long-short returns, e.g.,
#'   `"10sOryk_ddjkXagaajTKUk1nwJs2ZLRiI"` for the October 2025 release
#'   (v2.0.0). If provided, the data are downloaded from Google Drive
#'   instead of Hugging Face, and `release` must not be set. Only for
#'   `"long_short_returns"`. Google Drive limits how often a shared file can
#'   be downloaded. When the limit is reached, Google Drive returns an error
#'   page instead of the data and the function returns an empty tibble. The
#'   limit is usually lifted within 24 hours.
#' @param signals Optional. A character vector of predictors to read for
#'   `"predictors"` and `"signed_predictors"`, using the OSAP names in the
#'   `acronym` column of `"signal_doc"`, e.g., `c("BM", "Mom12m")`. If not
#'   provided, all predictors are returned.
#'
#' @returns A tibble containing the processed data. The column names are
#'   converted to snake_case, the `date` column is aligned to the beginning
#'   of the month, and the data is filtered by the specified date range if
#'   `start_date` and `end_date` are provided.
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   osap <- download_data_osap(
#'     start_date = "2020-01-01", end_date = "2020-06-30"
#'   )
#'
#'   # An earlier release
#'   osap_2025 <- download_data_osap(release = "2025.10")
#'
#'   # Signal documentation
#'   signal_doc <- download_data_osap(dataset = "signal_doc")
#' }
#' \dontrun{
#'   # Firm-level predictors (downloads about 2 GB)
#'   predictors <- download_data_osap(
#'     start_date = "2020-01-01",
#'     end_date = "2020-12-31",
#'     dataset = "signed_predictors",
#'     signals = c("BM", "Mom12m")
#'   )
#' }
download_data_osap <- function(
  start_date = NULL,
  end_date = NULL,
  dataset = "long_short_returns",
  release = "2026.10",
  sheet_id = NULL,
  signals = NULL
) {
  datasets <- c(
    "long_short_returns", "signal_doc", "predictors", "signed_predictors"
  )
  if (!is.character(dataset) || length(dataset) != 1 ||
        !dataset %in% datasets) {
    cli::cli_abort(c(
      "{.arg dataset} must be one of {.val {datasets}}.",
      "x" = "You supplied {.val {dataset}}."
    ))
  }
  if (!is.null(sheet_id) && dataset != "long_short_returns") {
    cli::cli_abort(
      "{.arg sheet_id} is only supported for {.val long_short_returns}."
    )
  }
  if (!is.null(signals) &&
        !dataset %in% c("predictors", "signed_predictors")) {
    cli::cli_abort(paste(
      "{.arg signals} is only supported for {.val predictors} and",
      "{.val signed_predictors}."
    ))
  }
  if (!is.null(signals) && (!is.character(signals) || length(signals) == 0)) {
    cli::cli_abort("{.arg signals} must be a non-empty character vector.")
  }

  if (dataset == "signal_doc") {
    url <- osap_release_url(release, dataset)
    raw_data <- handle_download_error(
      function() as_tibble(read_parquet_url(url)),
      fallback = tibble()
    )
    if (nrow(raw_data) == 0) {
      cli::cli_inform("Returning an empty data set due to download failure.")
      return(raw_data)
    }
    colnames(raw_data) <- to_snake_case(colnames(raw_data))
    return(raw_data)
  }

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  if (!is.null(sheet_id)) {
    if (!missing(release)) {
      cli::cli_abort(
        "Supply either {.arg release} or {.arg sheet_id}, not both."
      )
    }
    raw_data <- handle_download_error(
      function() download_osap_google_drive(sheet_id),
      fallback = tibble()
    )
  } else if (dataset == "long_short_returns") {
    url <- osap_release_url(release, dataset)
    raw_data <- handle_download_error(
      function() as_tibble(read_parquet_url(url)),
      fallback = tibble()
    )
  } else {
    url <- osap_release_url(release, dataset)
    col_select <- if (is.null(signals)) NULL else c("permno", "yyyymm", signals)
    cli::cli_inform(
      "Downloading {.val {dataset}} (about 2 GB). This can take a while."
    )
    raw_data <- handle_download_error(
      function() as_tibble(read_parquet_url(url, col_select = col_select)),
      fallback = tibble()
    )
  }

  if (nrow(raw_data) == 0) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(raw_data)
  }

  if (dataset == "long_short_returns") {
    processed_data <- raw_data |>
      mutate(date = floor_date(ymd(.data$date), "month"))
    colnames(processed_data) <- to_snake_case(colnames(processed_data))

    # All columns except the date are long-short returns in percent, so
    # scale them to plain numeric (decimal) returns.
    processed_data <- processed_data |>
      mutate(across(-"date", \(x) x / 100))
  } else {
    # Signals at the end of month yyyymm predict the returns after it.
    processed_data <- raw_data |>
      mutate(date = ymd(paste0(.data$yyyymm, "01")), .after = "permno") |>
      select(-"yyyymm")
    colnames(processed_data) <- to_snake_case(colnames(processed_data))
  }

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(.data$date, start_date, end_date))
  }

  processed_data
}

#' Build the Hugging Face URL of an OSAP File
#'
#' Each OSAP data release is a branch of the Hugging Face dataset. The
#' latest release is on a branch named `Data-Release-<release>`, the older
#' ones on a branch named like the release.
#'
#' @param release Character(1). The OSAP data release, e.g., `"2026.10"`.
#' @param dataset Character(1). The dataset, see `download_data_osap()`.
#'
#' @returns The URL of the parquet file.
#' @keywords internal
#' @noRd
osap_release_url <- function(release, dataset = "long_short_returns") {
  releases <- c(
    "2026.10", "2025.10", "2024.10", "2024.08", "2023.08", "2022.04",
    "2022.03", "2021.04", "2021.03"
  )
  # The signal documentation is a parquet file from release 2022.03 on.
  if (dataset == "signal_doc") {
    releases <- setdiff(releases, c("2021.04", "2021.03"))
  }
  if (!is.character(release) || length(release) != 1 ||
        !release %in% releases) {
    cli::cli_abort(c(
      "{.arg release} must be one of {.val {releases}} for {.val {dataset}}.",
      "x" = "You supplied {.val {release}}."
    ))
  }

  branch <- if (release == "2026.10") "Data-Release-2026.10" else release
  ls_folder <- if (release == "2021.03") {
    "Full_Sets_Main"
  } else {
    "Full%20Sets%20OP"
  }
  path <- switch(
    dataset,
    long_short_returns = paste0(
      "Portfolios/", ls_folder, "/PredictorLSretWide.parquet"
    ),
    signal_doc = "SignalDoc.parquet",
    predictors = "Firm%20Level%20Characteristics/Predictors.parquet",
    signed_predictors = paste0(
      "Firm%20Level%20Characteristics/signed_predictors_dl_wide.parquet"
    )
  )

  paste0(
    "https://huggingface.co/datasets/OpenSourceAP/CrossSection/resolve/",
    branch, "/", path
  )
}

#' Download OSAP Long-Short Returns from Google Drive
#'
#' @param sheet_id Character(1). The Google Drive file ID of the wide CSV.
#'
#' @returns A tibble with the raw CSV data.
#' @keywords internal
#' @noRd
download_osap_google_drive <- function(sheet_id) {
  url <- paste0("https://drive.google.com/uc?export=download&id=", sheet_id)

  response <- httr2::request(url) |>
    httr2::req_user_agent(get_random_user_agent()) |>
    httr2::req_timeout(seconds = 120) |>
    httr2::req_perform()
  body <- httr2::resp_body_string(response)

  # Google Drive answers with an HTML page instead of the CSV, e.g., when
  # the download quota of the file is exceeded.
  content_type <- httr2::resp_content_type(response)
  if (grepl("text/html", content_type, fixed = TRUE)) {
    if (grepl("Quota exceeded", body, fixed = TRUE)) {
      cli::cli_abort(paste(
        "The Google Drive download quota of the file is exceeded.",
        "Try again later, usually within 24 hours."
      ))
    }
    cli::cli_abort("Google Drive returned an HTML page instead of a CSV.")
  }

  suppressWarnings(as_tibble(read.csv(text = body)))
}

to_snake_case <- function(x) {
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  x <- gsub("[^[:alnum:]_]+", "_", x)
  x <- tolower(x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}
