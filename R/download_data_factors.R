#' Download and Process Fama-French Factor Data
#'
#' Downloads and processes Fama-French factor data based on the specified
#' dataset name and date range. The data is downloaded directly from Kenneth
#' French's data library and processed into a structured format, including
#' date conversion, scaling factor values, and filtering by the specified date
#' range.
#'
#' If there are multiple tables in the raw Fama-French data (e.g.,
#' value-weighted and equal-weighted returns), then the function only returns
#' the first table because these are the most popular. Download the source ZIP
#' archive directly if you need less commonly used tables.
#'
#' @param dataset The name of the Fama-French dataset to download (e.g.,
#'   "Fama/French 3 Factors").
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If not
#'   provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the end date for the data. If not provided, the full
#'   dataset is returned.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset` instead.
#'
#' @returns A tibble with processed factor data, including the date, risk-free
#'   rate, market excess return, and other factors, filtered by the specified
#'   date range.
#'
#' @references
#'   Fama, E. F., & French, K. R. (1993). Common risk factors in the returns on
#'   stocks and bonds. *Journal of Financial Economics*, 33(1), 3-56.
#'   \doi{10.1016/0304-405X(93)90023-5}
#'
#'   Fama, E. F., & French, K. R. (2015). A five-factor asset pricing model.
#'   *Journal of Financial Economics*, 116(1), 1-22.
#'   \doi{10.1016/j.jfineco.2014.10.010}
#'
#'   Carhart, M. M. (1997). On persistence in mutual fund performance.
#'   *Journal of Finance*, 52(1), 57-82.
#'   \doi{10.1111/j.1540-6261.1997.tb03808.x}
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_factors_ff(
#'     "Fama/French 3 Factors", "2000-01-01", "2020-12-31"
#'   )
#'   download_data_factors_ff(
#'     "10 Industry Portfolios", "2000-01-01", "2020-12-31"
#'   )
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
        "Use `list_supported_datasets(domain = 'Fama-French')` to ",
        "see the mapping."
      )
    )
    parsed <- parse_type_to_domain_dataset(dataset)
    dataset <- parsed$dataset
  }

  if (is.null(dataset)) {
    cli::cli_abort("Argument {.arg dataset} is required.")
  }

  file_url <- check_supported_dataset_ff(dataset)

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  raw_data <- handle_download_error(
    function() download_french_data_factors(file_url),
    fallback = tibble(
      date = Date()
    )
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform(
      "Returning an empty data set due to a download or parsing failure."
    )
    return(raw_data)
  }

  frequency <- determine_frequency_ff(dataset)

  if (frequency == "monthly") {
    processed_data <- raw_data |>
      mutate(
        date = floor_date(ymd(paste0(.data[["date"]], "01")), "month")
      )
  } else if (frequency %in% c("daily", "weekly")) {
    processed_data <- raw_data |>
      mutate(date = ymd(.data[["date"]]))
  } else {
    cli::cli_abort(
      "This dataset has neither daily, weekly, nor monthly frequency."
    )
  }

  processed_data <- processed_data |>
    mutate(
      across(-"date", ~ na_if(., -99.99)),
      across(-"date", ~ na_if(., -999))
    )

  # Factor files report percentage returns and are divided by 100. Breakpoints
  # files instead report dollar levels and share counts, which must not be
  # rescaled.
  if (!is_breakpoints_ff(dataset)) {
    processed_data <- processed_data |>
      mutate(across(-"date", ~ . / 100))
  }

  colnames_lower <- tolower(colnames(processed_data))
  colnames_clean <- gsub("-rf", "_excess", colnames_lower, fixed = TRUE)
  colnames_clean <- gsub("rf", "risk_free", colnames_clean, fixed = TRUE)
  colnames(processed_data) <- colnames_clean

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(.data[["date"]], start_date, end_date))
  }

  processed_data
}

#' Download a Fama-French Factor Data File
#'
#' Downloads a Kenneth French data library ZIP archive, unzips it, and parses
#' the first data table it contains. This is an internal replacement for
#' `frenchdata::download_french_data()` that only depends on `httr2` and base
#' R. Like the original, it returns the first table only, which is the most
#' commonly used one (e.g. value-weighted returns).
#'
#' @param file_url The path of the ZIP archive relative to the Kenneth French
#'   data library base URL, as stored in the `file_url` column of
#'   `list_supported_datasets_ff()`.
#' @param max_tries Number of download attempts before giving up.
#'
#' @returns A tibble with the parsed first table. The first column is named
#'   `date` and the remaining columns keep their original names from the source
#'   file.
#'
#' @keywords internal
#' @noRd
download_french_data_factors <- function(file_url, max_tries = 5) {
  base_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/"
  url <- paste0(base_url, file_url)

  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)

  httr2::request(url) |>
    httr2::req_user_agent(get_random_user_agent()) |>
    httr2::req_timeout(seconds = 60) |>
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

  parse_french_data_factors(csv_files[1])
}

#' Parse the First Table of a Fama-French CSV File
#'
#' Kenneth French CSV files start with descriptive header lines, then contain
#' one or more data tables of consecutive lines that start with a digit (the
#' date key). This helper extracts the first such table. Most factor files
#' carry a column-name row directly above the data (e.g. ",Mkt-RF,SMB,HML,RF");
#' breakpoints files have no header row at all (a prose title and a blank line
#' precede the data), in which case columns are read positionally and named
#' `date`, `V2`, `V3`, ... If annual summary rows (4-digit keys) follow the
#' periodic rows without a blank separator, they are dropped by keeping only the
#' leading rows whose date-key width matches the first data row.
#'
#' @param csv_file Path to the unzipped CSV file.
#'
#' @returns A tibble with the first column named `date` and the remaining
#'   columns keeping their original names (or positional `V*` names when the
#'   source file has no column-name row).
#'
#' @keywords internal
#' @noRd
parse_french_data_factors <- function(csv_file) {
  lines <- readLines(csv_file, warn = FALSE)

  # Data rows begin with a date key, i.e. an optional run of spaces followed by
  # a digit. The column-name row sits directly above the first such row.
  is_data <- grepl("^\\s*[0-9]", lines)
  if (!any(is_data)) {
    cli::cli_abort("Could not locate a data table in the downloaded file.")
  }

  runs <- rle(is_data)
  first_run <- which(runs$values)[1]
  preceding <- sum(runs$lengths[seq_len(first_run - 1L)])
  first_data <- preceding + 1L
  last_data <- preceding + runs$lengths[first_run]

  # A column-name row sits directly above the data only when the preceding line
  # exists, is non-blank, and is comma-delimited. Standard factor files have
  # one; breakpoints files do not (a prose title and a blank line precede the
  # data), so they are parsed positionally with header = FALSE.
  header_line <- first_data - 1L
  has_header <- header_line >= 1L &&
    !grepl("^\\s*$", lines[header_line]) &&
    grepl(",", lines[header_line])

  # Annual summary rows (4-digit keys) sometimes follow periodic rows with no
  # blank separator. Keep only the leading rows whose date-key width matches
  # the first data row, so monthly/daily tables never absorb annual rows.
  data_lines <- lines[first_data:last_data]
  key_width <- nchar(sub("^\\s*([0-9]+).*", "\\1", data_lines))
  keep <- cumprod(key_width == key_width[1]) == 1
  if (sum(!keep) > 0) {
    cli::cli_warn(c(
      "Dropped {sum(!keep)} trailing row{?s} with a different date-key width.",
      "i" = paste(
        "These are usually annual summary rows appended to a periodic table."
      )
    ))
  }
  last_data <- first_data + sum(keep) - 1L

  if (has_header) {
    block <- lines[c(header_line, first_data:last_data)]
    raw_data <- utils::read.csv(
      text = block,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  } else {
    block <- lines[first_data:last_data]
    raw_data <- utils::read.csv(
      text = block,
      header = FALSE,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  names(raw_data)[1] <- "date"

  # read.csv infers integer for whole-number columns (e.g. the "Count" column
  # in breakpoints files), but the downstream pipeline applies double-valued
  # operations such as `na_if(., -99.99)`. Coerce every value column to double
  # so the parser matches frenchdata's all-numeric output.
  value_cols <- setdiff(names(raw_data), "date")
  raw_data[value_cols] <- lapply(raw_data[value_cols], as.numeric)

  tibble::as_tibble(raw_data)
}

#' Download and Process Global Q Factor Data
#'
#' Downloads and processes Global Q factor data based on the
#' specified dataset, date range, and source URL. The processing
#' includes date conversion, renaming variables to a standardized
#' format, scaling factor values, and filtering by the specified
#' date range.
#'
#' @param dataset The name of the dataset to download (e.g.,
#'   "q5_factors_daily_2023.csv", "q5_factors_monthly_2023.csv").
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If
#'   not provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the end date for the data. If not
#'   provided, the full dataset is returned.
#' @param type `r lifecycle::badge("deprecated")` Use `dataset`
#'   instead.
#' @param url The base URL from which to download the dataset files.
#'
#' @returns A tibble with processed factor data, including the date,
#'   risk-free rate, market excess return, and other factors,
#'   filtered by the specified date range.
#'
#' @references
#'   Hou, K., Xue, C., & Zhang, L. (2015). Digesting anomalies: An investment
#'   approach. *Review of Financial Studies*, 28(3), 650-705.
#'   \doi{10.1093/rfs/hhu068}
#'
#'   Hou, K., Mo, H., Xue, C., & Zhang, L. (2019). Which factors?
#'   *Review of Finance*, 23(1), 1-35.
#'   \doi{10.1093/rof/rfy032}
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#' download_data_factors_q("q5_factors_daily_2024", "2020-01-01", "2020-12-31")
#' download_data_factors_q("q5_factors_annual_2024")
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
        "Use `list_supported_datasets(domain = 'Global Q')` to see the mapping."
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
    paste0(url, dataset, ".csv"),
    fallback = tibble(
      date = Date()
    )
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform(
      "Returning an empty data set due to a download or parsing failure."
    )
    return(raw_data)
  }

  frequency <- determine_frequency_q(dataset)

  if (frequency == "monthly") {
    processed_data <- raw_data |>
      mutate(date = ymd(paste(
        .data[["year"]], .data[["month"]], "01", sep = "-"
      ))) |>
      select(-c("year", "month"))
  } else if (frequency == "daily") {
    processed_data <- raw_data |>
      mutate(DATE = ymd(.data[["DATE"]]))
  } else if (frequency == "annual") {
    processed_data <- raw_data |>
      mutate(date = .data[["year"]])
  } else if (frequency %in% c("weekly", "quarterly")) {
    processed_data <- raw_data |>
      mutate(date = ymd(paste(
        .data[["year"]], .data[["month"]], .data[["day"]], sep = "-"
      ))) |>
      select(-c("year", "month", "day"))
  }

  processed_data <- processed_data |>
    rename_with(~ sub("R_", "", ., fixed = TRUE)) |>
    rename_with(tolower) |>
    mutate(across(-"date", ~ . / 100)) |>
    select(
      "date", risk_free = "f", mkt_excess = "mkt", everything()
    )

  if (!is.null(start_date) && !is.null(end_date)) {
    processed_data <- processed_data |>
      filter(between(.data[["date"]], start_date, end_date))
  }

  processed_data
}

# Helper functions --------------------------------------------------------

#' Check if a string is a legacy Fama-French type
#' @noRd
is_legacy_type_ff <- function(x) {
  ff_datasets <- dplyr::bind_rows(
    list_supported_datasets_ff(),
    list_supported_datasets_ff_legacy()
  )
  x %in% ff_datasets$type
}

#' Check if a string is a legacy Global Q type
#' @noRd
is_legacy_type_q <- function(x) {
  q_datasets <- list_supported_datasets_q()
  x %in% q_datasets$type
}

#' Determine frequency from Fama-French dataset name
#' @noRd
determine_frequency_ff <- function(dataset) {
  if (grepl("\\[Daily\\]", dataset)) {
    "daily"
  } else if (grepl("\\[Weekly\\]", dataset)) {
    "weekly"
  } else {
    "monthly"
  }
}

#' Check if a Fama-French dataset reports breakpoints
#'
#' Breakpoints files report dollar levels and share counts rather than
#' percentage returns, so the percentage scaling (dividing by 100) applied to
#' factor files must be skipped for them.
#' @noRd
is_breakpoints_ff <- function(dataset) {
  grepl("Breakpoints", dataset, fixed = TRUE)
}

#' Determine frequency from Global Q dataset name
#' @noRd
determine_frequency_q <- function(dataset) {
  if (grepl("daily", dataset, ignore.case = TRUE)) {
    "daily"
  } else if (grepl("weekly", dataset, ignore.case = TRUE)) {
    "weekly"
  } else if (grepl("monthly", dataset, ignore.case = TRUE)) {
    "monthly"
  } else if (grepl("quarterly", dataset, ignore.case = TRUE)) {
    "quarterly"
  } else if (grepl("annual", dataset, ignore.case = TRUE)) {
    "annual"
  } else {
    cli::cli_abort(
      "Cannot determine frequency from dataset name: {.val {dataset}}"
    )
  }
}

#' Validate a Fama-French dataset and return its source file URL
#'
#' Builds the combined Fama-French registry once, aborting if the dataset is
#' not supported, and returns the matching `file_url`.
#' @noRd
check_supported_dataset_ff <- function(dataset) {
  ff_datasets <- dplyr::bind_rows(
    list_supported_datasets_ff(),
    list_supported_datasets_ff_legacy()
  )

  idx <- match(dataset, ff_datasets$dataset_name)
  if (is.na(idx)) {
    cli::cli_abort(c(
      "Unsupported Fama-French dataset: {.val {dataset}}",
      "i" = paste0(
        "Use {.fn list_supported_datasets} with ",
        "{.arg domain = 'Fama-French'}"
      ),
      "to see available datasets."
    ))
  }

  ff_datasets$file_url[idx]
}

#' Check if Global Q dataset is supported
#' @noRd
check_supported_dataset_q <- function(dataset) {
  q_datasets <- list_supported_datasets_q()

  if (!dataset %in% q_datasets$dataset_name) {
    cli::cli_abort(c(
      "Unsupported Global Q dataset: {.val {dataset}}",
      "i" = "Use {.fn list_supported_datasets} with {.arg domain = 'Global Q'}",
      "to see available datasets."
    ))
  }
}
