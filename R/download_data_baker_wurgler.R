#' Download and Process the Baker-Wurgler Investor Sentiment Index
#'
#' Downloads and processes the monthly investor sentiment index of Baker and
#' Wurgler (2006, 2007) from
#' \href{https://pages.stern.nyu.edu/~jwurgler/}{Jeffrey Wurgler's website}.
#' The function reads the `DATA` sheet of the source Excel file, aligns the
#' date to the beginning of the month, renames the columns to the package
#' conventions, and optionally filters the data based on a provided date range.
#'
#' The source is an Excel file, so the function requires the \pkg{readxl}
#' package. The sentiment index is now based on five proxies: NYSE turnover,
#' one of the six original proxies, has been dropped. The authors advise
#' against using the series to measure changes in sentiment, as they are
#' low-frequency level indicators.
#'
#' @param dataset The dataset to download. Currently only
#'   `"sentiment_monthly"` (the default) is supported.
#' @param start_date Optional. A character string or Date object in
#'   "YYYY-MM-DD" format specifying the start date for the data. If not
#'   provided, the full dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the end date for the data. If not provided, the full
#'   dataset is returned.
#' @param url The URL of the Excel file with the sentiment data.
#'
#' @returns A tibble with the columns `date` (aligned to the beginning of the
#'   month), `sentiment` (the sentiment index, `SENT`), `sentiment_orth` (the
#'   sentiment index based on proxies orthogonalized with respect to
#'   macroeconomic indicators, `SENT_ORTH`), the sentiment proxies `pdnd`
#'   (dividend premium), `ripo` (first-day IPO returns), `nipo` (IPO volume),
#'   `cefd` (closed-end fund discount), and `s` (equity share in new issues),
#'   and the macroeconomic indicators `indpro` (industrial production),
#'   `consdur`, `consnon`, and `consserv` (nominal durables, nondurables, and
#'   services consumption), `recess` (NBER recession indicator), `employ`
#'   (employment), and `cpi` (consumer price index). Values are as of the end
#'   of the month and missing where the source has no data. The data is
#'   filtered by the specified date range if `start_date` and `end_date` are
#'   provided.
#'
#' @references
#'   Baker, M., & Wurgler, J. (2006). Investor sentiment and the cross-section
#'   of stock returns. *Journal of Finance*, 61(4), 1645-1680.
#'   \doi{10.1111/j.1540-6261.2006.00885.x}
#'
#'   Baker, M., & Wurgler, J. (2007). Investor sentiment in the stock market.
#'   *Journal of Economic Perspectives*, 21(2), 129-152.
#'   \doi{10.1257/jep.21.2.129}
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_baker_wurgler(
#'     start_date = "2020-01-01", end_date = "2020-12-31"
#'   )
#' }
download_data_baker_wurgler <- function(
  dataset = "sentiment_monthly",
  start_date = NULL,
  end_date = NULL,
  url = "https://pages.stern.nyu.edu/~jwurgler/data/SENTIMENT.xlsx"
) {
  if (!identical(dataset, "sentiment_monthly")) {
    cli::cli_abort(
      "{.arg dataset} must be {.str sentiment_monthly}."
    )
  }

  rlang::check_installed(
    "readxl",
    reason = "to read the Baker-Wurgler Excel file."
  )

  dates <- validate_dates(start_date, end_date)
  start_date <- dates$start_date
  end_date <- dates$end_date

  raw_data <- handle_download_error(
    read_baker_wurgler_file,
    url,
    fallback = tibble()
  )

  if (nrow(raw_data) == 0) {
    cli::cli_inform("Returning an empty data set due to download failure.")
    return(raw_data)
  }

  # Match the source columns case-insensitively, as their capitalization
  # differs (e.g., "SENT" and "pdnd").
  names(raw_data) <- tolower(names(raw_data))
  columns <- c(
    "yearmo",
    "sent",
    "sent_orth",
    "pdnd",
    "ripo",
    "nipo",
    "cefd",
    "s",
    "indpro",
    "consdur",
    "consnon",
    "consserv",
    "recess",
    "employ",
    "cpi"
  )
  missing_columns <- setdiff(columns, names(raw_data))
  if (length(missing_columns) > 0) {
    cli::cli_abort(c(
      "The Baker-Wurgler data has an unexpected layout.",
      "x" = "Missing columns: {.val {missing_columns}}."
    ))
  }

  processed_data <- raw_data |>
    filter(!is.na(.data$yearmo)) |>
    mutate(
      date = floor_date(ym(as.character(.data$yearmo)), "month"),
      across(all_of(columns[-1]), as.numeric)
    ) |>
    select(
      "date",
      sentiment = "sent",
      sentiment_orth = "sent_orth",
      all_of(columns[-(1:3)])
    )

  if (!is.null(start_date) && !is.null(end_date)) {
    filtered_data <- processed_data |>
      filter(between(.data$date, start_date, end_date))

    if (nrow(filtered_data) == 0) {
      available_range <- range(processed_data$date)
      cli::cli_warn(c(
        paste0(
          "The requested date range lies outside the available ",
          "Baker-Wurgler data."
        ),
        "i" = paste0(
          "Available data range: ",
          available_range[1],
          " to ",
          available_range[2],
          "."
        )
      ))
    }

    processed_data <- filtered_data
  }

  processed_data
}

#' Download and Read the Baker-Wurgler Excel File
#'
#' @param url The URL of the Excel file.
#'
#' @returns A tibble with the `DATA` sheet of the file.
#'
#' @keywords internal
#' @noRd
read_baker_wurgler_file <- function(url) {
  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)

  httr2::request(url) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = tmp)

  readxl::read_excel(tmp, sheet = "DATA")
}
