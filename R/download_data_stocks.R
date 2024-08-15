#' Download Stock Data
#'
#' Downloads historical stock data for a given symbol and date range from specified sources.
#'
#' @param type A character string representing the data source type.
#'   Currently supported type is `"stocks_yf"` for Yahoo Finance. Defaults to `"stocks_yf"`.
#' @param symbols A character vector of stock symbols to download data for. At least one
#'   symbol must be provided.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subset of the dataset is returned.
#'
#' @returns A tibble containing the downloaded stock data with columns depending on the data source.
#'
#' @export
#' @examples
#' \dontrun{
#' download_data_stocks(type = "stocks_yf", symbols = c("AAPL", "MSFT"))
#' }
download_data_stocks <- function(type, symbols, start_date = NULL, end_date = NULL) {

  check_supported_type(type)

  if (grepl("stocks_yf", type, fixed = TRUE)) {
    processed_data <- download_data_stocks_yf(start_date, end_date, symbols)
  }

  processed_data
}

#' Download Stock Data from Yahoo Finance
#'
#' Downloads historical stock data from Yahoo Finance for given symbols and date range.
#'
#' @param symbols A character vector of stock symbols to download data for. At least one
#'   symbol must be provided.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subset of the dataset is returned.
#'
#' @returns A tibble containing the downloaded stock data with columns: symbol,
#'   date, volume, open, low, high, close, and adjusted_close.
#'
#' @export
#' @examples
#' \dontrun{
#' download_data_stocks_yf(c("AAPL", "MSFT"))
#'
#' download_data_stocks_yf("GOOGL", "2021-01-01", "2022-01-01" )
#' }
download_data_stocks_yf <- function(symbols, start_date = NULL, end_date = NULL) {

  if (!is.character(symbols) || anyNA(symbols)) {
    cli::cli_abort(
      "{.arg symbols} must be character vector, not {.obj_type_friendly {symbols}}."
    )
  }

  rlang::check_installed(
    "httr2", reason = paste0("to download type ", type, ".")
  )

  if (is.null(start_date) || is.null(end_date)) {
    start_date <- Sys.Date() %m-% years(2)
    end_date <- Sys.Date() %m-% years(1)
    cli::cli_inform(c(
      "No {.arg start_date} or {.arg end_date} provided.",
      "Using the range {start_date} to {end_date} to avoid downloading large amounts of data."
    ))
  }
  start_timestamp <- as.integer(as.POSIXct(start_date, tz = "UTC"))
  end_timestamp <- as.integer(as.POSIXct(end_date, tz = "UTC"))

  processed_data <- list()

  for (j in seq_along(symbols)) {
    url <- paste0(
      "https://query2.finance.yahoo.com/v8/finance/chart/",
      symbols[j],
      "?period1=", start_timestamp,
      "&period2=", end_timestamp,
      "&interval=1d"
    )

    response <- httr2::request(url) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()

    if (response$status_code == 200) {
      raw_data <- httr2::resp_body_json(response)$chart$result

      ohlcv <- unlist(raw_data[[1]]$indicators$quote, recursive = FALSE)

      processed_data_symbol <- tibble(
        "symbol" = symbols[j],
        "date" = as.Date(as.POSIXct(as.numeric(raw_data[[1]]$timestamp), origin = "1970-01-01")),
        "volume" = as.numeric(ohlcv$volume),
        "open" = as.numeric(ohlcv$open),
        "low" = as.numeric(ohlcv$low),
        "high" = as.numeric(ohlcv$high),
        "close" = as.numeric(ohlcv$close),
        "adjusted_close" = as.numeric(unlist(raw_data[[1]]$indicators$adjclose))
      )

      processed_data[[j]] <- processed_data_symbol
    } else {
      error_message <- httr2::resp_body_json(response)$chart$error
      cli::cli_warn(
        "Failed to retrieve data for symbol {symbols[j]} with status code {response$status_code} ({error_message$code}): {error_message$description}"
      )
    }
  }
  bind_rows(processed_data)
}
