#' Download Stock Data
#'
#' Downloads historical stock data for a given symbol and date range from specified sources.
#'
#' @param type A character string representing the data source type. Currently supported type is "stocks_yf" for Yahoo Finance. Defaults to "stocks_yf".
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subset of the dataset is returned.
#' @param symbols A character vector of stock symbols to download data for. At least one
#'   symbol must be provided.
#'
#' @return A tibble containing the downloaded stock data with columns depending on the data source.
#'
#' @examples
#' \dontrun{
#' download_data_stocks(type = "stocks_yf", symbols = c("AAPL", "MSFT"))
#' }
#'
#' @export
download_data_stocks <- function(type, start_date, end_date, symbols) {

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
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the start date for the data. If not provided, a subset of the dataset is returned.
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD" format
#'   specifying the end date for the data. If not provided, a subset of the dataset is returned.
#' @param symbols A character vector of stock symbols to download data for. At least one
#'   symbol must be provided.
#'
#' @return A tibble containing the downloaded stock data with columns: symbol,
#'   date, volume, open, low, high, close, and adjusted_close.
#'
#' @examples
#' \dontrun{
#' download_data_stocks_yf(symbols = c("AAPL", "MSFT"))
#'
#' download_data_stocks_yf("2021-01-01", "2022-01-01", symbols = "GOOGL")
#' }
#' @export
download_data_stocks_yf <- function(start_date, end_date, symbols) {

  if (missing(symbols)) {
    stop("Error: No symbol provided. Please provide at least one symbol.")
  }

  check_if_package_installed("httr2", "stocks_yf")
  request <- getNamespace("httr2")$request
  req_error <- getNamespace("httr2")$req_error
  req_perform <- getNamespace("httr2")$req_perform
  resp_body_json <- getNamespace("httr2")$resp_body_json

  if (missing(start_date) || missing(end_date)) {
    start_date <- Sys.Date() %m-% years(2)
    end_date <- Sys.Date() %m-% years(1)
    message("No start_date or end_date provided. Using the range ",
            start_date, " to ", end_date, " to avoid downloading large amounts of data.")
    start_timestamp <- as.integer(as.POSIXct(start_date, tz = "UTC"))
    end_timestamp <- as.integer(as.POSIXct(end_date, tz = "UTC"))
  } else {
    start_timestamp <- as.integer(as.POSIXct(start_date, tz = "UTC"))
    end_timestamp <- as.integer(as.POSIXct(end_date, tz = "UTC"))
  }

  processed_data <- list()

  for (j in 1:length(symbols)) {
    url <- paste0(
      "https://query2.finance.yahoo.com/v8/finance/chart/",
      symbols[j],
      "?period1=", start_timestamp,
      "&period2=", end_timestamp,
      "&interval=1d"
    )

    response <- request(url) |>
      req_error(is_error = \(resp) FALSE) |>
      req_perform()

    if (response$status_code == 200) {
      raw_data <- resp_body_json(response)$chart$result

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
      error_message <- resp_body_json(response)$chart$error
      warning("Failed to retrieve data for symbol '", symbols[j],
              "' with status code ", response$status_code,
              " (", error_message$code, "): ", error_message$description)
    }
  }
  dplyr::bind_rows(processed_data)
}
