download_data_stocks <- function(type = "stocks_yf", start_date, end_date, symbol) {

  check_supported_type(type) # TODO: add stock_yf and stocks_gf to types

  if (grepl("stocks_yf", type, fixed = TRUE)) {
    processed_data <- download_data_stocks_yf(start_date, end_date, symbol)
  }
  if (grepl("stocks_gf", type, fixed = TRUE)) {
    processed_data <- download_data_stocks_gf(type, start_date, end_date, symbol)
  }

  processed_data
}


download_data_stocks_yf <- function(start_date, end_date, symbol) {

  start_timestamp <- as.integer(as.POSIXct(start_date, tz = "UTC"))
  end_timestamp <- as.integer(as.POSIXct(end_date, tz = "UTC"))

  processed_data <- list()

  for (j in 1:length(symbol)) {
    url <- paste0(
      "https://query2.finance.yahoo.com/v8/finance/chart/",
      symbol[j],
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

      processed_data_symbol <- tibble::tibble(
        "symbol" = symbol[j],
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
      warning("Failed to retrieve data for symbol '", symbol, "' with status code ", response$status_code,
              " (", error_message$code, "): ", error_message$description)
    }
  }
  dplyr::bind_rows(processed_data)
}

download_data_stocks_yf(start_date = Sys.Date()-30, end_date = Sys.Date()-1, symbol = "MSFT")

download_data_stocks_yf(start_date = Sys.Date()-30, end_date = Sys.Date()-1, symbol = c("AAPL", "MSFT"))

download_data_stocks_yf(start_date = Sys.Date()-30, end_date = Sys.Date()-1, symbol = c("AAPL", "MSFT", "FNSALASFBK"))
