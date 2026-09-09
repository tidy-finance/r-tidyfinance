#' Download Stock Data
#'
#' Downloads historical stock data from Yahoo Finance for given symbols and date
#' range.
#'
#' @param symbols A character vector of stock symbols to download data for. At
#'   least one symbol must be provided.
#' @param start_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the start date for the data. If not provided, a one-year
#'   subset of the dataset is returned (see [validate_dates()]).
#' @param end_date Optional. A character string or Date object in "YYYY-MM-DD"
#'   format specifying the end date for the data. If not provided, a one-year
#'   subset of the dataset is returned (see [validate_dates()]).
#'
#' @details Dates are the trading days in the exchange's local time zone, as
#'   reported by Yahoo Finance, and the range is inclusive of both
#'   `start_date` and `end_date`.
#'
#' @returns A tibble containing the downloaded stock data with columns: symbol,
#'   date, volume, open, low, high, close, and adjusted_close.
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_stock_prices(c("AAPL", "MSFT"))
#'   download_data_stock_prices("GOOGL", "2021-01-01", "2022-01-01" )
#' }
download_data_stock_prices <- function(
  symbols,
  start_date = NULL,
  end_date = NULL
) {
  if (!is.character(symbols) || anyNA(symbols)) {
    cli::cli_abort(
      paste(
        "{.arg symbols} must be character vector,",
        "not {.obj_type_friendly {symbols}}."
      )
    )
  }

  dates <- validate_dates(start_date, end_date, use_default_range = TRUE)
  start_date <- dates$start_date
  end_date <- dates$end_date

  # Yahoo Finance resolves `period1` and `period2` against the exchange's local
  # time zone, so a window expressed in UTC covers a different set of trading
  # days for each market. Request a buffer around the range and filter on the
  # converted dates below, which makes both bounds inclusive everywhere.
  request_buffer <- 2
  start_timestamp <- as.integer(
    as.POSIXct(start_date - request_buffer, tz = "UTC")
  )
  end_timestamp <- as.integer(as.POSIXct(end_date + request_buffer, tz = "UTC"))

  processed_data <- list()

  cli::cli_progress_bar(
    "Downloading symbols",
    total = length(symbols),
    clear = TRUE
  )

  for (j in seq_along(symbols)) {
    url <- paste0(
      "https://query2.finance.yahoo.com/v8/finance/chart/",
      symbols[j],
      "?period1=",
      start_timestamp,
      "&period2=",
      end_timestamp,
      "&interval=1d"
    )

    response <- httr2::request(url) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()

    if (response$status_code == 200) {
      raw_data <- httr2::resp_body_json(response)$chart$result

      replace_null_with_na <- function(x) {
        if (is.list(x)) {
          lapply(x, replace_null_with_na)
        } else if (is.null(x)) {
          NA
        } else {
          x
        }
      }

      ohlcv <- unlist(raw_data[[1]]$indicators$quote, recursive = FALSE)
      ohlcv <- replace_null_with_na(ohlcv)

      indicators <- raw_data[[1]]$indicators$adjclose
      indicators <- replace_null_with_na(indicators)

      # Daily timestamps refer to the market open in the exchange's local time
      # zone, so the dates must be derived in that zone. Converting in UTC
      # shifts markets ahead of UTC (e.g. Australia, New Zealand) to the
      # previous calendar day.
      exchange_timezone <- raw_data[[1]]$meta$exchangeTimezoneName
      if (is.null(exchange_timezone) || !nzchar(exchange_timezone)) {
        exchange_timezone <- "UTC"
      }

      processed_data_symbol <- tibble(
        "symbol" = symbols[j],
        "date" = as.Date(
          as.POSIXct(
            as.numeric(raw_data[[1]]$timestamp),
            origin = "1970-01-01",
            tz = "UTC"
          ),
          tz = exchange_timezone
        ),
        "volume" = as.numeric(unlist(ohlcv$volume)),
        "open" = as.numeric(unlist(ohlcv$open)),
        "low" = as.numeric(unlist(ohlcv$low)),
        "high" = as.numeric(unlist(ohlcv$high)),
        "close" = as.numeric(unlist(ohlcv$close)),
        "adjusted_close" = as.numeric(unlist(indicators))
      )

      processed_data[[j]] <- processed_data_symbol |>
        filter(between(.data$date, start_date, end_date))
    } else {
      error_message <- # nolint: object_usage_linter.
        httr2::resp_body_json(response)$chart$error
      cli::cli_warn(c(
        "Failed to retrieve data for symbol {symbols[j]} with ",
        "status code {response$status_code} ({error_message$code}): ",
        "{error_message$description}"
      ))
    }
    cli::cli_progress_update()
  }

  bind_rows(processed_data)
}
