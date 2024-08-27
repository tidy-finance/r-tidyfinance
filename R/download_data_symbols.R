#' Download Stock Symbols from an Exchange
#'
#' This function downloads a list of stock symbols from a specified exchange (NYSE, AMEX, or
#' NASDAQ). The data includes details such as symbol, company name, country, IPO year, industry,
#' and sector.
#'
#' @param exchange A character string specifying the exchange to download data from.
#'                 Must be one of "NYSE", "AMEX", or "NASDAQ".
#'
#' @return A data frame containing the stock symbols and related information from the specified
#'  exchange.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Download stock symbols from NYSE
#' symbols_nyse <- download_data_symbols("NYSE")
#'
#' # Download stock symbols from NASDAQ
#' symbols_nasdaq <- download_data_symbols("NASDAQ")
#' }
download_data_symbols <- function(exchange) {

  rlang::check_installed("httr2", reason = "to download symbols from exchanges.")

  if (!exchange %in% c("NYSE", "AMEX", "NASDAQ")) {
    cli::cli_abort("{.arg exchange} must be 'NYSE', 'AMEX', or 'NASDAQ'.")
  }

  user_agent <- get_random_user_agent()
  url <- paste0("https://api.nasdaq.com/api/screener/stocks?tableonly=true&exchange=", exchange, "&download=true")

  response <- httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_perform()

  body <- response |>
    httr2::resp_body_json()

  data_raw <- bind_rows(body$data$rows)

  data_processed <- data_raw |>
    select(symbol, name, country, ipo_year = ipoyear, industry, sector)

  data_processed
}


