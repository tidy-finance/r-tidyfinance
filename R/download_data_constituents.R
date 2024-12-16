#' Download Constituent Data
#'
#' This function downloads and processes the constituent data for a specified financial index.
#' The data is fetched from a remote CSV file, filtered, and cleaned to provide relevant information
#'  about constituents.
#'
#' @param index A character string specifying the name of the financial index for which to download
#'  constituent data. The index must be one of the supported indexes listed by
#'  \link{list_supported_indexes}.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{symbol}{The ticker symbol of the equity constituent.}
#'   \item{name}{The name of the equity constituent.}
#'   \item{location}{The location where the company is based.}
#'   \item{exchange}{The exchange where the equity is traded.}
#' }
#' The tibble is filtered to exclude non-equity entries, blacklisted symbols, empty names, and any
#' entries containing the index name or "CASH".
#'
#' @details
#' The function retrieves the URL of the CSV file for the specified index from ETF sites, then sends
#' an HTTP GET request to download the CSV file, and processes the CSV file to extract equity
#' constituents.
#'
#' The approach is inspired by `tidyquant::tq_index()`, which uses a different wrapper around o
#' ther ETFs.
#'
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_constituents("DAX")
#' }
#'
download_data_constituents <- function(index) {

  rlang::check_installed("httr2")

  symbol_blacklist <- c("", "USD", "GXU4", "EUR", "MARGIN_EUR", "MLIFT")

  supported_indexes <- list_supported_indexes()

  if (!(index %in% supported_indexes$index)) {
    cli::cli_abort(
      "The index '{index}' is not supported. Please use one of the supported indexes: {paste(supported_indexes$index, collapse = ', ')}"
    )
  }

  url <- supported_indexes$url[supported_indexes$index == index]

  user_agent <- get_random_user_agent()

  response <- httr2::request(url) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_perform()

  if (response$status_code != 200) {
    cli::cli_abort(
      "Failed to download data for index {.arg index}. Please check the index name or try again later."
    )
  }

  constituents_raw <- suppressWarnings(httr2::resp_body_string(response)) |>
    textConnection() |>
    read.csv(skip = supported_indexes$skip[supported_indexes$index == index])

  column_names <- paste0(colnames(constituents_raw), collapse = "|")

  if (grepl(column_names, "Anlageklasse")) {
    constituents_processed <- constituents_raw |>
      filter(Anlageklasse == "Aktien") |>
      select(
        symbol = "Emittententicker", name = "Name", location = "Standort", exchange = "B\u00f6rse"
      )
  }
  if (grepl(column_names, "Asset.Class")) {
    constituents_processed <- constituents_raw |>
      filter(Asset.Class == "Equity") |>
      select(
        symbol = "Ticker", name = "Name", location = "Location", exchange = "Exchange"
      )
  }

  constituents_processed <- constituents_processed |>
    filter(!symbol %in% symbol_blacklist) |>
    filter(name != "") |>
    filter(!grepl(tolower(index), tolower(name))) |>
    filter(!grepl("CASH", name)) |>
    filter(!grepl(tolower(gsub("\\s+", "", index)), tolower(name))) |>
    as_tibble() |>
    mutate(
      symbol = case_when(name == "NATIONAL BANK OF CANADA" ~ "NA", TRUE ~ symbol),
      symbol = gsub(" ", "-", symbol),
      symbol = gsub("/", "-", symbol)
    ) |>
    mutate(symbol = case_when(
      exchange %in% c("Xetra", "Deutsche B\u00f6rse AG") ~ paste0(symbol, ".DE"),
      exchange == "Boerse Berlin" ~ paste0(symbol, ".BE"),
      exchange == "Borsa Italiana" ~ paste0(symbol, ".MI"),
      exchange == "Nyse Euronext - Euronext Paris" ~ paste0(symbol, ".PA"),
      exchange == "Euronext Amsterdam" ~ paste0(symbol, ".AS"),
      exchange == "Nasdaq Omx Helsinki Ltd." ~ paste0(symbol, ".HE"),
      exchange == "Singapore Exchange" ~ paste0(symbol, ".SI"),
      exchange == "Asx - All Markets" ~ paste0(symbol, ".AX"),
      exchange == "London Stock Exchange" ~ paste0(symbol, ".L"),
      exchange == "SIX Swiss Exchange" ~ paste0(symbol, ".SW"),
      exchange == "Tel Aviv Stock Exchange" ~ paste0(symbol, ".TA"),
      exchange == "Tokyo Stock Exchange" ~ paste0(symbol, ".T"),
      exchange == "Hong Kong Stock Exchange" ~ paste0(symbol, ".HK"),
      exchange == "Toronto Stock Exchange" ~ paste0(symbol, ".TO"),
      exchange == "Euronext Brussels" ~ paste0(symbol, ".BR"),
      exchange == "Euronext Lisbon" ~ paste0(symbol, ".LS"),
      exchange == "Bovespa" ~ paste0(symbol, ".SA"),
      exchange == "Mexican Stock Exchange" ~ paste0(symbol, ".MX"),
      exchange == "Stockholm Stock Exchange" ~ paste0(symbol, ".ST"),
      exchange == "Oslo Stock Exchange" ~ paste0(symbol, ".OL"),
      exchange == "Johannesburg Stock Exchange" ~ paste0(symbol, ".J"),
      exchange == "Korea Exchange" ~ paste0(symbol, ".KS"),
      exchange == "Shanghai Stock Exchange" ~ paste0(symbol, ".SS"),
      exchange == "Shenzhen Stock Exchange" ~ paste0(symbol, ".SZ"),
      TRUE ~ symbol)) |>
    mutate(symbol = gsub("\\.\\.", "\\.", symbol)) |>
    mutate(currency = case_when(
      exchange %in% c("Xetra", "Boerse Berlin", "Deutsche B\u00f6rse AG") ~ "EUR",
      exchange == "Borsa Italiana" ~ "EUR",
      exchange == "Nyse Euronext - Euronext Paris" ~ "EUR",
      exchange == "Euronext Amsterdam" ~ "EUR",
      exchange == "Nasdaq Omx Helsinki Ltd." ~ "EUR",
      exchange == "Singapore Exchange" ~ "SGD",
      exchange == "Asx - All Markets" ~ "AUD",
      exchange == "London Stock Exchange" ~ "GBP",
      exchange == "SIX Swiss Exchange" ~ "CHF",
      exchange == "Tel Aviv Stock Exchange" ~ "ILS",
      exchange == "Tokyo Stock Exchange" ~ "JPY",
      exchange == "Hong Kong Stock Exchange" ~ "HKD",
      exchange == "Toronto Stock Exchange" ~ "CAD",
      exchange == "Euronext Brussels" ~ "EUR",
      exchange == "Euronext Lisbon" ~ "EUR",
      exchange == "Bovespa" ~ "BRL",
      exchange == "Mexican Stock Exchange" ~ "MXN",
      exchange == "Stockholm Stock Exchange" ~ "SEK",
      exchange == "Oslo Stock Exchange" ~ "NOK",
      exchange == "Johannesburg Stock Exchange" ~ "ZAR",
      exchange == "Korea Exchange" ~ "KRW",
      exchange == "Shanghai Stock Exchange" ~ "CNY",
      exchange == "Shenzhen Stock Exchange" ~ "CNY",
      TRUE ~ "USD")
    )

  constituents_processed

}
