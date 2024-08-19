#' Download Constituent Data
#'
#' This function downloads and processes the constituent data for a specified financial index.
#' The data is fetched from a remote CSV file, filtered, and cleaned to provide relevant information about constituents.
#'
#' @param index A character string specifying the name of the financial index for which to download constituent data.
#'  The index must be one of the supported indexes listed by the `list_supported_indexes()` function.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{symbol}{The ticker symbol of the equity constituent.}
#'   \item{name}{The name of the equity constituent.}
#'   \item{location}{The location where the company is based.}
#'   \item{exchange}{The exchange where the equity is traded.}
#' }
#' The tibble is filtered to exclude non-equity entries, blacklisted symbols, empty names, and any entries containing the index name or "CASH".
#'
#' @details
#' The function retrieves the URL of the CSV file for the specified index from ETF sites, then sends
#' an HTTP GET request to download the CSV file, and processes the CSV file to extract equity constituents.
#'
#' The approach is inspired by `tidyquant::tq_index()`, which uses a different wrapper around other ETFs.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' download_data_constituents("DAX")
#' }
#'
download_data_constituents <- function(index) {

  rlang::check_installed("httr2")

  symbol_blacklist <- c("", "USD", "GXU4", "EUR", "MARGIN_EUR", "MLIFT")

  supported_indexes <- list_supported_indexes()

  if (!(index %in% supported_indexes$index)) {
    cli::cli_abort("The index '{index}' is not supported. Please use one of the supported indexes: {paste(supported_indexes$index, collapse = ', ')}")
  }

  url <- supported_indexes$url[supported_indexes$index == index]

  response <- httr2::request(url) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  if (response$status_code != 200) {
    cli::cli_abort("Failed to download data for index {.arg index}. Please check the index name or try again later.")
  }

  constituents_raw <- suppressWarnings(httr2::resp_body_string(response)) |>
    textConnection() |>
    read.csv(skip = supported_indexes$skip[supported_indexes$index == index])

  column_names <- paste0(colnames(constituents_raw), collapse = "|")

  if (grepl(column_names, "Anlageklasse")) {
    constituents_processed <- constituents_raw |>
      filter(Anlageklasse == "Aktien") |>
      select(symbol = Emittententicker, name = Name, location = Standort, exchange = `Börse`)
  }
  if (grepl(column_names, "Asset.Class")) {
    constituents_processed <- constituents_raw |>
      filter(Asset.Class == "Equity") |>
      select(symbol = Ticker, name = Name, location = Location, exchange = Exchange)
  }

  constituents_processed <- constituents_processed |>
    filter(!symbol %in% symbol_blacklist) |>
    filter(name != "") |>
    filter(!grepl(tolower(index), tolower(name))) |>
    filter(!grepl("CASH", name)) |>
    filter(!grepl(tolower(gsub("\\s+", "", index)), tolower(name))) |>
    as_tibble() |>
    mutate(symbol = case_when(name == "NATIONAL BANK OF CANADA" ~ "NA", TRUE ~ symbol))

  constituents_processed

}
