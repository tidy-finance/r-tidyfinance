#' List Supported Indexes
#'
#' This function returns a tibble containing information about supported financial indexes.
#' Each index is associated with a URL that points to a CSV file containing the holdings of the index.
#' Additionally, each index has a corresponding `skip` value, which indicates the number of lines
#' to skip when reading the CSV file.
#'
#' @return A tibble with three columns:
#' \describe{
#'   \item{index}{The name of the financial index (e.g., "DAX", "S&P 500").}
#'   \item{url}{The URL to the CSV file containing the holdings data for the index.}
#'   \item{skip}{The number of lines to skip when reading the CSV file.}
#' }
#'
#' @export
#'
#' @examples
#' supported_indexes <- list_supported_indexes()
#' print(supported_indexes)
#'
list_supported_indexes <- function() {
  supported_indexes <- tribble(
    ~index, ~url, ~skip,
    "DAX", "https://www.ishares.com/de/privatanleger/de/produkte/251464/ishares-dax-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=DAXEX_holdings&dataType=fund", 2,
    "EURO STOXX 50", "https://www.ishares.com/de/privatanleger/de/produkte/251783/ishares-euro-stoxx-50-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=EXW1_holdings&dataType=fund", 2,
    "Dow Jones Industrial Average", "https://www.ishares.com/de/privatanleger/de/produkte/251770/ishares-dow-jones-industrial-average-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=EXI3_holdings&dataType=fund", 2,
    "Russell 1000", "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239707/ishares-russell-1000-etf/1495092304805.ajax?fileType=csv&fileName=IWB_holdings&dataType=fund", 9,
    "Russell 2000", "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239710/ishares-russell-2000-etf/1495092304805.ajax?fileType=csv&fileName=IWM_holdings&dataType=fund", 9,
    "Russell 3000", "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239714/ishares-russell-3000-etf/1495092304805.ajax?fileType=csv&fileName=IWV_holdings&dataType=fund", 9,
    "S&P 100", "https://www.ishares.com/ch/professionelle-anleger/de/produkte/239723/ishares-sp-100-etf/1495092304805.ajax?fileType=csv&fileName=OEF_holdings&dataType=fund", 9,
    "S&P 500", "https://www.ishares.com/de/privatanleger/de/produkte/253743/ishares-sp-500-b-ucits-etf-acc-fund/1478358465952.ajax?fileType=csv&fileName=SXR8_holdings&dataType=fund", 2,
    "Nasdaq 100", "https://www.ishares.com/de/privatanleger/de/produkte/251896/ishares-nasdaq100-ucits-etf-de-fund/1478358465952.ajax?fileType=csv&fileName=EXXT_holdings&dataType=fund", 2,
    "FTSE 100", "https://www.ishares.com/de/privatanleger/de/produkte/251795/ishares-ftse-100-ucits-etf-inc-fund/1478358465952.ajax?fileType=csv&fileName=IUSZ_holdings&dataType=fund", 2,
    "MSCI World", "https://www.ishares.com/de/privatanleger/de/produkte/251882/ishares-msci-world-ucits-etf-acc-fund/1478358465952.ajax?fileType=csv&fileName=EUNL_holdings&dataType=fund", 2,
    "Nikkei 225", "https://www.ishares.com/ch/professionelle-anleger/de/produkte/253742/ishares-nikkei-225-ucits-etf/1495092304805.ajax?fileType=csv&fileName=CSNKY_holdings&dataType=fund", 2,
    "TOPIX", "https://www.blackrock.com/jp/individual-en/en/products/279438/fund/1480664184455.ajax?fileType=csv&fileName=1475_holdings&dataType=fund", 2
  )
}

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
#' }
#' The tibble is filtered to exclude non-equity entries, blacklisted symbols, empty names, and any entries containing the index name or "CASH".
#'
#' @details The function performs the following steps:
#' \enumerate{
#'   \item Retrieves the URL of the CSV file for the specified index using the `list_supported_indexes()` function.
#'   \item Sends an HTTP GET request to download the CSV file.
#'   \item Reads and processes the CSV file to extract equity constituents.
#'   \item Filters out blacklisted symbols and other irrelevant data.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dax_constituents <- download_data_constituents("DAX")
#' print(dax_constituents)
#' }
#'
download_data_constituents <- function(index) {

  rlang::check_installed("httr2")

  symbol_blacklist <- c("", "USD", "GXU4", "EUR", "MARGIN_EUR", "MLIFT")

  supported_indexes <- list_supported_indexes()
  url <- supported_indexes$url[supported_indexes$index == index]

  response <- httr2::request(url) |>
    httr2::req_perform()

  constituents_raw <- suppressWarnings(httr2::resp_body_string(response)) |>
    textConnection() |>
    read.csv(skip = supported_indexes$skip[supported_indexes$index == index])

  column_names <- paste0(colnames(constituents_raw), collapse = "|")

  if (grepl(column_names, "Anlageklasse")) {
    constituents_processed <- constituents_raw |>
      filter(Anlageklasse == "Aktien") |>
      select(symbol = Emittententicker, name = Name)
  }
  if (grepl(column_names, "Asset.Class")) {
    constituents_processed <- constituents_raw |>
      filter(Asset.Class == "Equity") |>
      select(symbol = Ticker, name = Name)
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
