#' Download Constituent Data
#'
#' Downloads and processes the constituent data for a specified financial
#' index. The data is fetched from a remote CSV file, filtered, and cleaned to
#' provide relevant information about constituents.
#'
#' @details
#' The function retrieves the URL of the CSV file for the specified index from
#' ETF sites, then sends an HTTP GET request to download the CSV file, and
#' processes the CSV file to extract equity constituents.
#'
#' The approach is inspired by `tidyquant::tq_index()`, which uses a different
#' wrapper around other ETFs.
#'
#' @param index A character string specifying the name of the financial index
#'   for which to download constituent data. The index must be one of the
#'   supported indexes listed by [list_supported_indexes()]. Optional when
#'   `path` is given.
#' @param path Optional. Path to a local iShares or BlackRock holdings CSV, as
#'   saved from the fund's web page, to read instead of downloading. Use it as
#'   a fallback when the download fails. `index` is optional in this case; when
#'   given, entries whose name contains the index name are dropped.
#'
#' @returns A tibble with five columns:
#' \describe{
#'   \item{symbol}{The ticker symbol of the equity constituent.}
#'   \item{name}{The name of the equity constituent.}
#'   \item{location}{The location where the company is based.}
#'   \item{exchange}{The exchange where the equity is traded.}
#'   \item{currency}{The currency in which the equity is traded, derived
#'     from the exchange.}
#' }
#' The tibble is filtered to exclude non-equity entries, blacklisted symbols,
#' empty names, entries containing "CASH", and, when `index` is given, entries
#' containing the index name.
#'
#' @family download functions
#' @export
#'
#' @examples
#' \donttest{
#'   download_data_constituents("DAX")
#' }
#' \dontrun{
#'   download_data_constituents(path = "DAXEX_holdings.csv")
#' }
#'
download_data_constituents <- function(index = NULL, path = NULL) {
  symbol_blacklist <- c("", "-", "USD", "GXU4", "EUR", "MARGIN_EUR", "MLIFT")

  if (is.null(index) && is.null(path)) {
    cli::cli_abort(
      paste(
        "Pass {.arg index} to download constituents or {.arg path} to read a",
        "holdings file, e.g.",
        "{.code download_data(\"Index Constituents\",",
        "path = \"holdings.csv\")}."
      )
    )
  }

  if (!is.null(path)) {
    raw <- readBin(path, "raw", file.size(path))
    text <- rawToChar(raw)
    if (validUTF8(text)) {
      Encoding(text) <- "UTF-8"
      text <- sub("^\ufeff", "", text)
    } else {
      text <- iconv(text, from = "latin1", to = "UTF-8")
    }
  } else {
    supported_indexes <- list_supported_indexes()

    if (!(index %in% supported_indexes$index)) {
      cli::cli_abort(
        paste(
          "The index '{index}' is not supported.",
          "Please use one of the supported indexes:",
          "{paste(supported_indexes$index, collapse = ', ')}"
        )
      )
    }

    url <- supported_indexes$url[supported_indexes$index == index]

    user_agent <- get_random_user_agent()

    response <- handle_download_error(
      function() {
        httr2::request(url) |>
          httr2::req_error(is_error = \(resp) FALSE) |>
          httr2::req_user_agent(user_agent) |>
          httr2::req_perform()
      },
      fallback = NULL
    )

    if (is.null(response)) {
      return(NULL)
    }

    if (response$status_code != 200) {
      cli::cli_abort(
        c(
          "Failed to download data for index {.val {index}} from {.url {url}}.",
          "i" = paste(
            "The provider may have moved the file. As a fallback, download the",
            "holdings CSV from the fund's web page in your browser and pass it",
            "via {.arg path}, e.g.",
            "{.code download_data(\"Index Constituents\",",
            "path = \"holdings.csv\")}."
          )
        )
      )
    }

    text <- suppressWarnings(httr2::resp_body_string(response))
  }

  # Find the header row instead of skipping a fixed number of rows.
  lines <- strsplit(text, "\r?\n")[[1]]
  start <- which(grepl("Anlageklasse|Asset Class", lines))[1]
  if (is.na(start)) {
    cli::cli_abort("Unknown column format in downloaded data.")
  }

  constituents_raw <- read.csv(
    text = lines[start:length(lines)],
    check.names = FALSE
  )

  if ("Anlageklasse" %in% colnames(constituents_raw)) {
    constituents_processed <- constituents_raw |>
      filter(.data$Anlageklasse == "Aktien") |>
      select(
        symbol = "Emittententicker",
        name = "Name",
        location = "Standort",
        exchange = "B\u00f6rse"
      )
  } else {
    constituents_processed <- constituents_raw |>
      filter(.data[["Asset Class"]] == "Equity") |>
      select(
        symbol = "Ticker",
        name = "Name",
        location = "Location",
        exchange = "Exchange"
      )
  }

  constituents_processed <- constituents_processed |>
    mutate(symbol = trimws(.data$symbol)) |>
    filter(!.data$symbol %in% symbol_blacklist) |>
    filter(!grepl("CASH", .data$name))

  if (!is.null(index)) {
    constituents_processed <- constituents_processed |>
      filter(!grepl(tolower(index), tolower(.data$name))) |>
      filter(
        !grepl(tolower(gsub("\\s+", "", index)), tolower(.data$name))
      )
  }

  constituents_processed <- constituents_processed |>
    as_tibble() |>
    mutate(
      symbol = case_when(
        name == "NATIONAL BANK OF CANADA" ~ "NA",
        TRUE ~ .data$symbol
      ),
      symbol = gsub(" ", "-", .data$symbol),
      symbol = gsub("/", "-", .data$symbol)
    ) |>
    mutate(
      symbol = case_when(
        exchange %in% c("Xetra", "Deutsche B\u00f6rse AG") ~
          paste0(.data$symbol, ".DE"),
        exchange == "Boerse Berlin" ~ paste0(.data$symbol, ".BE"),
        exchange == "Borsa Italiana" ~ paste0(.data$symbol, ".MI"),
        exchange == "Nyse Euronext - Euronext Paris" ~
          paste0(.data$symbol, ".PA"),
        exchange == "Euronext Amsterdam" ~ paste0(.data$symbol, ".AS"),
        exchange == "Nasdaq Omx Helsinki Ltd." ~ paste0(
          .data$symbol,
          ".HE"
        ),
        exchange == "Singapore Exchange" ~ paste0(.data$symbol, ".SI"),
        exchange == "Asx - All Markets" ~ paste0(.data$symbol, ".AX"),
        exchange == "London Stock Exchange" ~ paste0(
          .data$symbol,
          ".L"
        ),
        exchange == "SIX Swiss Exchange" ~ paste0(.data$symbol, ".SW"),
        exchange == "Tel Aviv Stock Exchange" ~ paste0(
          .data$symbol,
          ".TA"
        ),
        exchange == "Tokyo Stock Exchange" ~ paste0(
          .data$symbol,
          ".T"
        ),
        exchange == "Hong Kong Stock Exchange" ~ paste0(
          .data$symbol,
          ".HK"
        ),
        exchange == "Toronto Stock Exchange" ~ paste0(
          .data$symbol,
          ".TO"
        ),
        exchange == "Euronext Brussels" ~ paste0(.data$symbol, ".BR"),
        exchange == "Euronext Lisbon" ~ paste0(.data$symbol, ".LS"),
        exchange == "Bovespa" ~ paste0(.data$symbol, ".SA"),
        exchange == "Mexican Stock Exchange" ~ paste0(
          .data$symbol,
          ".MX"
        ),
        exchange == "Stockholm Stock Exchange" ~ paste0(
          .data$symbol,
          ".ST"
        ),
        exchange == "Oslo Stock Exchange" ~ paste0(
          .data$symbol,
          ".OL"
        ),
        exchange == "Johannesburg Stock Exchange" ~ paste0(
          .data$symbol,
          ".J"
        ),
        exchange == "Korea Exchange" ~ paste0(.data$symbol, ".KS"),
        exchange == "Shanghai Stock Exchange" ~ paste0(
          .data$symbol,
          ".SS"
        ),
        exchange == "Shenzhen Stock Exchange" ~ paste0(
          .data$symbol,
          ".SZ"
        ),
        TRUE ~ .data$symbol
      )
    ) |>
    mutate(symbol = gsub("\\.\\.", "\\.", .data$symbol)) |>
    mutate(
      currency = case_when(
        exchange %in%
          c("Xetra", "Boerse Berlin", "Deutsche B\u00f6rse AG") ~
          "EUR",
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
        TRUE ~ "USD"
      )
    )

  constituents_processed
}
