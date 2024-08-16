download_data_constituents <- function(type, index) {

  rlang::check_installed("openxlsx")

  index <- switch(
    index,
    "Russell 1000" = "onek",
    "Russell 2000" = "twok",
    "Russell 3000" = "thrk",
    "Dow Jones Industrial Average" = "dia",
    "Dow Jones Global" = "dgt",
    "S&P 400" = "mdy",
    "S&P 500" = "spy",
    "S&P 600" = "slyg",
    "S&P 1000" = "smd",
    "EURO STOXX 50" = "fez"
  )

  url <- paste0(
    "https://www.ssga.com/us/en/institutional/etfs/library-content/products/fund-data/etfs/us/holdings-daily-us-en-",
    tolower(index), ".xlsx"
  )

  constituents_raw <- openxlsx::read.xlsx(url, startRow = 5) |>
    tibble::as_tibble() |>
    tidyr::drop_na()

  constituents <- constituents_raw |>
    select(symbol = Ticker, sedol = SEDOL, name = Name) |>
    filter(name != "US DOLLAR")

  constituents
}
