test_that("unsupported indexes fail", {
  testthat::local_mocked_bindings(
    list_supported_indexes = function() {
      tibble::tibble(index = "DAX", url = "url", skip = 0)
    }
  )

  expect_error(
    download_data_constituents("UNKNOWN"),
    "not supported"
  )
})

test_that("download errors can return NULL", {
  testthat::local_mocked_bindings(
    list_supported_indexes = function() {
      tibble::tibble(index = "DAX", url = "url", skip = 0)
    },
    get_random_user_agent = function() "ua",
    handle_download_error = function(expr, fallback) fallback
  )

  expect_null(download_data_constituents("DAX"))
})

test_that("non-200 responses fail", {
  testthat::local_mocked_bindings(
    list_supported_indexes = function() {
      tibble::tibble(index = "DAX", url = "url", skip = 0)
    },
    get_random_user_agent = function() "ua",
    handle_download_error = function(expr, fallback) {
      list(status_code = 500)
    }
  )

  expect_error(
    download_data_constituents("DAX"),
    "Failed to download data"
  )
})

test_that("German CSV layout is parsed and cleaned", {
  csv <- paste(
    "Anlageklasse,Emittententicker,Name,Standort,Börse",
    "Aktien, ABC ,Alpha AG,Germany,Xetra",
    "Aktien,-,Bad AG,Germany,Xetra",
    "Bonds,BND,Bond AG,Germany,Xetra",
    "Aktien,CASH,CASH,Germany,Xetra",
    "Aktien,DAX,DAX INDEX,Germany,Xetra",
    sep = "\n"
  )

  testthat::local_mocked_bindings(
    list_supported_indexes = function() {
      tibble::tibble(index = "DAX", url = "url", skip = 0)
    },
    get_random_user_agent = function() "ua",
    handle_download_error = function(expr, fallback) {
      list(status_code = 200, body = csv)
    }
  )
  testthat::local_mocked_bindings(
    .package = "httr2",
    resp_body_string = function(resp) resp$body
  )

  out <- download_data_constituents("DAX")

  expect_equal(out$symbol, "ABC.DE")
  expect_equal(out$currency, "EUR")
  expect_s3_class(out, "tbl_df")
})

test_that("Asset Class layout covers exchanges and symbol rules", {
  exchanges <- c(
    "Deutsche Börse AG",
    "Boerse Berlin",
    "Borsa Italiana",
    "Nyse Euronext - Euronext Paris",
    "Euronext Amsterdam",
    "Nasdaq Omx Helsinki Ltd.",
    "Singapore Exchange",
    "Asx - All Markets",
    "London Stock Exchange",
    "SIX Swiss Exchange",
    "Tel Aviv Stock Exchange",
    "Tokyo Stock Exchange",
    "Hong Kong Stock Exchange",
    "Toronto Stock Exchange",
    "Euronext Brussels",
    "Euronext Lisbon",
    "Bovespa",
    "Mexican Stock Exchange",
    "Stockholm Stock Exchange",
    "Oslo Stock Exchange",
    "Johannesburg Stock Exchange",
    "Korea Exchange",
    "Shanghai Stock Exchange",
    "Shenzhen Stock Exchange",
    "Other Exchange"
  )

  rows <- paste(
    "Equity",
    paste0("SYM", seq_along(exchanges)),
    paste0("Company ", seq_along(exchanges)),
    "Loc",
    exchanges,
    sep = ","
  )

  rows[1] <- paste(
    "Equity",
    "BAD SYM/PR..",
    "NATIONAL BANK OF CANADA",
    "Loc",
    exchanges[1],
    sep = ","
  )

  csv <- paste(
    "Asset Class,Ticker,Name,Location,Exchange",
    paste(rows, collapse = "\n"),
    "Cash,IGN,Ignored,Loc,Other Exchange",
    "Equity,USD,Bad,Loc,Other Exchange",
    "Equity,IDX,MSCI WORLD,Loc,Other Exchange",
    sep = "\n"
  )

  testthat::local_mocked_bindings(
    list_supported_indexes = function() {
      tibble::tibble(index = "MSCI World", url = "url", skip = 0)
    },
    get_random_user_agent = function() "ua",
    handle_download_error = function(expr, fallback) {
      list(status_code = 200, body = csv)
    }
  )
  testthat::local_mocked_bindings(
    .package = "httr2",
    resp_body_string = function(resp) resp$body
  )

  out <- download_data_constituents("MSCI World")

  expect_equal(nrow(out), length(exchanges))
  expect_true("NA.DE" %in% out$symbol)
  expect_true("SYM2.BE" %in% out$symbol)
  expect_true("SYM3.MI" %in% out$symbol)
  expect_true("SYM4.PA" %in% out$symbol)
  expect_true("SYM5.AS" %in% out$symbol)
  expect_true("SYM6.HE" %in% out$symbol)
  expect_true("SYM7.SI" %in% out$symbol)
  expect_true("SYM8.AX" %in% out$symbol)
  expect_true("SYM9.L" %in% out$symbol)
  expect_true("SYM10.SW" %in% out$symbol)
  expect_true("SYM11.TA" %in% out$symbol)
  expect_true("SYM12.T" %in% out$symbol)
  expect_true("SYM13.HK" %in% out$symbol)
  expect_true("SYM14.TO" %in% out$symbol)
  expect_true("SYM15.BR" %in% out$symbol)
  expect_true("SYM16.LS" %in% out$symbol)
  expect_true("SYM17.SA" %in% out$symbol)
  expect_true("SYM18.MX" %in% out$symbol)
  expect_true("SYM19.ST" %in% out$symbol)
  expect_true("SYM20.OL" %in% out$symbol)
  expect_true("SYM21.J" %in% out$symbol)
  expect_true("SYM22.KS" %in% out$symbol)
  expect_true("SYM23.SS" %in% out$symbol)
  expect_true("SYM24.SZ" %in% out$symbol)
  expect_true("SYM25" %in% out$symbol)

  expect_setequal(
    out$currency,
    c(
      "EUR",
      "SGD",
      "AUD",
      "GBP",
      "CHF",
      "ILS",
      "JPY",
      "HKD",
      "CAD",
      "BRL",
      "MXN",
      "SEK",
      "NOK",
      "ZAR",
      "KRW",
      "CNY",
      "USD"
    )
  )
})

test_that("download request pipeline is executed", {
  csv <- paste(
    "Asset Class,Ticker,Name,Location,Exchange",
    "Equity,AAPL,Apple Inc,United States,Nasdaq",
    sep = "\n"
  )

  testthat::local_mocked_bindings(
    list_supported_indexes = function() {
      tibble::tibble(index = "S&P 500", url = "mock-url", skip = 0)
    },
    get_random_user_agent = function() "test-agent",
    handle_download_error = function(expr, fallback) expr()
  )

  testthat::local_mocked_bindings(
    .package = "httr2",
    request = function(url) {
      expect_equal(url, "mock-url")
      list(url = url)
    },
    req_error = function(req, is_error) {
      expect_false(is_error(list()))
      req$error_checked <- TRUE
      req
    },
    req_user_agent = function(req, user_agent) {
      expect_equal(user_agent, "test-agent")
      req$user_agent <- user_agent
      req
    },
    req_perform = function(req) {
      expect_true(req$error_checked)
      expect_equal(req$user_agent, "test-agent")
      list(status_code = 200, body = csv)
    },
    resp_body_string = function(resp) resp$body
  )

  out <- download_data_constituents("S&P 500")

  expect_equal(out$symbol, "AAPL")
  expect_equal(out$currency, "USD")
})
