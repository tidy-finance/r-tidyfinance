mock_sentiment_data <- function(yearmo = c(196507, 196508)) {
  n <- length(yearmo)
  tibble(
    yearmo = yearmo,
    SENT = seq(-0.5, by = 0.5, length.out = n),
    SENT_ORTH = c(NA, seq(0.1, by = 0.1, length.out = n - 1)),
    pdnd = rep(1, n),
    ripo = rep(2, n),
    nipo = rep(3, n),
    cefd = rep(4, n),
    s = rep(0.1, n),
    indpro = rep(20, n),
    consdur = rep(30, n),
    consnon = rep(40, n),
    consserv = rep(50, n),
    recess = rep(0, n),
    employ = rep(60000, n),
    cpi = rep(31.5, n)
  )
}

no_dates <- function(start_date, end_date) {
  list(start_date = NULL, end_date = NULL)
}

test_that("downloads and processes the monthly sentiment index", {
  local_mocked_bindings(
    validate_dates = no_dates,
    read_baker_wurgler_file = function(url) mock_sentiment_data()
  )

  result <- download_data_baker_wurgler()

  expect_s3_class(result, "tbl_df")
  expect_equal(
    names(result),
    c(
      "date", "sentiment", "sentiment_orth", "pdnd", "ripo", "nipo", "cefd",
      "s", "indpro", "consdur", "consnon", "consserv", "recess", "employ",
      "cpi"
    )
  )
  # Dates are aligned to the beginning of the month.
  expect_equal(result$date, ymd(c("1965-07-01", "1965-08-01")))
  expect_equal(result$sentiment, c(-0.5, 0))
  expect_equal(result$sentiment_orth, c(NA, 0.1))
  expect_equal(result$cpi, c(31.5, 31.5))
})

test_that("drops rows without a date", {
  local_mocked_bindings(
    validate_dates = no_dates,
    read_baker_wurgler_file = function(url) {
      mock_sentiment_data(yearmo = c(196507, NA))
    }
  )

  result <- download_data_baker_wurgler()

  expect_equal(result$date, ymd("1965-07-01"))
})

test_that("filters rows when both dates are supplied", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = ymd("1965-08-01"), end_date = ymd("1965-08-31"))
    },
    read_baker_wurgler_file = function(url) {
      mock_sentiment_data(yearmo = c(196507, 196508, 196509))
    }
  )

  result <- download_data_baker_wurgler(
    start_date = "1965-08-01",
    end_date = "1965-08-31"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$date, ymd("1965-08-01"))
})

test_that("warns when the requested range is outside the available data", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = ymd("2100-01-01"), end_date = ymd("2100-12-31"))
    },
    read_baker_wurgler_file = function(url) mock_sentiment_data()
  )

  expect_warning(
    result <- download_data_baker_wurgler(
      start_date = "2100-01-01",
      end_date = "2100-12-31"
    ),
    "outside the available"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("aborts when the source layout changes", {
  local_mocked_bindings(
    validate_dates = no_dates,
    read_baker_wurgler_file = function(url) {
      dplyr::select(mock_sentiment_data(), -"SENT_ORTH")
    }
  )

  expect_error(download_data_baker_wurgler(), "Missing columns")
})

test_that("aborts on unsupported dataset", {
  expect_error(
    download_data_baker_wurgler(dataset = "sentiment_annual"),
    "dataset"
  )
})

test_that("returns empty tibble after download failure", {
  local_mocked_bindings(
    validate_dates = no_dates,
    read_baker_wurgler_file = function(url) stop("HTTP 404")
  )

  expect_message(
    result <- download_data_baker_wurgler(),
    "Returning an empty data set due to download failure."
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("read_baker_wurgler_file reads the DATA sheet of the download", {
  captured <- list()
  local_mocked_bindings(
    req_perform = function(req, path = NULL, ...) {
      captured$url <<- req$url
      captured$path <<- path
      invisible(NULL)
    },
    .package = "httr2"
  )
  local_mocked_bindings(
    read_excel = function(path, sheet, ...) {
      captured$sheet <<- sheet
      expect_identical(path, captured$path)
      mock_sentiment_data()
    },
    .package = "readxl"
  )

  result <- read_baker_wurgler_file("https://example.com/SENTIMENT.xlsx")

  expect_identical(captured$url, "https://example.com/SENTIMENT.xlsx")
  expect_match(captured$path, "\\.xlsx$")
  expect_identical(captured$sheet, "DATA")
  expect_equal(result, mock_sentiment_data())
})
