test_that("symbols must be a character vector without missing values", {
  expect_error(
    download_data_stock_prices(1)
  )

  expect_error(
    download_data_stock_prices(c("AAPL", NA_character_))
  )
})

test_that("downloads data, replaces NULL, and warns on failures", {
  progress <- 0L
  updates <- 0L

  success_body <- list(
    chart = list(
      result = list(
        list(
          meta = list(exchangeTimezoneName = "UTC"),
          timestamp = c(1577836800, 1577923200),
          indicators = list(
            quote = list(
              list(
                volume = list(100, NULL),
                open = list(10, 11),
                low = list(9, 10),
                high = list(12, 13),
                close = list(11, 12)
              )
            ),
            adjclose = list(
              list(adjclose = list(11, NULL))
            )
          )
        )
      )
    )
  )

  fail_body <- list(
    chart = list(
      error = list(
        code = "Not Found",
        description = "No data found"
      )
    )
  )

  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date, use_default_range) {
      expect_equal(start_date, "2020-01-01")
      expect_equal(end_date, "2020-01-03")
      expect_true(use_default_range)
      list(
        start_date = as.Date("2020-01-01"),
        end_date = as.Date("2020-01-03")
      )
    }
  )

  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_error = function(req, is_error) {
      expect_false(is_error(list()))
      req
    },
    req_perform = function(req) {
      if (grepl("FAIL", req$url)) {
        list(status_code = 404, body = fail_body)
      } else {
        list(status_code = 200, body = success_body)
      }
    },
    resp_body_json = function(response) response$body,
    .package = "httr2"
  )

  testthat::local_mocked_bindings(
    cli_progress_bar = function(..., total, clear) {
      progress <<- total
      expect_true(clear)
    },
    cli_progress_update = function(...) {
      updates <<- updates + 1L
    },
    .package = "cli"
  )

  expect_warning(
    out <- download_data_stock_prices(
      c("AAPL", "FAIL"),
      "2020-01-01",
      "2020-01-03"
    ),
    "Failed to retrieve data for symbol FAIL"
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(progress, 2L)
  expect_equal(updates, 2L)
  expect_equal(out$symbol, c("AAPL", "AAPL"))
  expect_equal(out$date, as.Date(c("2020-01-01", "2020-01-02")))
  expect_equal(out$volume, c(100, NA))
  expect_equal(out$open, c(10, 11))
  expect_equal(out$low, c(9, 10))
  expect_equal(out$high, c(12, 13))
  expect_equal(out$close, c(11, 12))
  expect_equal(out$adjusted_close, c(11, NA))
})

test_that("dates use the exchange time zone reported by Yahoo Finance", {
  # 1584313200 is 2020-03-16 10:00 in Australia/Sydney, i.e. the ASX open on
  # the day of the COVID crash, but 2020-03-15 (a Sunday) in UTC.
  make_body <- function(timezone) {
    list(
      chart = list(
        result = list(
          list(
            meta = list(exchangeTimezoneName = timezone),
            timestamp = 1584313200,
            indicators = list(
              quote = list(
                list(
                  volume = list(100),
                  open = list(10),
                  low = list(9),
                  high = list(12),
                  close = list(11)
                )
              ),
              adjclose = list(
                list(adjclose = list(11))
              )
            )
          )
        )
      )
    )
  }

  download_with_timezone <- function(timezone) {
    testthat::local_mocked_bindings(
      validate_dates = function(start_date, end_date, use_default_range) {
        list(
          start_date = as.Date("2020-03-01"),
          end_date = as.Date("2020-03-31")
        )
      }
    )
    testthat::local_mocked_bindings(
      request = function(url) list(url = url),
      req_error = function(req, is_error) req,
      req_perform = function(req) list(status_code = 200),
      resp_body_json = function(response) make_body(timezone),
      .package = "httr2"
    )
    testthat::local_mocked_bindings(
      cli_progress_bar = function(...) invisible(NULL),
      cli_progress_update = function(...) invisible(NULL),
      .package = "cli"
    )
    download_data_stock_prices("^AXJO", "2020-03-01", "2020-03-31")
  }

  expect_equal(
    download_with_timezone("Australia/Sydney")$date,
    as.Date("2020-03-16")
  )
  expect_equal(
    download_with_timezone("Pacific/Auckland")$date,
    as.Date("2020-03-16")
  )
  # Markets at or behind UTC are unaffected by the fix.
  expect_equal(
    download_with_timezone("America/New_York")$date,
    as.Date("2020-03-15")
  )
  # Missing time zone metadata falls back to UTC, i.e. the previous behavior.
  expect_equal(
    download_with_timezone(NULL)$date,
    as.Date("2020-03-15")
  )
})

test_that("both range bounds are inclusive and the request is buffered", {
  start_date <- as.Date("2020-03-02")
  end_date <- as.Date("2020-03-06")

  # Yahoo is asked for a buffered window, so it may return bars outside the
  # requested range; those must be dropped.
  returned_dates <- seq(
    as.Date("2020-03-01"),
    as.Date("2020-03-07"),
    by = "day"
  )
  timestamps <- as.integer(as.POSIXct(returned_dates, tz = "UTC"))
  n <- length(returned_dates)

  captured <- new.env()

  body <- list(
    chart = list(
      result = list(
        list(
          meta = list(exchangeTimezoneName = "UTC"),
          timestamp = timestamps,
          indicators = list(
            quote = list(
              list(
                volume = as.list(seq_len(n)),
                open = as.list(seq_len(n)),
                low = as.list(seq_len(n)),
                high = as.list(seq_len(n)),
                close = as.list(seq_len(n))
              )
            ),
            adjclose = list(
              list(adjclose = as.list(seq_len(n)))
            )
          )
        )
      )
    )
  )

  testthat::local_mocked_bindings(
    validate_dates = function(start_date, end_date, use_default_range) {
      list(start_date = as.Date("2020-03-02"), end_date = as.Date("2020-03-06"))
    }
  )
  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_error = function(req, is_error) req,
    req_perform = function(req) {
      captured$url <- req$url
      list(status_code = 200)
    },
    resp_body_json = function(response) body,
    .package = "httr2"
  )
  testthat::local_mocked_bindings(
    cli_progress_bar = function(...) invisible(NULL),
    cli_progress_update = function(...) invisible(NULL),
    .package = "cli"
  )

  out <- download_data_stock_prices("AAPL", start_date, end_date)

  # Both bounds are inclusive and nothing outside the range survives.
  expect_equal(out$date, seq(start_date, end_date, by = "day"))

  # The request itself reaches two days beyond each bound.
  expect_match(
    captured$url,
    paste0("period1=", as.integer(as.POSIXct(start_date - 2, tz = "UTC"))),
    fixed = TRUE
  )
  expect_match(
    captured$url,
    paste0("period2=", as.integer(as.POSIXct(end_date + 2, tz = "UTC"))),
    fixed = TRUE
  )
})
