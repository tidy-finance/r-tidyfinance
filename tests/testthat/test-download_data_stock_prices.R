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
