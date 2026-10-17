test_that("downloads and processes all rows", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "text/csv"),
      body = charToRaw("date\n")
    )
  })
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(expr, fallback) expr(),
    read.csv = function(file, ...) {
      data.frame(
        date = c("2020-01-31", "2020-02-29"),
        LongName = c(1, 2),
        check.names = FALSE
      )
    }
  )

  result <- download_data_osap(sheet_id = "abc")

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("date", "long_name"))
  # Dates are aligned to the beginning of the month.
  expect_equal(result$date, ymd(c("2020-01-01", "2020-02-01")))
  # Percentage returns are scaled to numeric (decimal) values.
  expect_equal(result$long_name, c(0.01, 0.02))
})

test_that("filters rows when both dates are supplied", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "text/csv"),
      body = charToRaw("date\n")
    )
  })
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(
        start_date = ymd("2020-02-01"),
        end_date = ymd("2020-02-28")
      )
    },
    handle_download_error = function(expr, fallback) expr(),
    read.csv = function(file, ...) {
      data.frame(
        date = c("2020-01-31", "2020-02-29", "2020-03-31"),
        value = c(1, 2, 3)
      )
    }
  )

  result <- download_data_osap(
    start_date = "2020-02-01",
    end_date = "2020-02-28"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$date, ymd("2020-02-01"))
  expect_equal(result$value, 0.02)
})

test_that("returns empty tibble after download failure", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(expr, fallback) fallback
  )

  expect_message(
    result <- download_data_osap(),
    "Returning an empty data set due to download failure."
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("reports an exceeded Google Drive quota", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "text/html; charset=utf-8"),
      body = charToRaw(
        "<html><head><title>Google Drive - Quota exceeded</title></head></html>"
      )
    )
  })

  expect_message(
    expect_message(
      result <- download_data_osap(),
      "download quota of the file is exceeded"
    ),
    "Returning an empty data set due to download failure."
  )
  expect_equal(result, tibble())
})

test_that("rejects other HTML pages instead of parsing them", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list("Content-Type" = "text/html"),
      body = charToRaw("<html><body>Sign in</body></html>")
    )
  })

  expect_message(
    expect_message(
      result <- download_data_osap(),
      "HTML page instead of a CSV"
    ),
    "Returning an empty data set due to download failure."
  )
  expect_equal(ncol(result), 0)
})

test_that("converts names to snake case", {
  result <- to_snake_case(c(
    "LongName",
    "two words",
    "__Already___Odd__"
  ))

  expect_equal(result, c("long_name", "two_words", "already_odd"))
})
