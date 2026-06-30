test_that("downloads and processes monthly mispricing factors", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(expr, fallback) expr(),
    read.csv = function(file, ...) {
      data.frame(
        YYYYMM = c(196301, 196302),
        MKTRF = c(0.0493, -0.0238),
        SMB = c(0.0248, 0.0181),
        MGMT = c(0.0213, 0.0110),
        PERF = c(-0.0228, 0.0009),
        RF = c(0.0025, 0.0023)
      )
    }
  )

  result <- download_data_stambaugh_yuan()

  expect_s3_class(result, "tbl_df")
  expect_equal(
    names(result),
    c("date", "mkt_excess", "smb", "mgmt", "perf", "risk_free")
  )
  # Dates are aligned to the beginning of the month.
  expect_equal(result$date, ymd(c("1963-01-01", "1963-02-01")))
  # Returns are already decimal and must not be rescaled.
  expect_equal(result$mkt_excess, c(0.0493, -0.0238))
  expect_equal(result$risk_free, c(0.0025, 0.0023))
})

test_that("parses daily dates from the DATE column", {
  captured_file <- NULL
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(expr, fallback) expr(),
    read.csv = function(file, ...) {
      captured_file <<- file
      data.frame(
        DATE = c(19630102, 19630103),
        MKTRF = c(-0.0054, 0.0166),
        SMB = c(0.0094, 0.0053),
        MGMT = c(0.0062, 0.0050),
        PERF = c(-0.0073, -0.0215),
        RF = c(0.00011, 0.00011)
      )
    }
  )

  result <- download_data_stambaugh_yuan(dataset = "daily")

  expect_match(captured_file, "M4d\\.csv$")
  expect_equal(result$date, ymd(c("1963-01-02", "1963-01-03")))
})

test_that("filters rows when both dates are supplied", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(
        start_date = ymd("1963-02-01"),
        end_date = ymd("1963-02-28")
      )
    },
    handle_download_error = function(expr, fallback) expr(),
    read.csv = function(file, ...) {
      data.frame(
        YYYYMM = c(196301, 196302, 196303),
        MKTRF = c(1, 2, 3),
        SMB = c(1, 2, 3),
        MGMT = c(1, 2, 3),
        PERF = c(1, 2, 3),
        RF = c(1, 2, 3)
      )
    }
  )

  result <- download_data_stambaugh_yuan(
    start_date = "1963-02-01",
    end_date = "1963-02-28"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$date, ymd("1963-02-01"))
})

test_that("warns when the requested range is outside the available data", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(
        start_date = ymd("2020-01-01"),
        end_date = ymd("2020-12-31")
      )
    },
    handle_download_error = function(expr, fallback) expr(),
    read.csv = function(file, ...) {
      data.frame(
        YYYYMM = c(196301, 196302),
        MKTRF = c(1, 2),
        SMB = c(1, 2),
        MGMT = c(1, 2),
        PERF = c(1, 2),
        RF = c(1, 2)
      )
    }
  )

  expect_warning(
    result <- download_data_stambaugh_yuan(
      start_date = "2020-01-01",
      end_date = "2020-12-31"
    ),
    "outside the available"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("aborts on unsupported dataset", {
  expect_error(
    download_data_stambaugh_yuan(dataset = "weekly"),
    "dataset"
  )
})

test_that("returns empty tibble after download failure", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(expr, fallback) fallback
  )

  expect_message(
    result <- download_data_stambaugh_yuan(),
    "Returning an empty data set due to download failure."
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
