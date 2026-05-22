test_that("invalid frequency aborts with informative message", {
  expect_error(
    download_data_risk_free(frequency = "weekly"),
    regexp = "monthly.*daily",
    class = "rlang_error"
  )
})

test_that("download failure is caught and re-thrown", {
  local_mocked_bindings(
    validate_dates = function(...) {
      list(
        start_date = NULL,
        end_date = NULL
      )
    }
  )
  local_mocked_bindings(
    read_parquet = function(...) stop("connection refused"),
    .package = "arrow"
  )

  expect_error(
    download_data_risk_free(),
    regexp = "Failed to download risk-free rate data",
    class = "rlang_error"
  )
})

test_that("full dataset returned when no dates are supplied", {
  mock_data <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-02-01")),
    risk_free = c(0.001, 0.002)
  )

  local_mocked_bindings(
    validate_dates = function(...) {
      list(
        start_date = NULL,
        end_date = NULL
      )
    }
  )
  local_mocked_bindings(
    read_parquet = function(...) mock_data,
    .package = "arrow"
  )

  result <- download_data_risk_free()

  expect_equal(result, mock_data)
})

test_that("data is filtered when start and end dates are supplied", {
  mock_data <- tibble::tibble(
    date = as.Date(
      c("2020-01-01", "2020-02-01", "2020-03-01")
    ),
    risk_free = c(0.001, 0.002, 0.003)
  )

  local_mocked_bindings(
    validate_dates = function(...) {
      list(
        start_date = as.Date("2020-01-01"),
        end_date = as.Date("2020-02-01")
      )
    }
  )
  local_mocked_bindings(
    read_parquet = function(...) mock_data,
    .package = "arrow"
  )

  result <- download_data_risk_free(
    "2020-01-01",
    "2020-02-01"
  )

  expect_equal(nrow(result), 2L)
  expect_equal(
    result$date,
    as.Date(c("2020-01-01", "2020-02-01"))
  )
})
