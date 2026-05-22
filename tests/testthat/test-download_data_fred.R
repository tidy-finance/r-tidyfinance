test_that("downloads, parses, and filters FRED data", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(
        start_date = as.Date(start_date),
        end_date = as.Date(end_date)
      )
    },
    handle_download_error = function(expr, fallback) {
      expr()
    }
  )

  local_mocked_bindings(
    request = function(url) {
      expect_equal(
        url,
        "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GDP"
      )
      url
    },
    req_error = function(req, is_error) req,
    req_perform = function(req) {
      list(status_code = 200)
    },
    resp_body_string = function(resp) {
      paste(
        "observation_date,GDP",
        "2020-01-01,1",
        "2020-02-01,2",
        sep = "\n"
      )
    },
    .package = "httr2"
  )

  local_mocked_bindings(
    cli_progress_bar = function(...) NULL,
    cli_progress_update = function(...) NULL,
    .package = "cli"
  )

  result <- download_data_fred(
    "GDP",
    start_date = "2020-02-01",
    end_date = "2020-02-01"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$date, as.Date("2020-02-01"))
  expect_equal(result$value, 2)
  expect_equal(result$series, "GDP")
})

test_that("returns empty data when FRED responds with non-200 status", {
  warned <- FALSE

  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(expr, fallback) {
      list(status_code = 404)
    }
  )

  local_mocked_bindings(
    cli_progress_bar = function(...) NULL,
    cli_progress_update = function(...) NULL,
    cli_warn = function(...) {
      warned <<- TRUE
    },
    .package = "cli"
  )

  result <- download_data_fred("GDP")

  expect_true(warned)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("date", "value", "series"))
})

test_that("returns empty data when download handling returns NULL", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(expr, fallback) NULL
  )

  local_mocked_bindings(
    cli_progress_bar = function(...) NULL,
    cli_progress_update = function(...) NULL,
    .package = "cli"
  )

  result <- download_data_fred("GDP")

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("date", "value", "series"))
})
