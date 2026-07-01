test_that("downloads and processes liquidity factors", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(expr, fallback) expr(),
    read.table = function(file, ...) {
      data.frame(
        month = c(196708, 196801),
        agg_liq = c(-0.01, 0.02),
        innov_liq = c(0.03, -0.04),
        traded_liq = c(-99, 0.05)
      )
    }
  )

  result <- download_data_pastor_stambaugh()

  expect_s3_class(result, "tbl_df")
  expect_equal(
    names(result),
    c("date", "agg_liq", "innov_liq", "traded_liq")
  )
  # Dates are aligned to the beginning of the month.
  expect_equal(result$date, ymd(c("1967-08-01", "1968-01-01")))
  # Returns are already decimal and must not be rescaled.
  expect_equal(result$agg_liq, c(-0.01, 0.02))
  # The -99 sentinel for the pre-1968 traded factor becomes NA.
  expect_equal(result$traded_liq, c(NA, 0.05))
})

test_that("filters rows when both dates are supplied", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(
        start_date = ymd("2020-02-01"),
        end_date = ymd("2020-02-28")
      )
    },
    handle_download_error = function(expr, fallback) expr(),
    read.table = function(file, ...) {
      data.frame(
        month = c(202001, 202002, 202003),
        agg_liq = c(1, 2, 3),
        innov_liq = c(1, 2, 3),
        traded_liq = c(1, 2, 3)
      )
    }
  )

  result <- download_data_pastor_stambaugh(
    start_date = "2020-02-01",
    end_date = "2020-02-28"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$date, ymd("2020-02-01"))
  expect_equal(result$innov_liq, 2)
})

test_that("returns empty tibble after download failure", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(expr, fallback) fallback
  )

  expect_message(
    result <- download_data_pastor_stambaugh(),
    "Returning an empty data set due to download failure."
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
