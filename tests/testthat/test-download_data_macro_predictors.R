test_that("dataset is required and must be supported", {
  expect_error(
    download_data_macro_predictors(),
    "dataset"
  )

  expect_error(
    download_data_macro_predictors("daily"),
    "Unsupported macro predictors dataset"
  )
})

macro_raw <- function(date_col, values) {
  tibble::tibble(
    !!date_col := values,
    Index = c(100, 110, 120),
    D12 = c(2, 2, 2),
    E12 = c(5, 5, 5),
    Rfree = c(0.01, 0.01, 0.01),
    svar = c(1, 1, 1),
    `b.m` = c(0.4, 0.5, 0.6),
    ntis = c(0.1, 0.1, 0.1),
    tbl = c(0.02, 0.02, 0.02),
    lty = c(0.05, 0.05, 0.05),
    ltr = c(0.03, 0.03, 0.03),
    BAA = c(0.07, 0.07, 0.07),
    AAA = c(0.04, 0.04, 0.04),
    infl = c(0.02, 0.02, 0.02)
  )
}

test_that("monthly data is downloaded and processed", {
  testthat::local_mocked_bindings(
    read.csv = function(...) {
      macro_raw("yyyymm", c("202001", "202002", "202003"))
    },
    handle_download_error = function(expr, fallback) expr()
  )

  out <- download_data_macro_predictors("monthly")

  expect_equal(nrow(out), 1)
  expect_equal(out$date, as.Date("2020-02-01"))
})

test_that("quarterly data is downloaded and filtered", {
  testthat::local_mocked_bindings(
    read.csv = function(...) {
      macro_raw("yyyyq", c("20201", "20202", "20203"))
    },
    handle_download_error = function(expr, fallback) expr()
  )

  out <- download_data_macro_predictors(
    "quarterly",
    start_date = "2020-04-01",
    end_date = "2020-04-01"
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$date, as.Date("2020-04-01"))
})

test_that("annual data is downloaded and processed", {
  testthat::local_mocked_bindings(
    read.csv = function(...) {
      macro_raw("yyyy", c("2019", "2020", "2021"))
    },
    handle_download_error = function(expr, fallback) expr()
  )

  out <- download_data_macro_predictors("annual")

  expect_equal(nrow(out), 1)
  expect_equal(out$date, as.Date("2020-01-01"))
})

test_that("empty downloads return an empty tibble", {
  testthat::local_mocked_bindings(
    read.csv = function(...) stop("download failed"),
    handle_download_error = function(expr, fallback) fallback
  )

  out <- download_data_macro_predictors("monthly")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})

test_that("explicit deprecated type is still handled", {
  testthat::local_mocked_bindings(
    read.csv = function(...) {
      macro_raw("yyyymm", c("202001", "202002", "202003"))
    },
    handle_download_error = function(expr, fallback) expr()
  )

  expect_warning(
    out <- download_data_macro_predictors(
      type = "macro_predictors_monthly"
    ),
    class = "lifecycle_warning_deprecated"
  )

  expect_equal(out$date, as.Date("2020-02-01"))
})

test_that("legacy dataset names are still handled", {
  testthat::local_mocked_bindings(
    read.csv = function(...) {
      macro_raw("yyyymm", c("202001", "202002", "202003"))
    },
    handle_download_error = function(expr, fallback) expr()
  )

  expect_warning(
    out <- download_data_macro_predictors(
      "macro_predictors_monthly"
    ),
    class = "lifecycle_warning_deprecated"
  )

  expect_equal(out$date, as.Date("2020-02-01"))
})

test_that("correct Google Sheets URL is constructed", {
  url <- NULL

  testthat::local_mocked_bindings(
    read.csv = function(x, ...) {
      url <<- x

      macro_raw("yyyymm", c("202001", "202002", "202003"))
    },
    handle_download_error = function(expr, fallback) expr()
  )

  download_data_macro_predictors(
    dataset = "monthly",
    sheet_id = "test_sheet"
  )

  expect_equal(
    url,
    paste0(
      "https://docs.google.com/spreadsheets/d/",
      "test_sheet",
      "/gviz/tq?tqx=out:csv&sheet=Monthly"
    )
  )
})

test_that("empty download informs and returns empty tibble", {
  testthat::local_mocked_bindings(
    handle_download_error = function(expr, fallback) {
      tibble::tibble()
    }
  )

  out <- download_data_macro_predictors("monthly")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)

  out <- download_data_macro_predictors("quarterly")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)

  out <- download_data_macro_predictors("annual")

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0)
})
