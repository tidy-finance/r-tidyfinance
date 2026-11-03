# Serves `data` as the parquet file of every download and returns a function
# with the requested URLs.
local_osap_file <- function(data, env = parent.frame()) {
  urls <- character()
  testthat::local_mocked_bindings(
    download_parquet_url = function(url, path) {
      urls <<- c(urls, url)
      nanoparquet::write_parquet(data, path)
    },
    .env = env
  )
  function() urls
}

test_that("downloads and processes a Google Drive file", {
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

test_that("downloads the latest release from Hugging Face by default", {
  urls <- local_osap_file(data.frame(
    date = as.Date(c("2020-01-31", "2020-02-29")),
    LongName = c(1, 2)
  ))

  result <- download_data_osap()

  expect_identical(
    urls(),
    paste0(
      "https://huggingface.co/datasets/OpenSourceAP/CrossSection/resolve/",
      "Data-Release-2026.10/Portfolios/Full%20Sets%20OP/",
      "PredictorLSretWide.parquet"
    )
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("date", "long_name"))
  expect_equal(result$date, ymd(c("2020-01-01", "2020-02-01")))
  expect_equal(result$long_name, c(0.01, 0.02))
})

test_that("downloads an earlier release from its branch", {
  urls <- local_osap_file(data.frame(date = as.Date("2020-01-31"), value = 1))

  download_data_osap(release = "2025.10")
  expect_match(urls(), "/resolve/2025.10/Portfolios/Full%20Sets%20OP/")

  download_data_osap(release = "2021.03")
  expect_match(urls()[2], "/resolve/2021.03/Portfolios/Full_Sets_Main/")
})

test_that("finds the latest release on a branch named like the release", {
  urls <- character()
  local_mocked_bindings(
    download_parquet_url = function(url, path) {
      urls <<- c(urls, url)
      if (grepl("Data-Release", url, fixed = TRUE)) {
        rlang::abort("Not found", class = "httr2_http_404")
      }
      nanoparquet::write_parquet(
        data.frame(date = as.Date("2020-01-31"), value = 1),
        path
      )
    }
  )

  result <- download_data_osap()

  expect_length(urls, 2)
  expect_match(urls[2], "/resolve/2026.10/")
  expect_equal(nrow(result), 1)
})

test_that("reports a missing file of an earlier release", {
  local_mocked_bindings(
    download_parquet_url = function(url, path) {
      rlang::abort("Not found", class = "httr2_http_404")
    }
  )

  expect_message(
    result <- download_data_osap(release = "2025.10"),
    "Returning an empty data set due to download failure."
  )
  expect_equal(nrow(result), 0)
})

test_that("rejects unsupported releases", {
  expect_error(download_data_osap(release = "2020.05"), "must be one of")
  expect_error(download_data_osap(release = "latest"), "must be one of")
  expect_error(
    download_data_osap(release = c("2026.10", "2025.10")),
    "must be one of"
  )
})

test_that("rejects release and sheet_id together", {
  expect_error(
    download_data_osap(release = "2025.10", sheet_id = "abc"),
    "either"
  )
  # A forwarded `NULL` is the same as leaving out the release.
  local_mocked_bindings(
    download_osap_google_drive = function(sheet_id) {
      tibble(date = "2020-01-31", value = 1)
    }
  )
  result <- download_data_osap(sheet_id = "abc", release = NULL)
  expect_equal(nrow(result), 1)
})

test_that("keeps sheet_id as the third positional argument", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    download_osap_google_drive = function(sheet_id) {
      tibble(date = "2020-01-31", id = nchar(sheet_id))
    }
  )

  result <- download_data_osap("2020-01-01", "2020-12-31", "abc")

  expect_equal(result$id, 0.03)
})

test_that("filters rows when both dates are supplied", {
  local_osap_file(data.frame(
    date = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31")),
    value = c(1, 2, 3)
  ))
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(
        start_date = ymd("2020-02-01"),
        end_date = ymd("2020-02-28")
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
      result <- download_data_osap(sheet_id = "abc"),
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
      result <- download_data_osap(sheet_id = "abc"),
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

test_that("returns empty tibble when the Hugging Face download fails", {
  local_mocked_bindings(
    download_parquet_url = function(url, path) stop("connection refused")
  )

  expect_message(
    result <- download_data_osap(),
    "Returning an empty data set due to download failure."
  )
  expect_equal(nrow(result), 0)
})

test_that("downloads the signal documentation", {
  urls <- local_osap_file(data.frame(
    Acronym = c("AM", "BM"),
    `Predictability in OP` = c("1_clear", "1_clear"),
    `T-Stat` = c(2.1, 3.4),
    check.names = FALSE
  ))

  result <- download_data_osap(
    start_date = "2020-01-01",
    end_date = "2020-12-31",
    dataset = "signal_doc"
  )

  expect_match(urls(), "/resolve/Data-Release-2026.10/SignalDoc.parquet")
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("acronym", "predictability_in_op", "t_stat"))
  # Acronyms are kept as OSAP names, which `signals` takes.
  expect_equal(result$acronym, c("AM", "BM"))
})

test_that("rejects releases without a signal documentation", {
  expect_error(
    download_data_osap(dataset = "signal_doc", release = "2021.04"),
    "must be one of"
  )
})

test_that("returns empty tibble when the signal documentation fails", {
  local_mocked_bindings(
    download_parquet_url = function(url, path) stop("connection refused")
  )

  expect_message(
    result <- download_data_osap(dataset = "signal_doc"),
    "Returning an empty data set due to download failure."
  )
  expect_equal(nrow(result), 0)
})

test_that("downloads and processes firm-level predictors", {
  urls <- local_osap_file(data.frame(
    permno = c(10000L, 10000L, 10001L),
    yyyymm = c(201912L, 202001L, 202002L),
    BM = c(0.5, 0.6, 0.7),
    Mom12m = c(0.1, 0.2, 0.3),
    Other = c(1, 2, 3)
  ))

  expect_message(
    result <- download_data_osap(
      start_date = "2020-01-01",
      end_date = "2020-12-31",
      dataset = "predictors",
      signals = c("BM", "Mom12m")
    ),
    "about 2 GB"
  )

  expect_match(urls(), "/Firm%20Level%20Characteristics/Predictors.parquet$")
  expect_equal(names(result), c("permno", "date", "bm", "mom12m"))
  expect_equal(result$date, ymd(c("2020-01-01", "2020-02-01")))
  # Signals are not scaled.
  expect_equal(result$bm, c(0.6, 0.7))
})

test_that("downloads all signed predictors by default", {
  urls <- local_osap_file(data.frame(
    permno = 10000L, yyyymm = 202001L, BM = 0.5, Mom12m = 0.1
  ))
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    }
  )

  suppressMessages(
    result <- download_data_osap(dataset = "signed_predictors")
  )

  expect_match(urls(), "signed_predictors_dl_wide.parquet$")
  expect_equal(names(result), c("permno", "date", "bm", "mom12m"))
})

test_that("rejects unknown signals instead of reporting a failed download", {
  local_osap_file(data.frame(
    permno = 10000L, yyyymm = 202001L, BM = 0.5
  ))
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    }
  )

  expect_error(
    suppressMessages(
      download_data_osap(dataset = "predictors", signals = c("BM", "bm"))
    ),
    "unknown names.*bm"
  )
})

test_that("reads the long-short returns of a single factor", {
  local_osap_file(data.frame(
    date = as.Date("2020-01-31"), BM = 1, Mom12m = 2
  ))

  result <- download_data_osap(signals = "BM")

  expect_equal(names(result), c("date", "bm"))
  expect_equal(result$bm, 0.01)
})

test_that("selects signals from a Google Drive file", {
  local_mocked_bindings(
    download_osap_google_drive = function(sheet_id) {
      tibble(date = "2020-01-31", BM = 1, Mom12m = 2)
    }
  )

  result <- download_data_osap(sheet_id = "abc", signals = "BM")
  expect_equal(names(result), c("date", "bm"))

  expect_error(
    download_data_osap(sheet_id = "abc", signals = "Nope"),
    "unknown names.*Nope"
  )
})

test_that("validates dataset, signals, and sheet_id", {
  expect_error(download_data_osap(dataset = "placebos"), "must be one of")
  expect_error(
    download_data_osap(dataset = "signal_doc", sheet_id = "abc"),
    "only supported for"
  )
  expect_error(
    download_data_osap(dataset = "signal_doc", signals = "BM"),
    "not supported for"
  )
  expect_error(
    download_data_osap(dataset = "predictors", signals = character()),
    "non-empty character vector"
  )
})
