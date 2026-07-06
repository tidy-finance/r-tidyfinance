# A tiny FRED-MD-shaped CSV: header, the Transform: tcode row, then M/D/YYYY
# levels.
fred_md_csv <- paste(
  "sasdate,LEVELSER,LOGDIFFSER",
  "Transform:,1,5",
  "1/1/2020,100,100",
  "2/1/2020,101,110",
  "3/1/2020,102,121",
  sep = "\n"
)

not_fred_md_html <- "<!DOCTYPE html>\n<html><body>not found</body></html>"

test_that("apply_fred_md_tcode applies the expected causal transform", {
  x <- c(1, 2, 4, 8)
  expect_equal(apply_fred_md_tcode(x, 1), x)
  expect_equal(apply_fred_md_tcode(x, 2), c(NA, 1, 2, 4))
  expect_equal(apply_fred_md_tcode(x, 3), c(NA, NA, 1, 2))
  expect_equal(apply_fred_md_tcode(x, 4), log(x))
  expect_true(is.na(apply_fred_md_tcode(x, 5)[1]))
  expect_equal(apply_fred_md_tcode(x, 5)[-1], diff(log(x)))
  expect_true(all(is.na(apply_fred_md_tcode(x, 6)[1:2])))
  expect_equal(apply_fred_md_tcode(x, 6)[3:4], diff(log(x), differences = 2))
  expect_true(is.na(apply_fred_md_tcode(x, 7)[1]))
  expect_error(apply_fred_md_tcode(x, 9), "tcode")
})

test_that("vintage_label extracts YYYY-MM from archived filenames", {
  expect_equal(vintage_label("2020-03-md.csv"), "2020-03")
  expect_equal(vintage_label("fred-md_2020m3.csv"), "2020-03")
  expect_equal(vintage_label("fred-qd_2018m05.csv"), "2018-05")
  expect_true(is.na(vintage_label("readme.txt")))
})

test_that("looks_like_fred_md guards against non-CSV / blank content", {
  expect_false(looks_like_fred_md(NULL))
  expect_false(looks_like_fred_md(""))
  expect_false(looks_like_fred_md("   \n  \n"))
  expect_false(looks_like_fred_md(not_fred_md_html))
  expect_false(looks_like_fred_md("\r\n\r\n<!DOCTYPE html>"))
  expect_true(looks_like_fred_md(fred_md_csv))
})

test_that("fetch_fred_md_text returns the response body on success", {
  local_mocked_bindings(
    request = function(url) url,
    req_timeout = function(req, ...) req,
    req_options = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_string = function(resp) fred_md_csv,
    .package = "httr2"
  )

  expect_equal(
    fetch_fred_md_text("https://example.org/current.csv"),
    fred_md_csv
  )
})

test_that("fetch_fred_md_text aborts on a non-200 response", {
  local_mocked_bindings(
    request = function(url) url,
    req_timeout = function(req, ...) req,
    req_options = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) list(status_code = 404),
    .package = "httr2"
  )

  expect_error(
    fetch_fred_md_text("https://example.org/current.csv"),
    "Failed to download"
  )
})

test_that("fetch_fred_md_bytes returns the response body on success", {
  local_mocked_bindings(
    request = function(url) url,
    req_timeout = function(req, ...) req,
    req_options = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_raw = function(resp) as.raw(c(1, 2, 3)),
    .package = "httr2"
  )

  expect_equal(
    fetch_fred_md_bytes("https://example.org/archive.zip"),
    as.raw(c(1, 2, 3))
  )
})

test_that("fetch_fred_md_bytes aborts on a non-200 response", {
  local_mocked_bindings(
    request = function(url) url,
    req_timeout = function(req, ...) req,
    req_options = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) list(status_code = 500),
    .package = "httr2"
  )

  expect_error(
    fetch_fred_md_bytes("https://example.org/archive.zip"),
    "Failed to download"
  )
})

test_that("vintage = 'latest' returns a wide [date, series...] frame", {
  local_mocked_bindings(
    request = function(url) url,
    req_user_agent = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_string = function(resp) fred_md_csv,
    .package = "httr2"
  )

  result <- download_data_fred_md("FRED-MD")

  expect_s3_class(result, "tbl_df")
  expect_equal(colnames(result), c("date", "LEVELSER", "LOGDIFFSER"))
  expect_false("vintage" %in% colnames(result))
  expect_equal(result$date[1], as.Date("2020-01-01"))
  expect_equal(result$LEVELSER, c(100, 101, 102))
})

test_that("vintage = 'latest' aborts when the current file is not valid CSV", {
  local_mocked_bindings(
    fetch_fred_md_text = function(url) not_fred_md_html
  )

  expect_error(
    download_data_fred_md("FRED-MD"),
    "not a valid CSV"
  )
})

test_that("transform = TRUE applies each series' tcode", {
  local_mocked_bindings(
    request = function(url) url,
    req_user_agent = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_string = function(resp) fred_md_csv,
    .package = "httr2"
  )

  result <- download_data_fred_md("FRED-MD", transform = TRUE)

  expect_equal(result$LEVELSER, c(100, 101, 102))
  expect_true(is.na(result$LOGDIFFSER[1]))
  expect_equal(result$LOGDIFFSER[2], log(110 / 100), tolerance = 1e-4)
})

test_that("a specific vintage hosted individually gets a vintage column", {
  local_mocked_bindings(
    request = function(url) url,
    req_user_agent = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_string = function(resp) fred_md_csv,
    .package = "httr2"
  )

  result <- download_data_fred_md("FRED-MD", vintage = "2026-01")

  expect_equal(colnames(result)[1:2], c("date", "vintage"))
  expect_equal(unique(result$vintage), "2026-01")
})

test_that("a vintage neither archived nor individually hosted errors", {
  local_mocked_bindings(
    fetch_fred_md_text = function(url) not_fred_md_html
  )

  expect_error(
    download_data_fred_md("FRED-MD", vintage = "2026-06"),
    "Could not fetch FRED-MD/QD vintage"
  )
})

test_that("a specific archived vintage is extracted from its archive", {
  local_mocked_bindings(
    fetch_fred_md_bytes = function(url) raw(0)
  )
  local_mocked_bindings(
    unzip = function(zipfile, exdir, ...) {
      lines <- strsplit(fred_md_csv, "\n")[[1]]
      writeLines(lines, file.path(exdir, "2020-03.csv"))
      invisible(NULL)
    },
    .package = "utils"
  )

  result <- download_data_fred_md("FRED-MD", vintage = "2020-03")

  expect_equal(colnames(result)[1:2], c("date", "vintage"))
  expect_equal(unique(result$vintage), "2020-03")
  expect_equal(result$LEVELSER, c(100, 101, 102))
})

test_that("a vintage covered by an archive but missing from it errors", {
  local_mocked_bindings(
    fetch_fred_md_bytes = function(url) raw(0)
  )
  local_mocked_bindings(
    unzip = function(zipfile, exdir, ...) {
      lines <- strsplit(fred_md_csv, "\n")[[1]]
      writeLines(lines, file.path(exdir, "2020-01.csv"))
      invisible(NULL)
    },
    .package = "utils"
  )

  expect_error(
    download_data_fred_md("FRED-MD", vintage = "2020-03"),
    "not found in its FRED-MD/QD archive"
  )
})

test_that("vintage = 'all' stacks archived and individually-hosted vintages", {
  local_mocked_bindings(
    fetch_fred_md_bytes = function(url) raw(0),
    fetch_fred_md_text = function(url) not_fred_md_html
  )
  local_mocked_bindings(
    unzip = function(zipfile, exdir, ...) {
      # A .csv file whose name carries no parseable vintage label is skipped.
      lines <- strsplit(fred_md_csv, "\n")[[1]]
      writeLines("not a vintage file", file.path(exdir, "notes.csv"))
      writeLines(lines, file.path(exdir, "2024-11.csv"))
      writeLines(lines, file.path(exdir, "2024-12.csv"))
      invisible(NULL)
    },
    .package = "utils"
  )

  result <- download_data_fred_md("FRED-MD", vintage = "all")

  expect_equal(colnames(result)[1:2], c("date", "vintage"))
  expect_setequal(unique(result$vintage), c("2024-11", "2024-12"))
})

test_that("vintage = 'all' aborts when no vintage can be downloaded", {
  local_mocked_bindings(
    fetch_fred_md_bytes = function(url) raw(0),
    fetch_fred_md_text = function(url) not_fred_md_html
  )
  local_mocked_bindings(
    unzip = function(zipfile, exdir, ...) invisible(NULL),
    .package = "utils"
  )

  expect_error(
    download_data_fred_md("FRED-MD", vintage = "all"),
    "No FRED-MD/QD vintages could be downloaded"
  )
})

test_that("vintage = 'all' fills in individually-hosted vintages", {
  local_mocked_bindings(
    fetch_fred_md_bytes = function(url) raw(0),
    fetch_fred_md_text = function(url) fred_md_csv
  )
  local_mocked_bindings(
    unzip = function(zipfile, exdir, ...) invisible(NULL),
    .package = "utils"
  )

  result <- download_data_fred_md("FRED-MD", vintage = "all")

  expect_equal(colnames(result)[1:2], c("date", "vintage"))
  expect_true("2024-12" %in% result$vintage)
})

test_that("invalid vintage raises an error", {
  expect_error(
    download_data_fred_md("FRED-MD", vintage = "banana"),
    "vintage"
  )
})

test_that("invalid database raises an error", {
  expect_error(
    download_data_fred_md("not_a_database"),
    "Unsupported database"
  )
})

test_that("FRED-MD and FRED-QD are registered under the FRED domain", {
  datasets <- list_supported_datasets(domain = "FRED")
  expect_true(all(c("FRED-MD", "FRED-QD") %in% datasets$type))
})

test_that("download_data('FRED', 'FRED-MD') routes to the FRED-MD handler", {
  local_mocked_bindings(
    request = function(url) url,
    req_user_agent = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_string = function(resp) fred_md_csv,
    .package = "httr2"
  )

  result <- download_data("FRED", "FRED-MD")

  expect_equal(colnames(result), c("date", "LEVELSER", "LOGDIFFSER"))
})

test_that("download_data('FRED', 'FRED-QD') routes to the FRED-QD handler", {
  local_mocked_bindings(
    request = function(url) url,
    req_user_agent = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_error = function(req, is_error) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_string = function(resp) fred_md_csv,
    .package = "httr2"
  )

  result <- download_data("FRED", "FRED-QD")

  expect_equal(colnames(result), c("date", "LEVELSER", "LOGDIFFSER"))
})
