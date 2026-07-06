# A tiny FRED-MD-shaped CSV: header, the Transform: tcode row, then M/D/YYYY levels.
fred_md_csv <- paste(
  "sasdate,LEVELSER,LOGDIFFSER",
  "Transform:,1,5",
  "1/1/2020,100,100",
  "2/1/2020,101,110",
  "3/1/2020,102,121",
  sep = "\n"
)

test_that("apply_fred_md_tcode applies the expected causal transform", {
  x <- c(1, 2, 4, 8)
  expect_equal(apply_fred_md_tcode(x, 1), x)
  expect_equal(apply_fred_md_tcode(x, 2), c(NA, 1, 2, 4))
  expect_true(is.na(apply_fred_md_tcode(x, 5)[1]))
  expect_error(apply_fred_md_tcode(x, 9), "tcode")
})

test_that("vintage = 'latest' returns a wide [date, series...] frame of raw levels", {
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
