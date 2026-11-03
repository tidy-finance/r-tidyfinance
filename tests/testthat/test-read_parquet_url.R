test_that("read_parquet_url downloads via httr2 and reads with nanoparquet", {
  fixture <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    risk_free = c(0.001, 0.002)
  )
  fixture_path <- tempfile(fileext = ".parquet")
  on.exit(unlink(fixture_path), add = TRUE)
  nanoparquet::write_parquet(fixture, fixture_path)

  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_perform = function(req, ..., path) {
      file.copy(fixture_path, path, overwrite = TRUE)
      invisible(NULL)
    },
    .package = "httr2"
  )

  result <- read_parquet_url("https://example.com/data.parquet")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_named(result, c("date", "risk_free"))
  expect_equal(result$risk_free, c(0.001, 0.002))
})

test_that("read_parquet_url propagates httr2 errors", {
  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_perform = function(req, ...) stop("HTTP 404 Not Found"),
    .package = "httr2"
  )

  expect_error(
    read_parquet_url("https://example.com/missing.parquet"),
    regexp = "404"
  )
})

test_that("read_parquet_url propagates nanoparquet read errors", {
  testthat::local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_perform = function(req, ..., path) {
      writeLines("not a parquet file", path)
      invisible(NULL)
    },
    .package = "httr2"
  )

  expect_error(
    read_parquet_url("https://example.com/bad.parquet")
  )
})

test_that("download_parquet_url retries after the rate limit resets", {
  # Mocked responses skip the retries of httr2, so check the retry policy.
  policies <- NULL
  httr2::local_mocked_responses(function(req) {
    policies <<- req$policies
    httr2::response(status_code = 200)
  })

  download_parquet_url("https://example.com/data.parquet", tempfile())

  expect_equal(policies$retry_max_tries, 3)
  expect_true(policies$retry_is_transient(httr2::response(status_code = 429)))
  expect_false(policies$retry_is_transient(httr2::response(status_code = 503)))
  expect_equal(
    policies$retry_after(httr2::response(
      status_code = 429,
      headers = list(RateLimit = "\"resolvers\";r=0;t=57")
    )),
    58
  )
})
