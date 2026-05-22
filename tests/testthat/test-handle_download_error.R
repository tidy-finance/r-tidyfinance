test_that("returns the result of the download function", {
  download_function <- function(x, y) {
    paste(x, y)
  }

  result <- handle_download_error(download_function, "a", "b")

  expect_equal(result, "a b")
})

test_that("returns NULL fallback on error by default", {
  messages <- character()

  testthat::local_mocked_bindings(
    cli_inform = function(message) {
      messages <<- c(messages, message)
    },
    .package = "cli"
  )

  result <- handle_download_error(function() {
    stop("not found")
  })

  expect_null(result)
  expect_length(messages, 1)
  expect_match(messages, "Failed to download or process the resource")
})

test_that("returns custom fallback on error", {
  messages <- character()
  fallback <- list(status = "failed")

  testthat::local_mocked_bindings(
    cli_inform = function(message) {
      messages <<- c(messages, message)
    },
    .package = "cli"
  )

  result <- handle_download_error(
    function(url) {
      stop("bad url")
    },
    "https://example.com",
    fallback = fallback
  )

  expect_identical(result, fallback)
  expect_length(messages, 1)
  expect_match(messages, "may not be available")
})
