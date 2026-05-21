test_that("opens base URL and returns invisible NULL when chapter is NULL", {
  visited <- NULL
  local_mocked_bindings(
    browseURL = function(url, ...) {
      visited <<- url
    },
    .package = "utils"
  )
  result <- withVisible(open_tidy_finance_website())
  expect_null(result$value)
  expect_false(result$visible)
  expect_equal(visited, "https://www.tidy-finance.org/r/")
})

test_that("opens chapter URL when chapter exists in the chapter list", {
  visited <- NULL
  local_mocked_bindings(
    list_tidy_finance_chapters = function() "beta-estimation"
  )
  local_mocked_bindings(
    browseURL = function(url, ...) {
      visited <<- url
    },
    .package = "utils"
  )
  open_tidy_finance_website("beta-estimation")
  expect_equal(
    visited,
    "https://www.tidy-finance.org/r/beta-estimation.html"
  )
})

test_that("falls back to base URL when chapter is not in the chapter list", {
  visited <- NULL
  local_mocked_bindings(
    list_tidy_finance_chapters = function() "beta-estimation"
  )
  local_mocked_bindings(
    browseURL = function(url, ...) {
      visited <<- url
    },
    .package = "utils"
  )
  open_tidy_finance_website("unknown-chapter")
  expect_equal(visited, "https://www.tidy-finance.org/r/")
})
