test_that("open_tidy_finance_website returns invisible NULL", {
  with_mocked_bindings(
    browseURL = function(url, ...) invisible(NULL),
    .package = "utils",
    {
      result <- open_tidy_finance_website()
      expect_null(result)
    }
  )
})

test_that("open_tidy_finance_website opens main page when chapter is NULL", {
  browsed_url <- NULL
  with_mocked_bindings(
    browseURL = function(url, ...) {
      browsed_url <<- url
      invisible(NULL)
    },
    .package = "utils",
    {
      open_tidy_finance_website(NULL)
    }
  )
  expect_equal(browsed_url, "https://www.tidy-finance.org/r/")
})

test_that("open_tidy_finance_website opens chapter page for a valid chapter", {
  browsed_url <- NULL
  with_mocked_bindings(
    browseURL = function(url, ...) {
      browsed_url <<- url
      invisible(NULL)
    },
    .package = "utils",
    {
      open_tidy_finance_website("beta-estimation")
    }
  )
  expect_equal(
    browsed_url,
    "https://www.tidy-finance.org/r/beta-estimation.html"
  )
})

test_that("open_tidy_finance_website falls back to main page for invalid chapter", {
  # nolint: line_length_linter
  browsed_url <- NULL
  with_mocked_bindings(
    browseURL = function(url, ...) {
      browsed_url <<- url
      invisible(NULL)
    },
    .package = "utils",
    {
      open_tidy_finance_website("nonexistent-chapter-xyz-abc")
    }
  )
  expect_equal(browsed_url, "https://www.tidy-finance.org/r/")
})

test_that("open_tidy_finance_website chapter URL is correctly formed", {
  chapters <- list_tidy_finance_chapters()
  chapter <- chapters[1]
  browsed_url <- NULL
  with_mocked_bindings(
    browseURL = function(url, ...) {
      browsed_url <<- url
      invisible(NULL)
    },
    .package = "utils",
    {
      open_tidy_finance_website(chapter)
    }
  )
  expect_equal(
    browsed_url,
    paste0("https://www.tidy-finance.org/r/", chapter, ".html")
  )
})
