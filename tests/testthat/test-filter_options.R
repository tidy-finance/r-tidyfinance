test_that("filter_options returns object of class tidyfinance_filter_options", {
  opts <- filter_options()
  expect_s3_class(opts, "tidyfinance_filter_options")
})

test_that("filter_options returns a list", {
  opts <- filter_options()
  expect_type(opts, "list")
})

test_that("filter_options has correct defaults", {
  opts <- filter_options()
  expect_false(opts$exclude_financials)
  expect_false(opts$exclude_utilities)
  expect_null(opts$min_stock_price)
  expect_null(opts$min_size_quantile)
  expect_null(opts$min_listing_age)
  expect_false(opts$exclude_negative_book_equity)
  expect_false(opts$exclude_negative_earnings)
})

test_that("filter_options accepts non-default logical flags", {
  opts <- filter_options(
    exclude_financials = TRUE,
    exclude_utilities = TRUE,
    exclude_negative_book_equity = TRUE,
    exclude_negative_earnings = TRUE
  )
  expect_true(opts$exclude_financials)
  expect_true(opts$exclude_utilities)
  expect_true(opts$exclude_negative_book_equity)
  expect_true(opts$exclude_negative_earnings)
})

test_that("filter_options accepts valid numeric thresholds", {
  opts <- filter_options(
    min_stock_price = 5,
    min_size_quantile = 0.2,
    min_listing_age = 12
  )
  expect_equal(opts$min_stock_price, 5)
  expect_equal(opts$min_size_quantile, 0.2)
  expect_equal(opts$min_listing_age, 12)
})

test_that("filter_options stores extra arguments via ...", {
  opts <- filter_options(custom_filter = "my_value")
  expect_equal(opts$custom_filter, "my_value")
})

test_that("filter_options errors for non-logical exclude_financials", {
  expect_error(filter_options(exclude_financials = "yes"), "exclude_financials")
  expect_error(filter_options(exclude_financials = 1), "exclude_financials")
  expect_error(filter_options(exclude_financials = NA), "exclude_financials")
})

test_that("filter_options errors for non-logical exclude_utilities", {
  expect_error(filter_options(exclude_utilities = "yes"), "exclude_utilities")
  expect_error(filter_options(exclude_utilities = NA), "exclude_utilities")
})

test_that("filter_options errors for non-positive min_stock_price", {
  expect_error(filter_options(min_stock_price = 0), "min_stock_price")
  expect_error(filter_options(min_stock_price = -1), "min_stock_price")
  expect_error(filter_options(min_stock_price = NA_real_), "min_stock_price")
  expect_error(filter_options(min_stock_price = "5"), "min_stock_price")
  expect_error(filter_options(min_stock_price = c(1, 2)), "min_stock_price")
})

test_that("filter_options errors for out-of-range min_size_quantile", {
  expect_error(filter_options(min_size_quantile = 0), "min_size_quantile")
  expect_error(filter_options(min_size_quantile = 1), "min_size_quantile")
  expect_error(filter_options(min_size_quantile = -0.1), "min_size_quantile")
  expect_error(filter_options(min_size_quantile = 1.1), "min_size_quantile")
  expect_error(
    filter_options(min_size_quantile = NA_real_),
    "min_size_quantile"
  )
})

test_that("filter_options errors for invalid min_listing_age", {
  expect_error(filter_options(min_listing_age = -1), "min_listing_age")
  expect_error(filter_options(min_listing_age = NA_real_), "min_listing_age")
  expect_error(filter_options(min_listing_age = "12"), "min_listing_age")
  expect_error(filter_options(min_listing_age = c(6, 12)), "min_listing_age")
})

test_that("filter_options accepts zero as valid min_listing_age", {
  expect_no_error(filter_options(min_listing_age = 0))
})

test_that(
  paste0(
    "filter_options errors for non-logical ",
    "exclude_negative_book_equity"
  ),
  {
    expect_error(
      filter_options(exclude_negative_book_equity = "TRUE"),
      "exclude_negative_book_equity"
    )
    expect_error(
      filter_options(exclude_negative_book_equity = NA),
      "exclude_negative_book_equity"
    )
    expect_error(
      filter_options(exclude_negative_book_equity = 1),
      "exclude_negative_book_equity"
    )
  }
)

test_that("filter_options errors for non-logical exclude_negative_earnings", {
  expect_error(
    filter_options(exclude_negative_earnings = "TRUE"),
    "exclude_negative_earnings"
  )
  expect_error(
    filter_options(exclude_negative_earnings = NA),
    "exclude_negative_earnings"
  )
  expect_error(
    filter_options(exclude_negative_earnings = 1),
    "exclude_negative_earnings"
  )
})
