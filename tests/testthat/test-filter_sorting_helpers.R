test_that("require_column passes silently when the column is present", {
  d <- data.frame(siccd = 1:3)
  expect_no_error(require_column(d, "siccd", "data_options$siccd"))
  expect_identical(require_column(d, "siccd", "data_options$siccd"), d)
})

test_that("require_column aborts with an informative error when missing", {
  d <- data.frame(x = 1:3)
  expect_error(
    require_column(d, "siccd", "data_options$siccd"),
    "not found"
  )
})

test_that("require_column supports a custom hint", {
  d <- data.frame(x = 1:3)
  expect_error(
    require_column(d, "exchange", "data_options$exchange", info = "NYSE hint"),
    "not found"
  )
})

test_that("filter_with_log keeps only matching rows", {
  d <- data.frame(price = c(0.5, 1, 5, NA))
  out <- filter_with_log(d, !is.na(price) & price >= 1, "min_stock_price", TRUE)
  expect_equal(out$price, c(1, 5))
})

test_that("filter_with_log reports dropped observations unless quiet", {
  d <- data.frame(price = c(0.5, 1, 5))
  expect_message(
    filter_with_log(d, price >= 1, "min_stock_price", FALSE),
    "Filter 'min_stock_price': removed 1 observation"
  )
})

test_that("filter_with_log is silent when quiet is TRUE", {
  d <- data.frame(price = c(0.5, 1, 5))
  expect_no_message(
    filter_with_log(d, price >= 1, "min_stock_price", TRUE)
  )
})

test_that("filter_with_log is silent when nothing is removed", {
  d <- data.frame(price = c(1, 5))
  expect_no_message(
    filter_with_log(d, price >= 1, "min_stock_price", FALSE)
  )
})
