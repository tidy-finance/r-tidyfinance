test_that("data_options returns object of class tidyfinance_data_options", {
  opts <- data_options()
  expect_s3_class(opts, "tidyfinance_data_options")
})

test_that("data_options returns a list", {
  opts <- data_options()
  expect_type(opts, "list")
})

test_that("data_options has correct default values", {
  opts <- data_options()
  expect_equal(opts$id, "permno")
  expect_equal(opts$date, "date")
  expect_equal(opts$exchange, "exchange")
  expect_equal(opts$mktcap_lag, "mktcap_lag")
  expect_equal(opts$ret_excess, "ret_excess")
  expect_equal(opts$portfolio, "portfolio")
  expect_equal(opts$siccd, "siccd")
  expect_equal(opts$price, "prc_adj")
  expect_equal(opts$listing_age, "listing_age")
  expect_equal(opts$be, "be")
  expect_equal(opts$earnings, "ib")
})

test_that("data_options accepts custom column names for core columns", {
  opts <- data_options(
    id = "firm_id",
    date = "month",
    exchange = "exch",
    mktcap_lag = "lag_mcap",
    ret_excess = "excess_ret",
    portfolio = "port"
  )
  expect_equal(opts$id, "firm_id")
  expect_equal(opts$date, "month")
  expect_equal(opts$exchange, "exch")
  expect_equal(opts$mktcap_lag, "lag_mcap")
  expect_equal(opts$ret_excess, "excess_ret")
  expect_equal(opts$portfolio, "port")
})

test_that(
  "data_options accepts custom column names for filter-related columns", {
  opts <- data_options(
    siccd = "sic_code",
    price = "adj_price",
    listing_age = "age_months",
    be = "book_equity",
    earnings = "ni"
  )
  expect_equal(opts$siccd, "sic_code")
  expect_equal(opts$price, "adj_price")
  expect_equal(opts$listing_age, "age_months")
  expect_equal(opts$be, "book_equity")
  expect_equal(opts$earnings, "ni")
})

test_that("data_options stores extra arguments via ...", {
  opts <- data_options(custom_col = "my_column")
  expect_equal(opts$custom_col, "my_column")
})

test_that("data_options errors for non-character id", {
  expect_error(data_options(id = 123), "id")
  expect_error(data_options(id = c("a", "b")), "id")
})

test_that("data_options errors for non-character date", {
  expect_error(data_options(date = 1), "date")
  expect_error(data_options(date = c("date1", "date2")), "date")
})

test_that("data_options errors for non-character exchange", {
  expect_error(data_options(exchange = TRUE), "exchange")
  expect_error(data_options(exchange = c("NYSE", "NASDAQ")), "exchange")
})

test_that("data_options errors for non-character mktcap_lag", {
  expect_error(data_options(mktcap_lag = 0), "mktcap_lag")
})

test_that("data_options errors for non-character ret_excess", {
  expect_error(data_options(ret_excess = NULL), "ret_excess")
})

test_that("data_options errors for non-character portfolio", {
  expect_error(data_options(portfolio = 5L), "portfolio")
})

test_that("data_options errors for non-character siccd", {
  expect_error(data_options(siccd = 6000), "siccd")
})

test_that("data_options errors for non-character price", {
  expect_error(data_options(price = TRUE), "price")
})

test_that("data_options errors for non-character listing_age", {
  expect_error(data_options(listing_age = 12), "listing_age")
})

test_that("data_options errors for non-character be", {
  expect_error(data_options(be = 1.0), "be")
})

test_that("data_options errors for non-character earnings", {
  expect_error(data_options(earnings = 1.0), "earnings")
  expect_error(data_options(earnings = c("ib", "ni")), "earnings")
})
