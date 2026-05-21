test_that("quiet must be a single non-NA logical", {
  d <- data.frame(x = 1)
  expect_error(
    filter_sorting_data(d, quiet = "yes"),
    "quiet"
  )
  expect_error(
    filter_sorting_data(d, quiet = NA),
    "quiet"
  )
  expect_error(
    filter_sorting_data(d, quiet = c(TRUE, FALSE)),
    "quiet"
  )
})

test_that("NULL filter_options and data_options leave data unchanged", {
  d <- data.frame(x = 1:3)
  expect_identical(filter_sorting_data(d), d)
})

test_that("SIC filters abort when siccd column is absent", {
  expect_error(
    filter_sorting_data(
      data.frame(x = 1),
      filter_options = filter_options(
        exclude_financials = TRUE
      )
    ),
    "siccd"
  )
})

test_that("exclude_financials removes SIC 6000-6799, keeps NA, messages", {
  d <- data.frame(
    siccd = c(5999L, 6000L, 6400L, 6799L, 6800L, NA_integer_)
  )
  expect_message(
    out <- filter_sorting_data(
      d,
      filter_options = filter_options(
        exclude_financials = TRUE
      )
    ),
    "exclude_financials"
  )
  # 5999, 6800, NA survive
  expect_equal(nrow(out), 3L)
  expect_true(all(
    is.na(out$siccd) |
      out$siccd < 6000L |
      out$siccd > 6799L
  ))
})

test_that("exclude_utilities removes SIC 4900-4999, keeps NA, messages", {
  d <- data.frame(
    siccd = c(
      4899L,
      4900L,
      4950L,
      4999L,
      5000L,
      NA_integer_
    )
  )
  expect_message(
    out <- filter_sorting_data(
      d,
      filter_options = filter_options(
        exclude_utilities = TRUE
      )
    ),
    "exclude_utilities"
  )
  # 4899, 5000, NA survive
  expect_equal(nrow(out), 3L)
  expect_true(all(
    is.na(out$siccd) |
      out$siccd < 4900L |
      out$siccd > 4999L
  ))
})

test_that("min_stock_price aborts when price column is absent", {
  expect_error(
    filter_sorting_data(
      data.frame(x = 1),
      filter_options = filter_options(min_stock_price = 1)
    ),
    "prc_adj"
  )
})

test_that("min_stock_price removes below-threshold and NA rows, messages", {
  d <- data.frame(prc_adj = c(NA_real_, 0.9, 1.0, 5.0))
  expect_message(
    out <- filter_sorting_data(
      d,
      filter_options = filter_options(min_stock_price = 1)
    ),
    "min_stock_price"
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(out$prc_adj >= 1))
})

test_that("min_size_quantile aborts when mktcap_lag column is absent", {
  expect_error(
    filter_sorting_data(
      data.frame(date = Sys.Date(), exchange = "NYSE"),
      filter_options = filter_options(
        min_size_quantile = 0.5
      )
    ),
    "mktcap_lag"
  )
})

test_that("min_size_quantile aborts when date column is absent", {
  expect_error(
    filter_sorting_data(
      data.frame(mktcap_lag = 1, exchange = "NYSE"),
      filter_options = filter_options(
        min_size_quantile = 0.5
      )
    ),
    "date"
  )
})

test_that("min_size_quantile aborts when exchange column is absent", {
  expect_error(
    filter_sorting_data(
      data.frame(mktcap_lag = 1, date = Sys.Date()),
      filter_options = filter_options(
        min_size_quantile = 0.5
      )
    ),
    "exchange"
  )
})

test_that("min_size_quantile warns on dates with no NYSE observations", {
  # "2020-01-02" is NASDAQ-only → no cutoff computed → warning
  d <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02")),
    exchange = c("NYSE", "NASDAQ"),
    mktcap_lag = c(100, 200)
  )
  expect_warning(
    filter_sorting_data(
      d,
      filter_options = filter_options(
        min_size_quantile = 0.5
      )
    ),
    "min_size_quantile"
  )
})

test_that("min_size_quantile removes below-NYSE-quantile stocks and messages", {
  # NYSE quantile(c(100,200,300), 0.5) = 200; keep mktcap_lag >= 200
  d <- data.frame(
    date = as.Date("2020-01-01"),
    exchange = c("NYSE", "NYSE", "NYSE", "NASDAQ"),
    mktcap_lag = c(100, 200, 300, 50)
  )
  expect_message(
    out <- filter_sorting_data(
      d,
      filter_options = filter_options(
        min_size_quantile = 0.5
      )
    ),
    "min_size_quantile"
  )
  expect_true(all(out$mktcap_lag >= 200))
})

test_that("min_size_quantile emits no message when no rows are removed", {
  # All NYSE mktcap_lag identical -> any quantile == 100;
  # every row passes, n_dropped = 0, no message emitted.
  d <- data.frame(
    date = as.Date("2020-01-01"),
    exchange = c("NYSE", "NYSE"),
    mktcap_lag = c(100, 100)
  )
  expect_no_message(
    filter_sorting_data(
      d,
      filter_options = filter_options(
        min_size_quantile = 0.1
      )
    )
  )
})

test_that("min_listing_age aborts when listing_age column is absent", {
  expect_error(
    filter_sorting_data(
      data.frame(x = 1),
      filter_options = filter_options(min_listing_age = 12)
    ),
    "listing_age"
  )
})

test_that("min_listing_age removes young and NA stocks and messages", {
  d <- data.frame(
    listing_age = c(NA_integer_, 6L, 12L, 24L)
  )
  expect_message(
    out <- filter_sorting_data(
      d,
      filter_options = filter_options(min_listing_age = 12)
    ),
    "min_listing_age"
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(out$listing_age >= 12))
})

test_that("exclude_negative_book_equity aborts when be column is absent", {
  expect_error(
    filter_sorting_data(
      data.frame(x = 1),
      filter_options = filter_options(
        exclude_negative_book_equity = TRUE
      )
    ),
    "be"
  )
})

test_that(
  paste0(
    "exclude_negative_book_equity removes non-positive",
    " be and NA rows and messages"
  ),
  {
    d <- data.frame(be = c(NA_real_, -1, 0, 1, 2))
    expect_message(
      out <- filter_sorting_data(
        d,
        filter_options = filter_options(
          exclude_negative_book_equity = TRUE
        )
      ),
      "exclude_negative_book_equity"
    )
    expect_equal(nrow(out), 2L)
    expect_true(all(out$be > 0))
  }
)

test_that("exclude_negative_earnings aborts when earnings column is absent", {
  expect_error(
    filter_sorting_data(
      data.frame(x = 1),
      filter_options = filter_options(
        exclude_negative_earnings = TRUE
      )
    ),
    "ib"
  )
})

test_that(
  paste0(
    "exclude_negative_earnings removes non-positive",
    " earnings and NA rows and messages"
  ),
  {
    d <- data.frame(ib = c(NA_real_, -1, 0, 1, 2))
    expect_message(
      out <- filter_sorting_data(
        d,
        filter_options = filter_options(
          exclude_negative_earnings = TRUE
        )
      ),
      "exclude_negative_earnings"
    )
    expect_equal(nrow(out), 2L)
    expect_true(all(out$ib > 0))
  }
)

test_that("quiet = TRUE suppresses messages across all filters", {
  # Each filter removes at least one row, but quiet = TRUE means
  # the `!quiet && n_dropped > 0` guard is FALSE for all of them.
  d <- data.frame(
    siccd = c(6100L, 4950L, 2000L),
    prc_adj = c(10, 10, 0.5),
    listing_age = c(5L, 12L, 24L),
    be = c(-1, 1, 2),
    ib = c(-1, 1, 2)
  )
  expect_no_message(
    filter_sorting_data(
      d,
      filter_options = filter_options(
        exclude_financials = TRUE,
        exclude_utilities = TRUE,
        min_stock_price = 1,
        min_listing_age = 12,
        exclude_negative_book_equity = TRUE,
        exclude_negative_earnings = TRUE
      ),
      quiet = TRUE
    )
  )
})
