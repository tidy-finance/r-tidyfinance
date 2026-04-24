make_sorting_data <- function(seed = 42) {
  set.seed(seed)
  data.frame(
    permno = 1:30,
    date = rep(as.Date("2020-01-01"), 30),
    siccd = c(
      rep(6200L, 5), # financial (SIC 6000–6799)
      rep(4950L, 5), # utility  (SIC 4900–4999)
      rep(5000L, 5), # retail   (neutral)
      rep(7000L, 5), # services (neutral)
      rep(1000L, 10) # other    (neutral)
    ),
    prc_adj = c(rep(0.5, 10), rep(5, 10), rep(20, 10)),
    mktcap_lag = c(
      seq(100, 500, length.out = 15),
      seq(5000, 10000, length.out = 15)
    ),
    listing_age = c(rep(6L, 10), rep(24L, 20)),
    be = c(rep(-1, 5), rep(0.5, 5), rep(1, 20))
  )
}

test_that("filter_sorting_data returns all rows when no filters are active", {
  data <- make_sorting_data()
  result <- filter_sorting_data(data, quiet = TRUE)
  expect_equal(nrow(result), nrow(data))
})

test_that("filter_sorting_data preserves all columns", {
  data <- make_sorting_data()
  result <- filter_sorting_data(data, quiet = TRUE)
  expect_equal(colnames(result), colnames(data))
})

test_that("filter_sorting_data removes financial firms when exclude_financials = TRUE", {
  data <- make_sorting_data()
  n_financials <- sum(data$siccd >= 6000 & data$siccd <= 6799, na.rm = TRUE)
  result <- filter_sorting_data(
    data,
    filter_options = filter_options(exclude_financials = TRUE),
    quiet = TRUE
  )
  expect_equal(nrow(result), nrow(data) - n_financials)
  expect_true(all(
    is.na(result$siccd) | result$siccd < 6000 | result$siccd > 6799
  ))
})

test_that("filter_sorting_data removes utility firms when exclude_utilities = TRUE", {
  data <- make_sorting_data()
  n_utilities <- sum(data$siccd >= 4900 & data$siccd <= 4999, na.rm = TRUE)
  result <- filter_sorting_data(
    data,
    filter_options = filter_options(exclude_utilities = TRUE),
    quiet = TRUE
  )
  expect_equal(nrow(result), nrow(data) - n_utilities)
  expect_true(all(
    is.na(result$siccd) | result$siccd < 4900 | result$siccd > 4999
  ))
})

test_that("filter_sorting_data removes both financials and utilities when both are excluded", {
  data <- make_sorting_data()
  n_fin_or_util <- sum(
    (data$siccd >= 6000 & data$siccd <= 6799) |
      (data$siccd >= 4900 & data$siccd <= 4999),
    na.rm = TRUE
  )
  result <- filter_sorting_data(
    data,
    filter_options = filter_options(
      exclude_financials = TRUE,
      exclude_utilities = TRUE
    ),
    quiet = TRUE
  )
  expect_equal(nrow(result), nrow(data) - n_fin_or_util)
})

test_that("filter_sorting_data errors when siccd column is missing for financial exclusion", {
  data <- make_sorting_data()
  data$siccd <- NULL
  expect_error(
    filter_sorting_data(
      data,
      filter_options = filter_options(exclude_financials = TRUE)
    ),
    "siccd"
  )
})

test_that("filter_sorting_data applies min_stock_price filter correctly", {
  data <- make_sorting_data()
  threshold <- 3
  n_below <- sum(!is.na(data$prc_adj) & data$prc_adj < threshold)
  result <- filter_sorting_data(
    data,
    filter_options = filter_options(min_stock_price = threshold),
    quiet = TRUE
  )
  expect_equal(nrow(result), nrow(data) - n_below)
  expect_true(all(result$prc_adj >= threshold, na.rm = TRUE))
})

test_that("filter_sorting_data errors when price column is missing for min_stock_price", {
  data <- make_sorting_data()
  data$prc_adj <- NULL
  expect_error(
    filter_sorting_data(
      data,
      filter_options = filter_options(min_stock_price = 5)
    ),
    "prc_adj"
  )
})

test_that("filter_sorting_data applies min_size_quantile filter correctly", {
  data <- make_sorting_data()
  result <- filter_sorting_data(
    data,
    filter_options = filter_options(min_size_quantile = 0.5),
    quiet = TRUE
  )
  # At least 50% of rows should be removed
  expect_true(nrow(result) <= nrow(data) * 0.55)
})

test_that("filter_sorting_data errors when mktcap_lag column is missing for min_size_quantile", {
  data <- make_sorting_data()
  data$mktcap_lag <- NULL
  expect_error(
    filter_sorting_data(
      data,
      filter_options = filter_options(min_size_quantile = 0.2)
    ),
    "mktcap_lag"
  )
})

test_that("filter_sorting_data applies min_listing_age filter correctly", {
  data <- make_sorting_data()
  min_age <- 12
  n_young <- sum(!is.na(data$listing_age) & data$listing_age < min_age)
  result <- filter_sorting_data(
    data,
    filter_options = filter_options(min_listing_age = min_age),
    quiet = TRUE
  )
  expect_equal(nrow(result), nrow(data) - n_young)
  expect_true(all(result$listing_age >= min_age, na.rm = TRUE))
})

test_that("filter_sorting_data errors when listing_age column is missing", {
  data <- make_sorting_data()
  data$listing_age <- NULL
  expect_error(
    filter_sorting_data(
      data,
      filter_options = filter_options(min_listing_age = 12)
    ),
    "listing_age"
  )
})

test_that("filter_sorting_data applies positive_book_equity filter correctly", {
  data <- make_sorting_data()
  n_nonpositive <- sum(!is.na(data$be) & data$be <= 0)
  result <- filter_sorting_data(
    data,
    filter_options = filter_options(positive_book_equity = TRUE),
    quiet = TRUE
  )
  expect_equal(nrow(result), nrow(data) - n_nonpositive)
  expect_true(all(result$be > 0, na.rm = TRUE))
})

test_that("filter_sorting_data errors when be column is missing for positive_book_equity", {
  data <- make_sorting_data()
  data$be <- NULL
  expect_error(
    filter_sorting_data(
      data,
      filter_options = filter_options(positive_book_equity = TRUE)
    ),
    "be"
  )
})

test_that("filter_sorting_data emits a message when quiet = FALSE and rows are removed", {
  data <- make_sorting_data()
  expect_message(
    filter_sorting_data(
      data,
      filter_options = filter_options(exclude_financials = TRUE),
      quiet = FALSE
    )
  )
})

test_that("filter_sorting_data suppresses messages when quiet = TRUE", {
  data <- make_sorting_data()
  expect_silent(
    filter_sorting_data(
      data,
      filter_options = filter_options(exclude_financials = TRUE),
      quiet = TRUE
    )
  )
})

test_that("filter_sorting_data errors for invalid quiet argument", {
  data <- make_sorting_data()
  expect_error(filter_sorting_data(data, quiet = "yes"), "quiet")
  expect_error(filter_sorting_data(data, quiet = NA), "quiet")
  expect_error(filter_sorting_data(data, quiet = 1), "quiet")
})

test_that("filter_sorting_data respects custom column names via data_options", {
  data <- make_sorting_data()
  names(data)[names(data) == "prc_adj"] <- "stock_price"
  result <- filter_sorting_data(
    data,
    filter_options = filter_options(min_stock_price = 3),
    data_options = data_options(price = "stock_price"),
    quiet = TRUE
  )
  expect_true(all(result$stock_price >= 3, na.rm = TRUE))
})

test_that("filter_sorting_data with NA siccd values preserves those rows", {
  data <- make_sorting_data()
  data$siccd[1:3] <- NA_integer_
  result <- filter_sorting_data(
    data,
    filter_options = filter_options(exclude_financials = TRUE),
    quiet = TRUE
  )
  # NA rows should be kept (not removed)
  expect_true(any(is.na(result$siccd)))
})
