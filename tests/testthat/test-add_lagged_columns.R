test_that("exact lag with by returns correct lagged values", {
  data <- tibble::tibble(
    permno = rep(1:2, each = 4),
    date = rep(
      seq.Date(as.Date("2023-01-01"), by = "month", length.out = 4),
      2
    ),
    size = as.numeric(1:8)
  )
  # Also tests that NULL data_options uses the default "date" column
  result <- add_lagged_columns(
    data,
    cols = "size",
    lag = months(1),
    by = "permno"
  )
  g1 <- result[result$permno == 1, ]
  expect_true(is.na(g1$size_lag[1]))
  expect_equal(g1$size_lag[2], 1)
  expect_equal(g1$size_lag[3], 2)
})

test_that("exact lag without by (by = NULL) returns correct values", {
  data <- tibble::tibble(
    date = seq.Date(as.Date("2023-01-01"), by = "month", length.out = 3),
    size = as.numeric(1:3)
  )
  result <- add_lagged_columns(data, cols = "size", lag = months(1))
  expect_true(is.na(result$size_lag[1]))
  expect_equal(result$size_lag[2], 1)
  expect_equal(result$size_lag[3], 2)
})

test_that("window lag handles all .src_date conditions", {
  # Three rows chosen to hit every branch of the inequality-join if_else:
  #   Jan: .upper = Dec 2022, no source exists        → NA (.src_date IS NA)
  #   Feb: .upper = Jan,  .lower = Dec; src = Jan(1)  → 1  (within window)
  #   Jun: .upper = May,  .lower = Apr; closest src =
  #        Feb(2) which is < Apr                      → NA (below lower bound)
  data <- tibble::tibble(
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-06-01")),
    size = as.numeric(1:3)
  )
  result <- add_lagged_columns(
    data,
    cols = "size",
    lag = months(1),
    max_lag = months(2)
  )
  expect_true(is.na(result$size_lag[1])) # no source at all
  expect_equal(result$size_lag[2], 1) # src_date within window
  expect_true(is.na(result$size_lag[3])) # src_date below lower bound
})

test_that("drop_na skips NA source rows in window lag", {
  # Feb and Mar are NA; without drop_na the closest source for Apr is
  # Mar (NA); with drop_na it falls back to Jan (1).
  data <- tibble::tibble(
    date = seq.Date(as.Date("2023-01-01"), by = "month", length.out = 5),
    size = c(1, NA, NA, 4, 5)
  )
  r_keep <- add_lagged_columns(
    data,
    cols = "size",
    lag = months(1),
    max_lag = months(3)
  )
  r_drop <- add_lagged_columns(
    data,
    cols = "size",
    lag = months(1),
    max_lag = months(3),
    drop_na = TRUE
  )
  # Apr row (index 4): window = [Jan, Mar]; closest non-NA = Mar → NA
  expect_true(is.na(r_keep$size_lag[4]))
  # Apr row with drop_na: closest non-NA in window = Jan → 1
  expect_equal(r_drop$size_lag[4], 1)
})

# NOTE: ff_adjustment with a non-NULL `by` crashes in the current source
# due to `interaction(lagged[by], yr)` receiving a data frame instead of
# a vector. That branch cannot be covered until the source is fixed to use
# `lagged[[by]]` (or an equivalent do.call approach for multi-column by).

test_that("ff_adjustment without by uses year grouping only", {
  # Tests the `else yr` branch of the ff_adjustment block.
  data <- tibble::tibble(
    date = as.Date(c("2022-06-01", "2022-12-01", "2023-06-01")),
    size = c(10, 20, 30)
  )
  # ff: 2022 → Dec kept (20); 2023 → Jun kept (30).
  # Shifted +6m: Jun-2023 (20), Dec-2023 (30).
  result <- add_lagged_columns(
    data,
    cols = "size",
    lag = months(6),
    ff_adjustment = TRUE
  )
  jun23 <- result$date == as.Date("2023-06-01")
  expect_equal(result$size_lag[jun23], 20)
})

test_that("non-NULL data_options uses the specified date column name", {
  data <- tibble::tibble(
    my_date = seq.Date(
      as.Date("2023-01-01"),
      by = "month",
      length.out = 3
    ),
    size = as.numeric(1:3)
  )
  opts <- data_options(date = "my_date")
  result <- add_lagged_columns(
    data,
    cols = "size",
    lag = months(1),
    data_options = opts
  )
  expect_true(is.na(result$size_lag[1]))
  expect_equal(result$size_lag[2], 1)
})

test_that("error when date column is absent from data", {
  expect_error(
    add_lagged_columns(
      tibble::tibble(x = 1),
      cols = "x",
      lag = months(1)
    ),
    "date"
  )
})

test_that("error when lag is negative", {
  data <- tibble::tibble(date = as.Date("2023-01-01"), size = 1)
  expect_error(
    add_lagged_columns(data, cols = "size", lag = -1),
    "non-negative"
  )
})

test_that("error when max_lag is less than lag", {
  data <- tibble::tibble(date = as.Date("2023-01-01"), size = 1)
  expect_error(
    add_lagged_columns(
      data,
      cols = "size",
      lag = months(3),
      max_lag = months(1)
    ),
    "max_lag"
  )
})

test_that("error when a requested column is absent from data", {
  data <- tibble::tibble(date = as.Date("2023-01-01"), size = 1)
  expect_error(
    add_lagged_columns(data, cols = "no_such_col", lag = months(1)),
    "missing"
  )
})

test_that("error when a by column is absent from data", {
  data <- tibble::tibble(date = as.Date("2023-01-01"), size = 1)
  expect_error(
    add_lagged_columns(
      data,
      cols = "size",
      lag = months(1),
      by = "no_such_grp"
    ),
    "missing"
  )
})

test_that("error when join key is not unique", {
  data <- tibble::tibble(
    date = rep(as.Date("2023-01-01"), 2),
    size = 1:2
  )
  expect_error(
    add_lagged_columns(data, cols = "size", lag = months(1)),
    "unique"
  )
})
