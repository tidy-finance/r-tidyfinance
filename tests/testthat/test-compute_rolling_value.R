make_monthly_df <- function(n = 12, start = "2020-01-01", value = seq_len(n)) {
  tibble::tibble(
    date = seq.Date(as.Date(start), by = "month", length.out = n),
    value = value
  )
}

test_that("errors when data has no 'date' column", {
  df <- tibble::tibble(x = 1:5)
  expect_error(
    compute_rolling_value(df, .f = ~ mean(.x$x), period = "month", periods = 3),
    "date"
  )
})

test_that("errors when 'date' column is not Date class", {
  df <- tibble::tibble(date = as.character(Sys.Date() + 0:4), value = 1:5)
  expect_error(
    compute_rolling_value(
      df,
      .f = ~ mean(.x$value),
      period = "month",
      periods = 3
    ),
    "Date"
  )
})

test_that("errors when period is not a single string", {
  df <- make_monthly_df(5)
  expect_error(
    compute_rolling_value(df, .f = ~ mean(.x$value), period = 123, periods = 3)
  )
  expect_error(
    compute_rolling_value(
      df,
      .f = ~ mean(.x$value),
      period = c("month", "year"),
      periods = 3
    )
  )
})

test_that("returns a numeric vector with length equal to nrow(data)", {
  df <- make_monthly_df(12)
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 3
  )
  expect_type(result, "double")
  expect_length(result, nrow(df))
})

test_that("min_obs defaults to periods; early windows return NA", {
  df <- make_monthly_df(6)
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 3
  )
  # First two windows have fewer than 3 rows → NA

  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  # Third window onwards should be non-NA
  expect_false(any(is.na(result[3:6])))
})

test_that("custom min_obs produces fewer leading NAs", {
  df <- make_monthly_df(6)
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 3,
    min_obs = 1
  )
  # Even the first row (1 observation) should be non-NA

  expect_false(any(is.na(result)))
})

test_that("min_obs larger than periods makes more windows NA", {
  df <- make_monthly_df(6)
  # periods = 3 but min_obs = 5 → only windows with ≥5 rows produce values

  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 6,
    min_obs = 5
  )
  expect_true(is.na(result[1]))
  expect_true(is.na(result[4]))
  expect_false(is.na(result[5]))
})

test_that("rows with NA values are dropped before applying .f", {
  df <- make_monthly_df(6)
  df$value[2] <- NA
  df$value[3] <- NA

  # Window for row 4 spans rows 2-4; after dropping NAs only row 4 remains (1 obs)
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 3,
    min_obs = 1
  )
  expect_type(result, "double")
  # Row 2 has NA in value → after complete cases the window for row 2 (rows 1-2)

  # has only row 1 → min_obs = 1 so still non-NA

  expect_false(is.na(result[1]))
})

test_that("window returns NA when complete cases < min_obs due to NAs", {
  df <- make_monthly_df(6)
  df$value[1] <- NA
  df$value[2] <- NA
  df$value[3] <- NA

  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 3,
    min_obs = 2
  )
  # Windows covering rows 1-3 have 0 complete rows → NA

  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
  expect_true(is.na(result[3]))
})

test_that("rolling mean with periods = 1 equals the original values", {
  df <- make_monthly_df(5, value = c(10, 20, 30, 40, 50))
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 1,
    min_obs = 1
  )
  expect_equal(result, c(10, 20, 30, 40, 50))
})

test_that("rolling mean with periods = 3 computes correct values", {
  df <- make_monthly_df(5, value = c(10, 20, 30, 40, 50))
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 3,
    min_obs = 1
  )
  expect_equal(result[1], 10) # window: 10

  expect_equal(result[2], mean(c(10, 20))) # window: 10, 20
  expect_equal(result[3], mean(c(10, 20, 30))) # window: 10, 20, 30
  expect_equal(result[4], mean(c(20, 30, 40))) # window: 20, 30, 40
  expect_equal(result[5], mean(c(30, 40, 50))) # window: 30, 40, 50
})

test_that("rolling sum works correctly", {
  df <- make_monthly_df(5, value = c(1, 2, 3, 4, 5))
  result <- compute_rolling_value(
    df,
    .f = ~ sum(.x$value),
    period = "month",
    periods = 3,
    min_obs = 1
  )
  expect_equal(result[3], 6) # 1 + 2 + 3
  expect_equal(result[4], 9) # 2 + 3 + 4
  expect_equal(result[5], 12) # 3 + 4 + 5
})

test_that("rolling sd works correctly", {
  set.seed(42)
  df <- make_monthly_df(6, value = c(1, 3, 2, 5, 4, 6))
  result <- compute_rolling_value(
    df,
    .f = ~ sd(.x$value),
    period = "month",
    periods = 3,
    min_obs = 2
  )
  expect_equal(result[3], sd(c(1, 3, 2)))
  expect_equal(result[5], sd(c(2, 5, 4)))
})

test_that("accepts purrr-style formula (.f = ~ ...)", {
  df <- make_monthly_df(5, value = c(1, 2, 3, 4, 5))
  result <- compute_rolling_value(
    df,
    .f = ~ sum(.x$value),
    period = "month",
    periods = 2,
    min_obs = 1
  )
  expect_equal(result[4], 7) # 3 + 4
})

test_that("accepts a regular function for .f", {
  my_fn <- function(d) max(d$value)
  df <- make_monthly_df(4, value = c(10, 5, 20, 3))
  result <- compute_rolling_value(
    df,
    .f = my_fn,
    period = "month",
    periods = 3,
    min_obs = 1
  )
  expect_equal(result[3], 20)
  expect_equal(result[4], 20)
})

test_that("works with period = 'quarter'", {
  # 8 months → 3 quarters (Q1: Jan-Mar, Q2: Apr-Jun, Q3: Jul-Aug)
  df <- tibble::tibble(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 8),
    value = 1:8
  )
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "quarter",
    periods = 2,
    min_obs = 1
  )

  expect_length(result, 3)
  expect_type(result, "double")
})

test_that("works with period = 'year'", {
  df <- tibble::tibble(
    date = seq.Date(as.Date("2018-06-01"), by = "quarter", length.out = 12),
    value = 1:12
  )
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "year",
    periods = 2,
    min_obs = 1
  )
  expect_length(result, 4)
  expect_type(result, "double")
})

test_that("works with multiple columns (e.g., regression residuals)", {
  set.seed(99)
  df <- tibble::tibble(
    date = seq.Date(as.Date("2020-01-01"), by = "month", length.out = 24),
    y = rnorm(24),
    x = rnorm(24)
  )
  result <- compute_rolling_value(
    df,
    .f = ~ {
      fit <- lm(y ~ x, data = .x)
      tail(fit$residuals, 1)
    },
    period = "month",
    periods = 12,
    min_obs = 6
  )
  expect_length(result, 24)
  expect_true(all(is.na(result[1:5])))
  expect_false(any(is.na(result[12:24])))
})

test_that("single-row data frame works", {
  df <- make_monthly_df(1, value = 42)
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 1,
    min_obs = 1
  )
  expect_equal(result, 42)
})

test_that("single-row data frame returns NA when min_obs > 1", {
  df <- make_monthly_df(1, value = 42)
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 3,
    min_obs = 2
  )
  expect_true(is.na(result))
})

test_that("all-NA value column returns all NAs", {
  df <- make_monthly_df(5, value = rep(NA_real_, 5))
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 3,
    min_obs = 1
  )
  expect_true(all(is.na(result)))
})

test_that("periods = nrow(data) uses full history for last row", {
  df <- make_monthly_df(5, value = c(2, 4, 6, 8, 10))
  result <- compute_rolling_value(
    df,
    .f = ~ mean(.x$value),
    period = "month",
    periods = 5,
    min_obs = 5
  )
  # Only the last row has a full 5-period window

  expect_true(all(is.na(result[1:4])))
  expect_equal(result[5], mean(c(2, 4, 6, 8, 10)))
})
