data <- tibble::tibble(
  permno = rep(1:2, each = 10),
  date = rep(seq.Date(as.Date("2023-01-01"), by = "month", length.out = 10), 2),
  bm = runif(20, 0.5, 1.5),
  size = runif(20, 100, 200)
)

# --- exact lag tests ---

test_that("add_lagged_columns adds lagged columns", {
  result <- add_lagged_columns(
    data,
    cols = c("bm", "size"),
    lag = months(3),
    by = "permno"
  )
  expect_true("bm_lag" %in% colnames(result))
  expect_true("size_lag" %in% colnames(result))
  expect_equal(nrow(result), nrow(data))
})

test_that("add_lagged_columns works without grouping", {
  result <- add_lagged_columns(
    data |> dplyr::filter(permno == 1),
    cols = c("bm", "size"),
    lag = months(3)
  )
  expect_true("bm_lag" %in% colnames(result))
  expect_true("size_lag" %in% colnames(result))
})

test_that("add_lagged_columns preserves original column values", {
  result <- add_lagged_columns(
    data,
    cols = c("bm", "size"),
    lag = months(3),
    by = "permno"
  )
  expect_equal(result$bm, data$bm)
  expect_equal(result$size, data$size)
})

test_that("add_lagged_columns handles drop_na correctly", {
  data_with_na <- data
  data_with_na$bm[c(1, 11)] <- NA

  result <- add_lagged_columns(
    data_with_na,
    cols = "bm",
    lag = months(3),
    by = "permno",
    drop_na = TRUE
  )

  # Row 4 (date = 2023-04-01) looks back to 2023-01-01 which is NA and dropped
  expect_true(is.na(result$bm_lag[result$permno == 1][4]))
  # Row 5 (date = 2023-05-01) looks back to 2023-02-01 which is not NA
  expect_false(is.na(result$bm_lag[result$permno == 1][5]))
})

test_that("add_lagged_columns works with integer lag", {
  daily_data <- tibble::tibble(
    date = as.Date("2023-01-01") + 0:9,
    value = 1:10
  )
  result <- add_lagged_columns(daily_data, cols = "value", lag = 1)
  expect_equal(result$value_lag, c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9))
})

test_that("add_lagged_columns works with custom data_options", {
  custom_data <- data |> dplyr::rename(my_date = date)
  result <- add_lagged_columns(
    custom_data,
    cols = "bm",
    lag = months(3),
    by = "permno",
    data_options = data_options(date = "my_date")
  )
  expect_true("bm_lag" %in% colnames(result))
})

# --- error handling ---

test_that("add_lagged_columns errors on missing date column", {
  expect_error(
    add_lagged_columns(
      data |> dplyr::select(-date),
      cols = "bm",
      lag = months(3)
    ),
    "date"
  )
})

test_that("add_lagged_columns errors on missing cols", {
  expect_error(
    add_lagged_columns(data, cols = "nonexistent", lag = months(3)),
    "nonexistent"
  )
})

test_that("add_lagged_columns errors on missing by columns", {
  expect_error(
    add_lagged_columns(data, cols = "bm", lag = months(3), by = "nonexistent"),
    "nonexistent"
  )
})

test_that("add_lagged_columns errors on duplicate by-date combinations", {
  dup_data <- dplyr::bind_rows(data, data[1, ])
  expect_error(
    add_lagged_columns(dup_data, cols = "bm", lag = months(3), by = "permno"),
    "unique"
  )
})

test_that("add_lagged_columns errors on invalid lag/max_lag", {
  expect_error(
    add_lagged_columns(data, cols = "bm", lag = -1, by = "permno"),
    "non-negative"
  )
  expect_error(
    add_lagged_columns(data, cols = "bm", lag = months(6), max_lag = months(3), by = "permno"),
    "max_lag"
  )
})

# --- max_lag tests (window / inequality join path) ---

test_that("add_lagged_columns with max_lag adds lagged columns", {
  result <- add_lagged_columns(
    data,
    cols = c("bm", "size"),
    lag = months(3),
    max_lag = months(6),
    by = "permno"
  )
  expect_true("bm_lag" %in% colnames(result))
  expect_true("size_lag" %in% colnames(result))
})

test_that("add_lagged_columns with max_lag preserves row count", {
  result <- add_lagged_columns(
    data,
    cols = "size",
    lag = months(2),
    max_lag = months(5),
    by = "permno"
  )
  expect_equal(nrow(result), nrow(data))
})

test_that("add_lagged_columns with max_lag and drop_na", {
  data_with_na <- data
  data_with_na$bm[c(1, 5, 11, 15)] <- NA

  result <- add_lagged_columns(
    data_with_na,
    cols = "bm",
    lag = months(2),
    max_lag = months(4),
    by = "permno",
    drop_na = TRUE
  )

  expect_equal(nrow(result), nrow(data_with_na))
  expect_true("bm_lag" %in% colnames(result))
})

test_that("add_lagged_columns with max_lag returns NA when no match in window", {
  daily_data <- tibble::tibble(
    date = as.Date("2023-01-01") + 0:9,
    value = 1:10
  )
  result <- add_lagged_columns(daily_data, cols = "value", lag = 10, max_lag = 20)
  expect_true(all(is.na(result$value_lag)))
})

test_that("add_lagged_columns with max_lag works with integer lags", {
  daily_data <- tibble::tibble(
    date = as.Date("2023-01-01") + 0:9,
    value = 1:10
  )
  result <- add_lagged_columns(daily_data, cols = "value", lag = 1, max_lag = 3)
  expected <- c(NA, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  expect_equal(result$value_lag, expected)
})

test_that("add_lagged_columns with max_lag without grouping", {
  result <- add_lagged_columns(
    data |> dplyr::filter(permno == 1),
    cols = "size",
    lag = months(2),
    max_lag = months(4)
  )
  expect_true("size_lag" %in% colnames(result))
  expect_equal(nrow(result), 10)
})

test_that("add_lagged_columns with max_lag and non-NULL by produces correct per-group values", {
  # Regression test: prior to fix, !!!by spliced raw strings into join_by() instead of
  # symbols, causing the group equality constraint to be silently dropped and values to
  # bleed across groups in the inequality-join path (lag < max_lag).
  det_data <- tibble::tibble(
    permno = rep(1:2, each = 4),
    date = rep(
      as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01")),
      2
    ),
    value = c(10, 20, 30, 40, 100, 200, 300, 400)
  )

  result <- add_lagged_columns(
    det_data,
    cols = "value",
    lag = months(1),
    max_lag = months(2),
    by = "permno"
  )

  # For 2023-03-01, the window is [Jan, Feb]; most recent is Feb.
  # For 2023-04-01, the window is [Feb, Mar]; most recent is Mar.
  p1 <- result[result$permno == 1, ]
  p2 <- result[result$permno == 2, ]

  expect_equal(p1$value_lag, c(NA, 10, 20, 30))
  expect_equal(p2$value_lag, c(NA, 100, 200, 300))
})
