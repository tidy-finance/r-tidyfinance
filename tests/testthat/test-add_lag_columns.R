data <- tibble::tibble(
  permno = rep(1:2, each = 10),
  date = rep(seq.Date(as.Date('2023-01-01'), by = "month", length.out = 10), 2),
  bm = runif(20, 0.5, 1.5),
  size = runif(20, 100, 200)
)

test_that("add_lag_columns adds lagged columns", {
  result <- add_lag_columns(
    data,
    cols = c("bm", "size"),
    lag = months(3),
    by = "permno"
  )
  expect_true("bm_lag" %in% colnames(result))
  expect_true("size_lag" %in% colnames(result))
})

test_that("add_lag_columns returns error for negative lag or max_lag", {
  expect_error(add_lag_columns(data, cols = c("bm", "size"), lag = -1))
  expect_error(add_lag_columns(
    data,
    cols = c("bm", "size"),
    lag = months(3),
    max_lag = months(1)
  ))
})

test_that("add_lag_columns works without grouping", {
  result <- add_lag_columns(data, cols = c("bm", "size"), lag = months(3))
  expect_true("bm_lag" %in% colnames(result))
  expect_true("size_lag" %in% colnames(result))
})

test_that("add_lag_columns preserves original column values", {
  result <- add_lag_columns(
    data,
    cols = c("bm", "size"),
    lag = months(3),
    by = "permno"
  )
  expect_equal(result$bm, data$bm)
  expect_equal(result$size, data$size)
})
