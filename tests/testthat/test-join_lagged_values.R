test_that("normal join adds lagged columns for matching date windows", {
  # Also covers: NULL data_options default, loop over multiple cols.
  orig <- tibble::tibble(
    id = 1L,
    date = as.Date(c("2023-02-01", "2023-04-01", "2023-05-01"))
  )
  new <- tibble::tibble(
    id = 1L,
    date = as.Date("2023-01-01"),
    x = 10,
    y = 20
  )
  # T equal to Jan-2023: window [Feb-2023, Apr-2023]
  # Feb, Apr → match; May → no match
  result <- join_lagged_values(
    orig,
    new,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(3)
  )
  expect_equal(result$x[1], 10)
  expect_equal(result$x[2], 10)
  expect_true(is.na(result$x[3]))
  expect_equal(result$y[1], 20)
})

test_that("ff_adjustment uses the last observation per id-year", {
  orig <- tibble::tibble(
    id = 1L,
    date = as.Date(c("2023-06-01", "2023-07-01", "2024-06-01", "2024-07-01"))
  )
  new <- tibble::tibble(
    id = 1L,
    date = as.Date(c("2022-06-01", "2022-12-01")),
    x = c(5, 10)
  )
  # Jun-2022 is dropped, Dec-2022 (last in 2022) is used Jul-2023 to Jun-2024.
  result <- join_lagged_values(orig, new, id_keys = "id", ff_adjustment = TRUE)
  expect_equal(result$x, c(NA, 10, 10, NA))
})

test_that("ff_adjustment updates in July for every fiscal year-end", {
  orig <- tidyr::expand_grid(
    id = c("dec", "jun"),
    date = seq(as.Date("2021-01-01"), as.Date("2022-12-01"), by = "month")
  )
  new <- tibble::tibble(
    id = c("dec", "jun"),
    date = as.Date(c("2020-12-01", "2020-06-01")),
    x = c(1, 2)
  )
  result <- join_lagged_values(orig, new, id_keys = "id", ff_adjustment = TRUE)

  in_window <- result$date >= as.Date("2021-07-01") &
    result$date <= as.Date("2022-06-01")
  expect_equal(result$x[in_window & result$id == "dec"], rep(1, 12))
  expect_equal(result$x[in_window & result$id == "jun"], rep(2, 12))
  expect_true(all(is.na(result$x[!in_window])))
})

test_that("ff_adjustment works with end-of-month dates", {
  orig <- tibble::tibble(
    id = 1L,
    date = as.Date(c("2021-06-30", "2021-07-31", "2022-06-30", "2022-07-31"))
  )
  new <- tibble::tibble(id = 1L, date = as.Date("2020-09-30"), x = 3)
  result <- join_lagged_values(orig, new, id_keys = "id", ff_adjustment = TRUE)
  expect_equal(result$x, c(NA, 3, 3, NA))
})

test_that("ff_adjustment ignores lags with a message", {
  orig <- tibble::tibble(id = 1L, date = as.Date("2021-07-01"))
  new <- tibble::tibble(id = 1L, date = as.Date("2020-12-01"), x = 1)
  rlang::local_options(rlib_message_verbosity = "verbose")
  expect_message(
    result <- join_lagged_values(
      orig,
      new,
      id_keys = "id",
      min_lag = months(1),
      max_lag = months(2),
      ff_adjustment = TRUE
    ),
    "ignored"
  )
  expect_equal(result$x, 1)
})

test_that("min_lag and max_lag are required without ff_adjustment", {
  orig <- tibble::tibble(id = 1L, date = as.Date("2021-07-01"))
  new <- tibble::tibble(id = 1L, date = as.Date("2020-12-01"), x = 1)
  expect_error(
    join_lagged_values(orig, new, id_keys = "id", min_lag = months(1)),
    "required"
  )
})

test_that("non-NULL data_options uses specified date column", {
  orig <- tibble::tibble(
    id = 1L,
    my_date = as.Date(c("2023-02-01", "2023-05-01"))
  )
  new <- tibble::tibble(
    id = 1L,
    my_date = as.Date("2023-01-01"),
    x = 7
  )
  opts <- data_options(date = "my_date")
  result <- join_lagged_values(
    orig,
    new,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(3),
    data_options = opts
  )
  expect_equal(result$x[1], 7)
  expect_true(is.na(result$x[2]))
})

test_that("error when id_keys is not a character vector", {
  orig <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"))
  new <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"), x = 1)
  expect_error(
    join_lagged_values(
      orig,
      new,
      id_keys = 1L,
      min_lag = months(1),
      max_lag = months(3)
    ),
    "character"
  )
})

test_that("error when date column missing from original_data", {
  orig <- tibble::tibble(id = 1L, other = 1)
  new <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"), x = 1)
  expect_error(
    join_lagged_values(
      orig,
      new,
      id_keys = "id",
      min_lag = months(1),
      max_lag = months(3)
    ),
    "original_data"
  )
})

test_that("error when date column missing from new_data", {
  orig <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"))
  new <- tibble::tibble(id = 1L, other = 1)
  expect_error(
    join_lagged_values(
      orig,
      new,
      id_keys = "id",
      min_lag = months(1),
      max_lag = months(3)
    ),
    "new_data"
  )
})

test_that("error when id_keys column missing from original_data", {
  orig <- tibble::tibble(date = as.Date("2023-01-01"), val = 1)
  new <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"), x = 1)
  expect_error(
    join_lagged_values(
      orig,
      new,
      id_keys = "id",
      min_lag = months(1),
      max_lag = months(3)
    ),
    "original_data"
  )
})

test_that("error when id_keys column missing from new_data", {
  orig <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"))
  new <- tibble::tibble(date = as.Date("2023-01-01"), x = 1)
  expect_error(
    join_lagged_values(
      orig,
      new,
      id_keys = "id",
      min_lag = months(1),
      max_lag = months(3)
    ),
    "new_data"
  )
})

test_that("error when new_data has no columns besides id_keys and date", {
  orig <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"))
  new <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"))
  expect_error(
    join_lagged_values(
      orig,
      new,
      id_keys = "id",
      min_lag = months(1),
      max_lag = months(3)
    ),
    "columns besides"
  )
})

test_that("error when new_data column already exists in original_data", {
  orig <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"), x = 0)
  new <- tibble::tibble(id = 1L, date = as.Date("2023-01-01"), x = 1)
  expect_error(
    join_lagged_values(
      orig,
      new,
      id_keys = "id",
      min_lag = months(1),
      max_lag = months(3)
    ),
    "already exist"
  )
})
