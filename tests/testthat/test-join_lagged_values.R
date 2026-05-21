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
  # T=Jan-2023: window [Feb-2023, Apr-2023]
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

test_that("ff_adjustment removes non-last observations per id-year", {
  orig <- tibble::tibble(
    id = 1L,
    date = as.Date(c("2022-08-01", "2023-03-01"))
  )
  new <- tibble::tibble(
    id = 1L,
    date = as.Date(c("2022-06-01", "2022-12-01")),
    x = c(5, 10)
  )
  # ff: Jun-2022 dropped, Dec-2022 kept (last in 2022).
  # Dec window: [Jan-2023, Mar-2023]
  #   Aug-2022: no match → NA
  #   Mar-2023: match    → 10
  result <- join_lagged_values(
    orig,
    new,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(3),
    ff_adjustment = TRUE
  )
  expect_true(is.na(result$x[1]))
  expect_equal(result$x[2], 10)
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
