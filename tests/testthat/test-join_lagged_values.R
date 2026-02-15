test_that("basic lag join works with single id and single variable", {
  df1 <- tibble(
    id = rep(1, 4),
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01"))
  )

  df2 <- tibble(
    id = rep(1, 4),
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")),
    x = c(10, 20, 30, 40)
  )

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(3)
  )

  expect_true("x" %in% names(result))
  expect_equal(nrow(result), 4)
  # Row with date 2020-01-01: looking for new_data where lower >= date and date < upper
  # i.e. new_data rows where .lower <= 2020-01-01 < .upper
  # new_data row date=2020-01-01 -> .lower=2020-02-01, .upper=2020-05-01 -> 2020-01-01 < .lower, no
  # So first row should be NA (nothing lagged into it yet)
  expect_true(is.na(result$x[1]))
})

test_that("multiple id keys are supported", {
  df1 <- tibble(
    id1 = c(1, 1, 2, 2),
    id2 = c("a", "a", "b", "b"),
    date = as.Date(c("2020-01-01", "2020-03-01", "2020-01-01", "2020-03-01"))
  )

  df2 <- tibble(
    id1 = c(1, 1, 2, 2),
    id2 = c("a", "a", "b", "b"),
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01")),
    val = c(100, 200, 300, 400)
  )

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = c("id1", "id2"),
    min_lag = months(1),
    max_lag = months(3)
  )

  expect_true("val" %in% names(result))
  expect_equal(nrow(result), 4)
  # IDs should not bleed across groups
  # For id1=2, id2="b", date=2020-03-01: should match new_data with id1=2, id2="b"
  row_2b_march <- result |>
    filter(id1 == 2, id2 == "b", date == as.Date("2020-03-01"))
  row_1a_march <- result |>
    filter(id1 == 1, id2 == "a", date == as.Date("2020-03-01"))
  # These should come from different source rows
  expect_false(
    identical(row_2b_march$val, row_1a_march$val) && !is.na(row_2b_march$val)
  )
})

test_that("multiple new columns are joined", {
  df1 <- tibble(
    id = rep(1, 3),
    date = as.Date(c("2020-02-01", "2020-03-01", "2020-04-01"))
  )

  df2 <- tibble(
    id = rep(1, 3),
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    x = c(1, 2, 3),
    y = c(10, 20, 30)
  )

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(3)
  )

  expect_true(all(c("x", "y") %in% names(result)))
  expect_equal(ncol(result), 4) # id, date, x, y
})

test_that("no match produces NAs", {
  df1 <- tibble(
    id = 1,
    date = as.Date("2025-01-01")
  )

  df2 <- tibble(
    id = 1,
    date = as.Date("2020-01-01"),
    x = 999
  )

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(3)
  )

  expect_true(is.na(result$x[1]))
})

test_that("different ids do not cross-contaminate", {
  df1 <- tibble(
    id = c(1, 2),
    date = as.Date(c("2020-03-01", "2020-03-01"))
  )

  df2 <- tibble(
    id = c(1, 2),
    date = as.Date(c("2020-01-01", "2020-01-01")),
    x = c(100, 200)
  )

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(6)
  )

  expect_equal(result$x[result$id == 1], 100)
  expect_equal(result$x[result$id == 2], 200)
})

test_that("ff_adjustment picks latest date per year", {
  df1 <- tibble(
    id = rep(1, 2),
    date = as.Date(c("2021-06-01", "2021-09-01"))
  )

  # Two rows in 2020 for same id; ff_adjustment should pick the later one
  df2 <- tibble(
    id = rep(1, 3),
    date = as.Date(c("2020-06-01", "2020-12-01", "2021-03-01")),
    x = c(10, 20, 30)
  )

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = "id",
    min_lag = months(6),
    max_lag = months(18),
    ff_adjustment = TRUE
  )

  expect_true("x" %in% names(result))
  # With ff_adjustment, only the max-date row per (id, year) is kept
  # Year 2020: keeps date=2020-12-01 (x=20), .lower=2021-06-01, .upper=2022-12-01
  # Year 2021: keeps date=2021-03-01 (x=30), .lower=2021-09-01, .upper=2023-03-01
  # For original date=2021-06-01: closest .lower <= 2021-06-01 -> 2021-06-01 == .lower of 2020 row, x=20
  expect_equal(result$x[1], 20)
})

test_that("input validation: id_keys must be character", {
  df <- tibble(id = 1, date = as.Date("2020-01-01"))
  expect_error(
    join_lagged_values(
      df,
      df,
      id_keys = 1,
      min_lag = months(1),
      max_lag = months(3)
    )
  )
})

test_that("input validation: id_date must exist in both datasets", {
  df1 <- tibble(id = 1, date = as.Date("2020-01-01"))
  df2 <- tibble(id = 1, dt = as.Date("2020-01-01"), x = 1)

  expect_error(
    join_lagged_values(
      df1,
      df2,
      id_keys = "id",
      id_date = "dt",
      min_lag = months(1),
      max_lag = months(3)
    )
  )
})

test_that("custom id_date column name works", {
  df1 <- tibble(id = 1, dt = as.Date(c("2020-03-01", "2020-04-01")))
  df2 <- tibble(
    id = 1,
    dt = as.Date(c("2020-01-01", "2020-02-01")),
    x = c(5, 10)
  )

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = "id",
    id_date = "dt",
    min_lag = months(1),
    max_lag = months(4)
  )

  expect_true("x" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("original_data columns are preserved", {
  df1 <- tibble(
    id = rep(1, 3),
    date = as.Date(c("2020-02-01", "2020-03-01", "2020-04-01")),
    existing_col = c("a", "b", "c")
  )

  df2 <- tibble(
    id = rep(1, 3),
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    x = c(1, 2, 3)
  )

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(3)
  )

  expect_true("existing_col" %in% names(result))
  expect_equal(result$existing_col, c("a", "b", "c"))
})

test_that("empty original_data returns empty result with correct columns", {
  df1 <- tibble(id = integer(), date = as.Date(character()))
  df2 <- tibble(id = 1, date = as.Date("2020-01-01"), x = 1)

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(3)
  )

  expect_equal(nrow(result), 0)
  expect_true("x" %in% names(result))
})

test_that("closest match is used within the lag window", {
  # With multiple new_data dates falling in the window, closest() should pick

  df1 <- tibble(
    id = 1,
    date = as.Date("2020-06-01")
  )

  df2 <- tibble(
    id = rep(1, 4),
    date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")),
    x = c(10, 20, 30, 40)
  )

  result <- join_lagged_values(
    original_data = df1,
    new_data = df2,
    id_keys = "id",
    min_lag = months(1),
    max_lag = months(6)
  )

  # .lower for each new_data row: +1 month from date
  # 2020-01-01 -> .lower=2020-02-01, .upper=2020-08-01
  # 2020-02-01 -> .lower=2020-03-01, .upper=2020-09-01
  # 2020-03-01 -> .lower=2020-04-01, .upper=2020-10-01
  # 2020-04-01 -> .lower=2020-05-01, .upper=2020-11-01
  # original date=2020-06-01: closest(.date >= .lower) picks the largest .lower <= 2020-06-01
  # That's .lower=2020-05-01 from date=2020-04-01, so x=40
  expect_equal(result$x[1], 40)
})
