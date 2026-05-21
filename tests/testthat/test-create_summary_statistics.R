test_data <- tibble(
  A = c(1, 2, 3, 4, NA),
  B = c(NA, 2, 3, 4, 5),
  C = c(1, 1, 2, 2, 2),
  D = c("a", "b", "c", "d", "e")
)

# Basic functionality with numeric columns
test_that("Basic summary statistics are computed correctly", {
  result <- create_summary_statistics(test_data, A, B, C)
  expect_true("n" %in% colnames(result))
  expect_true("mean" %in% colnames(result))
  expect_equal(nrow(result), 3)
})

# Functionality with `detail` = TRUE
test_that("Detailed summary statistics are computed correctly", {
  result <- create_summary_statistics(test_data, A, B, detail = TRUE)
  expect_true("q01" %in% colnames(result))
  expect_true("q99" %in% colnames(result))
})

# Handling of non-numeric columns
test_that("Non-numeric columns are rejected", {
  expect_error(create_summary_statistics(test_data, A, D))
})

# Grouped summaries with a `by` argument
test_that("Summary statistics by group are computed correctly", {
  result <- create_summary_statistics(test_data, A, by = C)
  expect_true("C" %in% colnames(result))
  expect_equal(
    length(unique(result$C)),
    length(unique(test_data$C[!is.na(test_data$C)]))
  )
})

# Test proper handling of NA values
test_that("NA values are handled correctly", {
  data <- data.frame(numeric_col = c(1, 2, NA, 4, 5))

  result_drop_na <- create_summary_statistics(data, numeric_col, drop_na = TRUE)
  result_keep_na <- create_summary_statistics(
    data,
    numeric_col,
    drop_na = FALSE
  )

  expect_identical(result_drop_na$n, result_keep_na$n)
  expect_identical(result_keep_na$mean, NA_real_)
  expect_identical(result_keep_na$sd, NA_real_)
  expect_identical(result_keep_na$min, NA_real_)
  expect_identical(result_keep_na$q50, NA_real_)
  expect_identical(result_keep_na$max, NA_real_)
})

df <- data.frame(
  x = c(1, 2, 3, NA, 5), # has a missing value
  y = c(10, 20, 30, 40, 50),
  g = c("A", "A", "B", "B", "A"),
  chr = letters[1:5]
)

test_that("non-numeric columns in ... raise an error", {
  expect_error(create_summary_statistics(df, x, chr))
})

test_that("detail = FALSE returns only basic statistics columns", {
  result <- create_summary_statistics(df, y) # y has no NAs → quantile_na_handler anyNA = FALSE
  expect_named(result, c("variable", "n", "mean", "sd", "min", "q50", "max"))
  expect_equal(nrow(result), 1L)
  expect_false(is.na(result$q50))
})

test_that("detail = TRUE returns extended quantile columns, NA when data has NAs", {
  result <- create_summary_statistics(df, x, detail = TRUE) # x has NA → anyNA = TRUE
  expect_true(all(c("q01", "q25", "q75", "q99") %in% names(result)))
  expect_true(is.na(result$q50)) # quantile_na_handler anyNA = TRUE path
})

test_that("drop_na = TRUE in the no-by branch removes NA rows before summarising", {
  # NA row dropped before quantile_na_handler is called → anyNA = FALSE → real quantile returned
  result <- create_summary_statistics(df, x, detail = TRUE, drop_na = TRUE)
  expect_false(is.na(result$q50))
})

test_that("by argument groups results and drop_na = TRUE applies in grouped branch", {
  result <- create_summary_statistics(df, x, y, by = g, drop_na = TRUE)
  expect_true("g" %in% names(result))
  expect_equal(nrow(result), 4L) # 2 variables × 2 groups
})
