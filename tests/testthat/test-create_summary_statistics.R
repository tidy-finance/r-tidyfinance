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
  expect_equal(length(unique(result$C)), length(unique(test_data$C[!is.na(test_data$C)])))
})

# Test proper handling of NA values
test_that("NA values are handled correctly", {
  data <- data.frame(numeric_col = c(1, 2, NA, 4, 5))

  result_drop_na <- create_summary_statistics(data, numeric_col, drop_na = TRUE)
  result_keep_na <- create_summary_statistics(data, numeric_col, drop_na = FALSE)

  expect_identical(result_drop_na$n, result_keep_na$n)
  expect_identical(result_keep_na$mean, NA_real_)
  expect_identical(result_keep_na$sd, NA_real_)
  expect_identical(result_keep_na$min, NA_real_)
  expect_identical(result_keep_na$q50, NA_real_)
  expect_identical(result_keep_na$max, NA_real_)
})
