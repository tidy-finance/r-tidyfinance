test_data <- tibble(
  A = c(1, 2, 3, 4, NA),
  B = c(NA, 2, 3, 4, 5),
  C = c(1, 1, 2, 2, 2),
  D = c("a", "b", "c", "d", "e")
)

# Basic functionality with numeric columns
test_that("Basic summary statistics are computed correctly", {
  result <- create_summary_statistics(test_data, A, B)
  expect_true("n" %in% colnames(result))
  expect_true("mean" %in% colnames(result))
  expect_equal(nrow(result), 2) # Two variables summarized
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

# Proper handling of missing (`NA`) values
test_that("Missing values do not skew summary statistics", {
  result <- create_summary_statistics(test_data, A)
  expect_equal(result$n[1], 4) # One missing value in A
})
