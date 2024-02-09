# Test winsorize() function -------------------------------------------
# Test that winsorize function works correctly
test_that("winsorize() correctly adjusts values", {
  set.seed(123)
  x <- rnorm(100)
  cut <- 0.05

  # Winsorize the data
  winsorized_x <- winsorize(x, cut)

  # Check if the lower and upper bounds are correctly applied
  expect_equal(min(winsorized_x), unname(quantile(x, cut, na.rm = TRUE)))
  expect_equal(max(winsorized_x), unname(quantile(x, 1 - cut, na.rm = TRUE)))

  # Check if values outside the quantiles are replaced
  expect_true(all(winsorized_x >= unname(quantile(x, cut, na.rm = TRUE))))
  expect_true(all(winsorized_x <= unname(quantile(x, 1 - cut, na.rm = TRUE))))
})

# Test handling of NA values
test_that("winsorize() handles NA values", {
  x <- c(NA, 1, 2, 3, 4, 5, NA)
  cut <- 0.1

  # Winsorize the data, expecting NA values to be ignored
  winsorized_x <- winsorize(x, cut)

  # Check that the length of the output matches the input
  expect_equal(length(winsorized_x), length(x))

  # Check that NA values are still present and other values are adjusted correctly
  expect_true(all(is.na(winsorized_x) == is.na(x)))
  expect_true(all(winsorized_x[!is.na(winsorized_x)] >= unname(quantile(x, cut, na.rm = TRUE, names = FALSE))))
  expect_true(all(winsorized_x[!is.na(winsorized_x)] <= unname(quantile(x, 1 - cut, na.rm = TRUE, names = FALSE))))
})

# Test edge cases
test_that("winsorize() handles edge cases", {
  # Test with an empty vector
  expect_equal(winsorize(numeric(0), 0.1), numeric(0))

  # Test with a vector of identical values
  x <- rep(1, 10)
  expect_equal(winsorize(x, 0.1), x)
})

# Test trim() function ------------------------------------------------
library(testthat)

test_that("trim correctly replaces values outside quantiles with NA", {
  set.seed(123)
  x <- rnorm(100)
  cut <- 0.05

  # Trim the data
  trimmed_x <- trim(x, cut)

  # Calculate lower and upper bounds without names for comparison
  lb <- unname(quantile(x, cut, na.rm = TRUE))
  up <- unname(quantile(x, 1 - cut, na.rm = TRUE))

  # Check if values outside the quantiles are replaced with NA
  expect_true(all(is.na(trimmed_x[x < lb]) & is.na(trimmed_x[x > up])))

  # Check if values within the quantiles are unchanged
  expect_true(all(trimmed_x[x >= lb & x <= up] == x[x >= lb & x <= up]))
})

test_that("trim handles NA values correctly", {
  set.seed(123)
  x <- c(NA, rnorm(100), NA)
  cut <- 0.05

  # Trim the data, expecting NA values to remain unchanged
  trimmed_x <- trim(x, cut)

  # Check that NA values are still present
  original_na_positions <- is.na(x)
  trimmed_na_positions <- is.na(trimmed_x)
  expect_true(all(trimmed_na_positions[original_na_positions] == TRUE))

  # Check that non-NA values outside the quantiles are replaced with NA
  lb <- unname(quantile(x, cut, na.rm = TRUE, names = FALSE))
  up <- unname(quantile(x, 1 - cut, na.rm = TRUE, names = FALSE))
  expect_true(all(is.na(trimmed_x[x < lb]) & is.na(trimmed_x[x > up])))
})

test_that("trim handles edge cases", {
  # Test with an empty vector
  expect_equal(trim(numeric(0), 0.1), numeric(0))

  # Test with a vector of identical values, none should be replaced since they all are at the same quantile
  x <- rep(1, 10)
  expect_equal(trim(x, 0.1), x)

  # Test with all values outside the cut, should result in all NAs
  x <- c(1, 2, 3, 4, 5)
  expect_true(all(is.na(trim(x, 0.9))))
})


# Test create_summary_statistics() function ---------------------------
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
