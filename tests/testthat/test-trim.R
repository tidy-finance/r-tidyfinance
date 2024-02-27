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
})
