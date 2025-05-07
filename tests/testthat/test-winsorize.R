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
  expect_true(all(
    winsorized_x[!is.na(winsorized_x)] >=
      unname(quantile(x, cut, na.rm = TRUE, names = FALSE))
  ))
  expect_true(all(
    winsorized_x[!is.na(winsorized_x)] <=
      unname(quantile(x, 1 - cut, na.rm = TRUE, names = FALSE))
  ))
})

# Test edge cases
test_that("winsorize() handles edge cases", {
  # Test with an empty vector
  expect_equal(winsorize(numeric(0), 0.1), numeric(0))

  # Test with a vector of identical values
  x <- rep(1, 10)
  expect_equal(winsorize(x, 0.1), x)
})
