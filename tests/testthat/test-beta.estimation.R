# Tests for estimate_model() ------------------------------------------
# Sample dataset
data <- data.frame(
  ret_excess = rnorm(100),
  mkt_excess = rnorm(100),
  smb = rnorm(100),
  hml = rnorm(100)
)

# Test with sufficient observations and a single independent variable
test_that("Single independent variable with sufficient observations", {
  result <- estimate_model(data, mkt_excess, min_obs = 1)
  expect_type(result, "double")
  expect_length(result, 1)
})

# Test with sufficient observations and multiple independent variables
test_that("Multiple independent variables with sufficient observations", {
  result <- estimate_model(data, mkt_excess, smb, hml, min_obs = 1)
  expect_type(result, "list")
  expect_equal(ncol(result), 3)
})

# Test for the presence of specified independent variables in the dataset
test_that("Independent variables are present in the dataset", {
  expect_error(estimate_model(data, fake_var))
})

# Test with insufficient observations
test_that("Insufficient observations", {
  small_data <- data[1:5, ]
  result <- estimate_model(small_data, mkt_excess, min_obs = 10)
  expect_true(all(is.na(result)))
})

# Test with exactly the minimum required observations
test_that("Exactly minimum required observations", {
  min_obs_data <- data[1:10, ]
  result <- estimate_model(min_obs_data, mkt_excess, min_obs = 10)
  expect_type(result, "double")
})

# Test with no independent variables specified
test_that("No independent variables specified", {
  expect_error(estimate_model(data, min_obs = 1))
})

# Tests for roll_model_estimation() -----------------------------------
# test_that("Function returns error for missing independent variables", {
#   data <- tibble(month = 1:5, ret_excess = rnorm(5), mkt_excess = rnorm(5))
#   expect_error(roll_model_estimation(data, months = 3, min_obs = 1, smb, hml), "missing in the data")
# })
#
# test_that("Function handles correct input without error", {
#   data <- tibble(month = as.Date(1:5), ret_excess = rnorm(5), mkt_excess = rnorm(5))
#   expect_silent(roll_model_estimation(data, months = 3, min_obs = 1, mkt_excess))
# })
#
# test_that("Function returns a tibble with correct structure", {
#   data <- tibble(month = 1:5, ret_excess = rnorm(5), mkt_excess = rnorm(5))
#   result <- roll_model_estimation(data, months = 3, min_obs = 1, mkt_excess)
#   expect_true(is.tibble(result))
#   expect_true(all(c("month", "beta") %in% names(result)))
# })
#
# test_that("Function correctly calculates with specified months and min_obs", {
#   # Create a dataset with known beta values
#   data <- tibble(month = 1:12, ret_excess = 1:12, mkt_excess = rep(1, 12))
#   # Assuming a simplistic model where ret_excess = mkt_excess, beta should be 1
#   result <- roll_model_estimation(data, months = 6, min_obs = 1, mkt_excess)
#   # Check if the beta calculation is as expected
#   expect_true(all(result$beta == 1, na.rm = TRUE))
# })


