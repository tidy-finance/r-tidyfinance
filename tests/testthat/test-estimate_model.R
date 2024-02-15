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
