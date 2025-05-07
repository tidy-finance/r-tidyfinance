# Sample dataset
data <- data.frame(
  ret_excess = rnorm(100),
  mkt_excess = rnorm(100),
  smb = rnorm(100),
  hml = rnorm(100)
)

# Test with sufficient observations and a single independent variable
test_that("Single independent variable with sufficient observations", {
  result <- estimate_model(data, "ret_excess ~ mkt_excess", min_obs = 1)
  expect_type(result, "list")
  expect_length(ncol(result), 1)
})

# Test with sufficient observations and multiple independent variables
test_that("Multiple independent variables with sufficient observations", {
  result <- estimate_model(
    data,
    "ret_excess ~ mkt_excess + smb + hml",
    min_obs = 1
  )
  expect_type(result, "list")
  expect_equal(ncol(result), 4)
})

# Test for the presence of specified independent variables in the dataset
test_that("Independent variables are present in the dataset", {
  expect_error(estimate_model(data, "ret_excess ~ fake_var"))
})

# Test with insufficient observations
test_that("Insufficient observations", {
  small_data <- data[1:5, ]
  result <- estimate_model(small_data, "ret_excess ~ mkt_excess", min_obs = 10)
  expect_true(all(is.na(result)))
})

# Test with exactly the minimum required observations
test_that("Exactly minimum required observations", {
  min_obs_data <- data[1:10, ]
  result <- estimate_model(
    min_obs_data,
    "ret_excess ~ mkt_excess",
    min_obs = 10
  )
  expect_type(result, "list")
})

# Test with no independent variables specified
test_that("No independent variables specified", {
  expect_error(estimate_model(data, min_obs = 1))
})

test_that("estimate_model returns tibble with only NAs when insufficient data", {
  set.seed(1234)
  df <- tibble(
    date = seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2020-12-01"),
      by = "month"
    ),
    ret_excess = rnorm(12, 0, 0.1),
    mkt_excess = rnorm(12, 0, 0.1),
    smb = rnorm(12, 0, 0.1),
    hml = rnorm(12, 0, 0.1)
  )

  model <- "ret_excess ~ mkt_excess + smb + hml"

  betas <- slider::slide_period_dfr(
    .x = df,
    .i = df$date,
    .period = "month",
    .f = ~ estimate_model(., model, min_obs = 2),
    .before = 3 - 1,
    .complete = FALSE
  )

  # Check if all values in the tibble are NA
  expect_true(all(is.na(betas)))
  # Check if the output is a tibble
  expect_s3_class(betas, "tbl_df")
})
