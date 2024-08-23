test_that("estimate_fama_macbeth handles missing date column", {
  data <- tibble(
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1),
    log_mktcap = rnorm(600, 10, 1)
  )

  expect_error(
    estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap"),
  )
})

test_that("estimate_fama_macbeth handles invalid vcov option", {
  data <- tibble(
    date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 50),
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1),
    log_mktcap = rnorm(600, 10, 1)
  )

  expect_error(
    estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap", vcov = "invalid_vcov"),
    regexp = "`vcov` must be either 'iid' or 'newey-west'."
  )
})

test_that("estimate_fama_macbeth works with valid iid vcov", {
  data <- tibble(
    date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 50),
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1),
    log_mktcap = rnorm(600, 10, 1)
  )

  result <- estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap", vcov = "iid")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("factor", "risk_premium", "n", "standard_error", "t_statistic") %in% colnames(result)))
})

test_that("estimate_fama_macbeth works with valid newey-west vcov", {
  data <- tibble(
    date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 50),
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1),
    log_mktcap = rnorm(600, 10, 1)
  )

  result <- estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap", vcov = "newey-west")

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("factor", "risk_premium", "n", "standard_error", "t_statistic") %in% colnames(result)))
})

test_that("estimate_fama_macbeth computes correct number of rows", {
  data <- tibble(
    date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 50),
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1),
    log_mktcap = rnorm(600, 10, 1)
  )

  result <- estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap")

  expect_equal(nrow(result), 3)
})

test_that("estimate_fama_macbeth handles edge cases", {
  data <- tibble(
    date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 1),
    permno = 1,
    ret_excess = rnorm(12, 0, 0.1),
    beta = rnorm(12, 1, 0.2),
    bm = rnorm(12, 0.5, 0.1),
    log_mktcap = rnorm(12, 10, 1)
  )

  expect_error(
    estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap"),
    regexp = "Each date grouping must have more rows than the number of predictors in the model to estimate coefficients. Please check your data."
  )
})
