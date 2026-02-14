set.seed(42)
test_data <- data.frame(
  ret_excess = rnorm(100),
  mkt_excess = rnorm(100),
  smb = rnorm(100),
  hml = rnorm(100)
)

test_data_with_na <- test_data
test_data_with_na$ret_excess[1:5] <- NA
test_data_with_na$mkt_excess[6:10] <- NA

small_data <- data.frame(
  ret_excess = rnorm(3),
  mkt_excess = rnorm(3),
  smb = rnorm(3)
)

test_that("invalid output values raise an error", {
  expect_error(
    estimate_model(test_data, "ret_excess ~ mkt_excess", output = "invalid"),
    "output"
  )
  expect_error(
    estimate_model(
      test_data,
      "ret_excess ~ mkt_excess",
      output = c("coefficients", "bogus")
    ),
    "output"
  )
})

test_that("missing independent variables raise an error", {
  expect_error(
    estimate_model(test_data, "ret_excess ~ nonexistent_var"),
    "missing"
  )
  expect_error(
    estimate_model(test_data, "ret_excess ~ mkt_excess + nonexistent_var"),
    "nonexistent_var"
  )
})

test_that("column named 'intercept' raises an error", {
  bad_data <- test_data
  bad_data$intercept <- rnorm(100)
  expect_error(
    estimate_model(bad_data, "ret_excess ~ intercept"),
    "intercept"
  )
})

test_that("default output returns a tibble of coefficients", {
  result <- estimate_model(test_data, "ret_excess ~ mkt_excess")
  expect_s3_class(result, "tbl_df")
  expect_true("intercept" %in% names(result))
  expect_true("mkt_excess" %in% names(result))
  expect_equal(ncol(result), 2)
})

test_that("coefficients match lm() output", {
  result <- estimate_model(test_data, "ret_excess ~ mkt_excess + smb + hml")
  fit <- lm(ret_excess ~ mkt_excess + smb + hml, data = test_data)
  coefs <- coef(fit)
  expect_equal(result$intercept, unname(coefs["(Intercept)"]))
  expect_equal(result$mkt_excess, unname(coefs["mkt_excess"]))
  expect_equal(result$smb, unname(coefs["smb"]))
  expect_equal(result$hml, unname(coefs["hml"]))
})

test_that("model without intercept omits intercept column", {
  result <- estimate_model(test_data, "ret_excess ~ mkt_excess - 1")
  expect_false("intercept" %in% names(result))
  expect_true("mkt_excess" %in% names(result))
})

test_that("tstats output returns t-statistics as tibble", {
  result <- estimate_model(
    test_data,
    "ret_excess ~ mkt_excess + smb",
    output = "tstats"
  )
  expect_s3_class(result, "tbl_df")
  expect_true("intercept" %in% names(result))
  expect_true("mkt_excess" %in% names(result))
  expect_true("smb" %in% names(result))

  fit <- lm(ret_excess ~ mkt_excess + smb, data = test_data)
  tvals <- summary(fit)$coefficients[, "t value"]
  expect_equal(result$mkt_excess, unname(tvals["mkt_excess"]))
})

test_that("residuals output returns numeric vector of correct length", {
  result <- estimate_model(
    test_data,
    "ret_excess ~ mkt_excess",
    output = "residuals"
  )
  expect_type(result, "double")
  expect_length(result, nrow(test_data))
})

test_that("residuals match lm() residuals", {
  result <- estimate_model(
    test_data,
    "ret_excess ~ mkt_excess + smb",
    output = "residuals"
  )
  fit <- lm(ret_excess ~ mkt_excess + smb, data = test_data)
  expect_equal(result, unname(fit$residuals))
})

test_that("residuals are NA where data has missing values", {
  result <- estimate_model(
    test_data_with_na,
    "ret_excess ~ mkt_excess",
    output = "residuals"
  )
  expect_length(result, nrow(test_data_with_na))
  expect_true(all(is.na(result[1:10])))
  expect_true(all(!is.na(result[11:100])))
})

test_that("multiple outputs return a named list", {
  result <- estimate_model(
    test_data,
    "ret_excess ~ mkt_excess",
    output = c("coefficients", "tstats", "residuals")
  )
  expect_type(result, "list")
  expect_named(result, c("coefficients", "tstats", "residuals"))
  expect_s3_class(result$coefficients, "tbl_df")
  expect_s3_class(result$tstats, "tbl_df")
  expect_type(result$residuals, "double")
})

test_that("two outputs return a named list with two elements", {
  result <- estimate_model(
    test_data,
    "ret_excess ~ mkt_excess",
    output = c("coefficients", "residuals")
  )
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("coefficients", "residuals"))
})

test_that("insufficient observations return NA coefficients", {
  result <- estimate_model(small_data, "ret_excess ~ mkt_excess", min_obs = 100)
  expect_s3_class(result, "tbl_df")
  expect_true(all(is.na(result)))
})

test_that("insufficient observations return NA residuals", {
  result <- estimate_model(
    small_data,
    "ret_excess ~ mkt_excess",
    min_obs = 100,
    output = "residuals"
  )
  expect_type(result, "double")
  expect_length(result, nrow(small_data))
  expect_true(all(is.na(result)))
})

test_that("insufficient observations return NA tstats", {
  result <- estimate_model(
    small_data,
    "ret_excess ~ mkt_excess",
    min_obs = 100,
    output = "tstats"
  )
  expect_s3_class(result, "tbl_df")
  expect_true(all(is.na(result)))
})

test_that("model with multiple independent variables works", {
  result <- estimate_model(test_data, "ret_excess ~ mkt_excess + smb + hml")
  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 4) # intercept + 3 vars
})

test_that("min_obs = 1 works with minimal data", {
  tiny <- data.frame(ret_excess = 1:2, mkt_excess = 1:2)
  result <- estimate_model(tiny, "ret_excess ~ mkt_excess", min_obs = 1)
  expect_s3_class(result, "tbl_df")
})

test_that("residuals are NA when too few complete cases after NA removal", {
  almost_all_na <- test_data
  almost_all_na$ret_excess[1:99] <- NA
  result <- estimate_model(
    almost_all_na,
    "ret_excess ~ mkt_excess + smb",
    min_obs = 5,
    output = "residuals"
  )
  expect_true(all(is.na(result)))
})
