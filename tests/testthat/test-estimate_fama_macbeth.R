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
    date = rep(
      seq.Date(
        from = as.Date("2020-01-01"),
        to = as.Date("2020-12-01"),
        by = "month"
      ),
      each = 50
    ),
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1),
    log_mktcap = rnorm(600, 10, 1)
  )

  expect_error(
    estimate_fama_macbeth(
      data,
      "ret_excess ~ beta + bm + log_mktcap",
      vcov = "invalid_vcov"
    ),
    regexp = "`vcov` must be either 'iid' or 'newey-west'."
  )
})

test_that("estimate_fama_macbeth works with valid iid vcov", {
  data <- tibble(
    date = rep(
      seq.Date(
        from = as.Date("2020-01-01"),
        to = as.Date("2020-12-01"),
        by = "month"
      ),
      each = 50
    ),
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1),
    log_mktcap = rnorm(600, 10, 1)
  )

  result <- estimate_fama_macbeth(
    data,
    "ret_excess ~ beta + bm + log_mktcap",
    vcov = "iid"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c(
      "factor",
      "risk_premium",
      "n",
      "standard_error",
      "t_statistic"
    ) %in%
      colnames(result)
  ))
})

test_that("estimate_fama_macbeth works with valid newey-west vcov", {
  data <- tibble(
    date = rep(
      seq.Date(
        from = as.Date("2020-01-01"),
        to = as.Date("2020-12-01"),
        by = "month"
      ),
      each = 50
    ),
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1),
    log_mktcap = rnorm(600, 10, 1)
  )

  result <- estimate_fama_macbeth(
    data,
    "ret_excess ~ beta + bm + log_mktcap",
    vcov = "newey-west",
    vcov_options = list(prewhite = FALSE)
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c(
      "factor",
      "risk_premium",
      "n",
      "standard_error",
      "t_statistic"
    ) %in%
      colnames(result)
  ))
})

test_that("estimate_fama_macbeth computes correct number of rows", {
  data <- tibble(
    date = rep(
      seq.Date(
        from = as.Date("2020-01-01"),
        to = as.Date("2020-12-01"),
        by = "month"
      ),
      each = 50
    ),
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1),
    log_mktcap = rnorm(600, 10, 1)
  )

  result <- estimate_fama_macbeth(
    data,
    "ret_excess ~ beta + bm + log_mktcap",
    vcov_options = list(prewhite = FALSE)
  )

  expect_equal(nrow(result), 4)
})

test_that("estimate_fama_macbeth handles edge cases", {
  data <- tibble(
    date = rep(
      seq.Date(
        from = as.Date("2020-01-01"),
        to = as.Date("2020-12-01"),
        by = "month"
      ),
      each = 1
    ),
    permno = 1,
    ret_excess = rnorm(12, 0, 0.1),
    beta = rnorm(12, 1, 0.2),
    bm = rnorm(12, 0.5, 0.1),
    log_mktcap = rnorm(12, 10, 1)
  )

  expect_error(
    estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap"),
    regexp = paste(
      "Each date grouping must have more rows than the number of predictors",
      "in the model to estimate coefficients. Please check your data."
    )
  )
})

test_that(
  paste(
    "estimate_fama_macbeth detail = FALSE returns flat data frame by default"
  ),
  {
    set.seed(123)
    data <- tibble(
      date = rep(
        seq.Date(
          from = as.Date("2020-01-01"),
          to = as.Date("2020-12-01"),
          by = "month"
        ),
        each = 50
      ),
      permno = rep(1:50, times = 12),
      ret_excess = rnorm(600, 0, 0.1),
      beta = rnorm(600, 1, 0.2),
      bm = rnorm(600, 0.5, 0.1)
    )

    result <- estimate_fama_macbeth(data, "ret_excess ~ beta + bm")

    expect_s3_class(result, "tbl_df")
    expect_true(all(
      c(
        "factor",
        "risk_premium",
        "n",
        "standard_error",
        "t_statistic"
      ) %in%
        colnames(result)
    ))
  }
)

test_that(
  paste(
    "estimate_fama_macbeth detail = TRUE returns list with two data frames"
  ),
  {
    set.seed(123)
    data <- tibble(
      date = rep(
        seq.Date(
          from = as.Date("2020-01-01"),
          to = as.Date("2020-12-01"),
          by = "month"
        ),
        each = 50
      ),
      permno = rep(1:50, times = 12),
      ret_excess = rnorm(600, 0, 0.1),
      beta = rnorm(600, 1, 0.2),
      bm = rnorm(600, 0.5, 0.1)
    )

    result <- estimate_fama_macbeth(
      data,
      "ret_excess ~ beta + bm",
      detail = TRUE
    )

    expect_type(result, "list")
    expect_named(result, c("coefficients", "summary_statistics"))
    expect_s3_class(result$coefficients, "tbl_df")
    expect_s3_class(result$summary_statistics, "tbl_df")
  }
)

test_that(
  paste(
    "estimate_fama_macbeth detail = TRUE coefficients has correct structure"
  ),
  {
    set.seed(123)
    data <- tibble(
      date = rep(
        seq.Date(
          from = as.Date("2020-01-01"),
          to = as.Date("2020-12-01"),
          by = "month"
        ),
        each = 50
      ),
      permno = rep(1:50, times = 12),
      ret_excess = rnorm(600, 0, 0.1),
      beta = rnorm(600, 1, 0.2),
      bm = rnorm(600, 0.5, 0.1),
      log_mktcap = rnorm(600, 10, 1)
    )

    result <- estimate_fama_macbeth(
      data,
      "ret_excess ~ beta + bm + log_mktcap",
      detail = TRUE
    )

    coefficients <- result$coefficients
    expect_equal(
      colnames(coefficients),
      c("factor", "risk_premium", "n", "standard_error", "t_statistic")
    )
    expect_equal(nrow(coefficients), 4)
    expect_false("r_squared" %in% colnames(coefficients))
    expect_false("adj_r_squared" %in% colnames(coefficients))
    expect_false("n_obs" %in% colnames(coefficients))
  }
)

test_that(
  paste(
    "estimate_fama_macbeth detail = TRUE summary_statistics",
    "has correct structure"
  ),
  {
    set.seed(42)
    data <- tibble(
      date = rep(
        seq.Date(
          from = as.Date("2020-01-01"),
          to = as.Date("2020-12-01"),
          by = "month"
        ),
        each = 50
      ),
      permno = rep(1:50, times = 12),
      ret_excess = rnorm(600, 0, 0.1),
      beta = rnorm(600, 1, 0.2),
      bm = rnorm(600, 0.5, 0.1)
    )

    result <- estimate_fama_macbeth(
      data,
      "ret_excess ~ beta + bm",
      detail = TRUE
    )

    summary_stats <- result$summary_statistics
    expect_equal(
      colnames(summary_stats),
      c("r_squared", "adj_r_squared", "n_obs")
    )
    expect_equal(nrow(summary_stats), 1)
    expect_true(summary_stats$r_squared >= 0 & summary_stats$r_squared <= 1)
    expect_true(summary_stats$adj_r_squared <= summary_stats$r_squared)
    expect_equal(summary_stats$n_obs, 50)
  }
)

test_that("estimate_fama_macbeth detail works with different vcov options", {
  set.seed(123)
  data <- tibble(
    date = rep(
      seq.Date(
        from = as.Date("2020-01-01"),
        to = as.Date("2020-12-01"),
        by = "month"
      ),
      each = 50
    ),
    permno = rep(1:50, times = 12),
    ret_excess = rnorm(600, 0, 0.1),
    beta = rnorm(600, 1, 0.2),
    bm = rnorm(600, 0.5, 0.1)
  )

  result_iid <- estimate_fama_macbeth(
    data,
    "ret_excess ~ beta + bm",
    vcov = "iid",
    detail = TRUE
  )
  result_nw <- estimate_fama_macbeth(
    data,
    "ret_excess ~ beta + bm",
    vcov = "newey-west",
    detail = TRUE
  )

  # Both should return the same structure
  expect_type(result_iid, "list")
  expect_named(result_iid, c("coefficients", "summary_statistics"))
  expect_type(result_nw, "list")
  expect_named(result_nw, c("coefficients", "summary_statistics"))

  # Risk premia should be identical (same data, same cross-sectional fits)
  expect_equal(
    result_iid$coefficients$risk_premium,
    result_nw$coefficients$risk_premium
  )

  # Standard errors should differ between vcov methods
  expect_false(
    all(
      result_iid$coefficients$standard_error ==
        result_nw$coefficients$standard_error
    )
  )
})
