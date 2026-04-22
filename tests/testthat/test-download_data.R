test_that("download_data errors when domain is NULL", {
  expect_error(
    download_data(domain = NULL),
    "domain.*required"
  )
})

test_that("download_data errors for unsupported domain", {
  expect_error(
    download_data(domain = "not_a_real_domain"),
    "Unsupported domain"
  )
})

test_that("download_data errors for start_date after end_date via famafrench", {
  expect_error(
    download_data(
      domain = "famafrench",
      dataset = "Fama/French 3 Factors",
      start_date = "2021-12-31",
      end_date = "2020-01-01"
    ),
    regexp = "`start_date` cannot be after `end_date`"
  )
})

test_that("download_data errors for start_date after end_date via globalq", {
  expect_error(
    download_data(
      domain = "globalq",
      dataset = "q5_factors_daily_2024",
      start_date = "2021-12-31",
      end_date = "2020-01-01"
    ),
    regexp = "`start_date` cannot be after `end_date`"
  )
})

test_that("download_data errors for start_date after end_date via macro_predictors", {
  expect_error(
    download_data(
      domain = "macro_predictors",
      dataset = "monthly",
      start_date = "2021-12-31",
      end_date = "2020-01-01"
    ),
    regexp = "`start_date` cannot be after `end_date`"
  )
})

test_that("download_data emits deprecation warning for legacy type argument", {
  expect_warning(
    expect_error(
      download_data(
        type = "factors_ff3_monthly",
        start_date = "2025-01-01",
        end_date = "2020-01-01"
      )
    ),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("download_data emits deprecation warning for legacy Global Q type", {
  expect_warning(
    expect_error(
      download_data(
        type = "factors_q5_daily",
        start_date = "2025-01-01",
        end_date = "2020-01-01"
      )
    ),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("download_data errors for unsupported domain after handling legacy type", {
  # Passing a valid domain string as a domain that isn't supported
  expect_error(
    download_data(domain = "imaginary_domain_xyz"),
    "Unsupported domain"
  )
})
