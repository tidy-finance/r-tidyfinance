test_that("returns correct structure with all arguments including `...`", {
  result <- breakpoint_options(
    n_portfolios = 5,
    percentiles = c(0.2, 0.8),
    breakpoints_exchanges = "NYSE",
    smooth_bunching = TRUE,
    breakpoints_min_size_threshold = 0.1,
    custom_arg = "test"
  )

  expect_s3_class(result, "tidyfinance_breakpoint_options")
  expect_equal(result$n_portfolios, 5)
  expect_equal(result$percentiles, c(0.2, 0.8))
  expect_equal(result$breakpoints_exchanges, "NYSE")
  expect_true(result$smooth_bunching)
  expect_equal(result$breakpoints_min_size_threshold, 0.1)
  expect_equal(result$custom_arg, "test")
})

test_that("returns correct structure with default (NULL) arguments", {
  result <- breakpoint_options()

  expect_s3_class(result, "tidyfinance_breakpoint_options")
  expect_null(result$n_portfolios)
  expect_null(result$percentiles)
  expect_null(result$breakpoints_exchanges)
  expect_false(result$smooth_bunching)
  expect_null(result$breakpoints_min_size_threshold)
})

test_that("n_portfolios errors on non-numeric, non-positive, non-integer", {
  expect_error(
    breakpoint_options(n_portfolios = "a"),
    regexp = "n_portfolios"
  )
  expect_error(
    breakpoint_options(n_portfolios = -1),
    regexp = "n_portfolios"
  )
  expect_error(
    breakpoint_options(n_portfolios = 1.5),
    regexp = "n_portfolios"
  )
})

test_that("percentiles errors on non-numeric and out-of-range values", {
  expect_error(
    breakpoint_options(percentiles = "a"),
    regexp = "percentiles"
  )
  expect_error(
    breakpoint_options(percentiles = c(0.5, 1.5)),
    regexp = "percentiles"
  )
})

test_that("breakpoints_exchanges errors on non-character and empty vector", {
  expect_error(
    breakpoint_options(breakpoints_exchanges = 123),
    regexp = "breakpoints_exchanges"
  )
  expect_error(
    breakpoint_options(breakpoints_exchanges = character(0)),
    regexp = "breakpoints_exchanges"
  )
})

test_that("smooth_bunching errors on non-logical, NA, and length > 1", {
  expect_error(
    breakpoint_options(smooth_bunching = "yes"),
    regexp = "smooth_bunching"
  )
  expect_error(
    breakpoint_options(smooth_bunching = NA),
    regexp = "smooth_bunching"
  )
  expect_error(
    breakpoint_options(smooth_bunching = c(TRUE, FALSE)),
    regexp = "smooth_bunching"
  )
})

test_that("breakpoints_min_size_threshold errors on invalid values", {
  expect_error(
    breakpoint_options(
      breakpoints_min_size_threshold = c(0.1, 0.2)
    ),
    regexp = "breakpoints_min_size_threshold"
  )
  expect_error(
    breakpoint_options(breakpoints_min_size_threshold = "a"),
    regexp = "breakpoints_min_size_threshold"
  )
  expect_error(
    breakpoint_options(breakpoints_min_size_threshold = 0),
    regexp = "breakpoints_min_size_threshold"
  )
  expect_error(
    breakpoint_options(breakpoints_min_size_threshold = 1),
    regexp = "breakpoints_min_size_threshold"
  )
})
