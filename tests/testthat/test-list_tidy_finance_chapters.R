test_that("list_tidy_finance_chapters returns a character vector", {
  result <- list_tidy_finance_chapters()
  expect_type(result, "character")
})

test_that("list_tidy_finance_chapters returns multiple chapters", {
  result <- list_tidy_finance_chapters()
  expect_gt(length(result), 5)
})

test_that("list_tidy_finance_chapters has no NAs", {
  result <- list_tidy_finance_chapters()
  expect_true(all(!is.na(result)))
})

test_that("list_tidy_finance_chapters contains known chapters", {
  result <- list_tidy_finance_chapters()
  expect_true("beta-estimation" %in% result)
  expect_true("univariate-portfolio-sorts" %in% result)
  expect_true("fama-macbeth-regressions" %in% result)
  expect_true("introduction-to-tidy-finance" %in% result)
})

test_that("list_tidy_finance_chapters values are unique", {
  result <- list_tidy_finance_chapters()
  expect_equal(length(unique(result)), length(result))
})

test_that("list_tidy_finance_chapters values use lowercase and hyphens (no spaces)", {
  result <- list_tidy_finance_chapters()
  expect_true(all(!grepl(" ", result)))
  expect_true(all(grepl("^[a-z0-9-]+$", result)))
})
