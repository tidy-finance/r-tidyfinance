test_helper_manifest <- function() {
  list(
    factors = list(
      usa = c("mkt", "all_factors", "all_themes", "value", "be_me"),
      frontier = c("mkt", "all_factors", "aliq_mat")
    ),
    factors_monthly_only = list(
      frontier = c("aliq_mat")
    )
  )
}

test_that("downloads and processes monthly factor returns", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) test_helper_manifest(),
    download_jkp_file = function(url, ...) {
      tibble(
        location = "usa",
        name = "mkt",
        freq = "monthly",
        weighting = "vw_cap",
        direction = NA_character_,
        n_stocks = c(494, 505),
        n_stocks_min = NA_integer_,
        date = c("1926-01-31", "1926-02-28"),
        ret = c(0.001, -0.046)
      )
    }
  )

  result <- download_data_jkp(region = "usa", factors = "mkt")

  expect_s3_class(result, "tbl_df")
  # Dates are aligned to the beginning of the month.
  expect_equal(result$date, ymd(c("1926-01-01", "1926-02-01")))
  # Returns are already decimal and must not be rescaled.
  expect_equal(result$ret, c(0.001, -0.046))
})

test_that("keeps daily dates as-is", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) test_helper_manifest(),
    download_jkp_file = function(url, ...) {
      tibble(date = c("2020-01-02", "2020-01-03"), ret = c(0.01, 0.02))
    }
  )

  result <- download_data_jkp(
    region = "usa", factors = "mkt", frequency = "daily"
  )

  expect_equal(result$date, ymd(c("2020-01-02", "2020-01-03")))
})

test_that("filters rows when both dates are supplied", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = ymd("2020-02-01"), end_date = ymd("2020-02-28"))
    },
    fetch_jkp_availability = function(...) test_helper_manifest(),
    download_jkp_file = function(url, ...) {
      tibble(
        date = c("2020-01-31", "2020-02-29", "2020-03-31"),
        ret = c(1, 2, 3)
      )
    }
  )

  result <- download_data_jkp(
    region = "usa", factors = "mkt",
    start_date = "2020-02-01", end_date = "2020-02-28"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$date, ymd("2020-02-01"))
})

test_that("aborts on unsupported dataset", {
  expect_error(
    download_data_jkp(dataset = "portfolios"),
    "Only"
  )
})

test_that("aborts on invalid region", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) test_helper_manifest()
  )

  expect_error(
    download_data_jkp(region = "atlantis", factors = "mkt"),
    "Unsupported"
  )
})

test_that("aborts on factor not available in region", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) test_helper_manifest()
  )

  expect_error(
    download_data_jkp(region = "frontier", factors = "value"),
    "Unsupported"
  )
})

test_that("aborts on daily request for a monthly-only factor", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) test_helper_manifest()
  )

  expect_error(
    download_data_jkp(
      region = "frontier", factors = "aliq_mat", frequency = "daily"
    ),
    "only available at monthly frequency"
  )
})

test_that("aborts on invalid frequency or weighting", {
  expect_error(
    download_data_jkp(frequency = "weekly"),
    "frequency"
  )
  expect_error(
    download_data_jkp(weighting = "gdp_weighted"),
    "weighting"
  )
})

test_that("returns empty tibble when the manifest is unavailable", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) cli::cli_abort("boom")
  )

  expect_message(
    result <- download_data_jkp(),
    "Returning an empty data set due to download failure."
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("builds the bracketed S3 object key", {
  url <- build_jkp_factors_url("usa", "all_factors", "monthly", "vw_cap")
  expect_equal(
    url,
    paste0(
      "https://jkpfactors-data.s3.amazonaws.com/public/",
      "%5Busa%5D_%5Ball_factors%5D_%5Bmonthly%5D_%5Bvw_cap%5D.zip"
    )
  )
})

test_that("list_supported_jkp_factors returns regions and per-region factors", {
  local_mocked_bindings(
    fetch_jkp_availability = function(...) test_helper_manifest()
  )

  expect_equal(list_supported_jkp_factors(), c("usa", "frontier"))
  expect_true("be_me" %in% list_supported_jkp_factors("usa"))
  expect_error(list_supported_jkp_factors("atlantis"), "Unsupported")
})
