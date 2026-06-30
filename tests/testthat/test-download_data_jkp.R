test_helper_manifest <- function() {
  list(
    factors = list(
      usa = c("mkt", "all_factors", "all_themes", "value", "be_me"),
      frontier = c("mkt", "all_factors", "aliq_mat")
    ),
    portfolios = list(
      usa = c("be_me", "ret_12_1")
    ),
    industry = list(
      usa = c("gics", "ff49")
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

test_that("downloads portfolios and coerces the pf identifier to integer", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) test_helper_manifest(),
    download_jkp_file = function(url, ...) {
      tibble(
        location = "usa", name = "be_me",
        pf = c(1.0, 3.0), n = c(494, 497),
        freq = "monthly", weighting = "vw_cap",
        date = c("1926-01-31", "1926-01-31"), ret = c(0.001, -0.002)
      )
    }
  )

  result <- download_data_jkp(dataset = "portfolios", factors = "be_me")

  expect_type(result$pf, "integer")
  expect_equal(result$pf, c(1L, 3L))
  expect_equal(result$date, ymd(c("1926-01-01", "1926-01-01")))
})

test_that("downloads industry returns at monthly frequency", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) test_helper_manifest(),
    download_jkp_file = function(url, ...) {
      tibble(
        gics = c(55, 15), date = c("1999-07-31", "1999-07-31"),
        n = c(170, 376), location = "usa", ret = c(-0.004, -0.036),
        freq = "monthly", weighting = "vw_cap"
      )
    }
  )

  result <- download_data_jkp(dataset = "industry", classification = "gics")

  expect_equal(result$date, ymd(c("1999-07-01", "1999-07-01")))
  expect_true("gics" %in% names(result))
})

test_that("downloads reference cutoffs and renames eom to date", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    download_jkp_csv = function(url, ...) {
      tibble(
        eom = c("1925-12-31", "1926-01-31"),
        n = c(495, 508),
        nyse_p50 = c(15.84, 16.25)
      )
    }
  )

  result <- download_data_jkp(dataset = "nyse_cutoffs")

  expect_true("date" %in% names(result))
  expect_false("eom" %in% names(result))
  expect_equal(result$date, ymd(c("1925-12-01", "1926-01-01")))
})

test_that("return_cutoffs selects the daily file by frequency", {
  captured_url <- NULL
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    download_jkp_csv = function(url, ...) {
      captured_url <<- url
      tibble(eom = "2020-01-31", ret_1 = -0.14)
    }
  )

  download_data_jkp(dataset = "return_cutoffs", frequency = "daily")

  expect_match(captured_url, "return_cutoffs_daily\\.csv$")
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
    download_data_jkp(dataset = "bogus"),
    "dataset"
  )
})

test_that("aborts on daily request for the industry dataset", {
  expect_error(
    download_data_jkp(dataset = "industry", frequency = "daily"),
    "monthly frequency"
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

test_that("aborts on invalid industry classification", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) test_helper_manifest()
  )

  expect_error(
    download_data_jkp(
      dataset = "industry", region = "usa", classification = "naics"
    ),
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
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    }
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

test_that("builds the bracketed S3 object keys", {
  expect_equal(
    build_jkp_url("factors", "usa", "all_factors", "monthly", "vw_cap"),
    paste0(
      "https://jkpfactors-data.s3.amazonaws.com/public/",
      "%5Busa%5D_%5Ball_factors%5D_%5Bmonthly%5D_%5Bvw_cap%5D.zip"
    )
  )
  expect_equal(
    build_jkp_url("portfolios", "usa", "be_me", "monthly", "vw_cap"),
    paste0(
      "https://jkpfactors-data.s3.amazonaws.com/public/portfolios/",
      "%5Busa%5D_%5Bbe_me%5D_%5Bmonthly%5D_%5Bvw_cap%5D.zip"
    )
  )
  # Industry always uses the monthly key regardless of the frequency argument.
  expect_equal(
    build_jkp_url("industry", "usa", "gics", "daily", "ew"),
    paste0(
      "https://jkpfactors-data.s3.amazonaws.com/public/industry/",
      "%5Busa%5D_%5Bgics%5D_%5Bmonthly%5D_%5Bew%5D.zip"
    )
  )
})

test_that("builds reference file urls", {
  expect_match(
    build_jkp_reference_url("nyse_cutoffs", "monthly"),
    "/public/other/nyse_cutoffs\\.csv$"
  )
  expect_match(
    build_jkp_reference_url("return_cutoffs", "monthly"),
    "/public/other/return_cutoffs\\.csv$"
  )
  expect_match(
    build_jkp_reference_url("return_cutoffs", "daily"),
    "/public/other/return_cutoffs_daily\\.csv$"
  )
})

test_that("returns empty tibble when a factor download fails", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    fetch_jkp_availability = function(...) test_helper_manifest(),
    download_jkp_file = function(url, ...) tibble(date = Date())
  )

  expect_message(
    result <- download_data_jkp(region = "usa", factors = "mkt"),
    "download or parsing failure"
  )
  expect_equal(nrow(result), 0)
})

test_that("returns empty tibble when a reference download fails", {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date) {
      list(start_date = NULL, end_date = NULL)
    },
    download_jkp_csv = function(url, ...) tibble(date = Date())
  )

  expect_message(
    result <- download_data_jkp(dataset = "nyse_cutoffs"),
    "download or parsing failure"
  )
  expect_equal(nrow(result), 0)
})

test_that("list_supported_jkp_factors returns regions and per-region values", {
  local_mocked_bindings(
    fetch_jkp_availability = function(...) test_helper_manifest()
  )

  expect_equal(list_supported_jkp_factors(), c("usa", "frontier"))
  expect_true("be_me" %in% list_supported_jkp_factors("usa"))
  expect_equal(
    list_supported_jkp_factors("usa", dataset = "industry"),
    c("gics", "ff49")
  )
  expect_error(list_supported_jkp_factors("atlantis"), "Unsupported")
})

test_that("list_supported_jkp_factors returns empty vector on manifest failure", {
  local_mocked_bindings(
    fetch_jkp_availability = function(...) cli::cli_abort("boom")
  )

  expect_message(
    result <- list_supported_jkp_factors(),
    "Returning an empty vector due to download failure."
  )
  expect_equal(result, character())
})

test_that("fetch_jkp_availability parses the JSON manifest", {
  local_mocked_bindings(get_random_user_agent = function() "test-agent")
  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_perform = function(req, path = NULL) invisible(req),
    resp_body_string = function(resp, ...) {
      '{"factors":{"usa":["mkt","be_me"]}}'
    },
    .package = "httr2"
  )

  result <- fetch_jkp_availability()

  expect_equal(result$factors$usa, c("mkt", "be_me"))
})

test_that("download_jkp_file unzips and reads the first CSV", {
  local_mocked_bindings(get_random_user_agent = function() "test-agent")
  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_perform = function(req, path = NULL) {
      if (!is.null(path)) file.create(path)
      invisible(req)
    },
    .package = "httr2"
  )
  local_mocked_bindings(
    unzip = function(zipfile, exdir, ...) {
      writeLines(
        c("location,date,ret", "usa,2020-01-31,0.01"),
        file.path(exdir, "data.csv")
      )
      invisible(NULL)
    },
    .package = "utils"
  )

  result <- download_jkp_file("https://example.com/x.zip")

  expect_equal(result$location, "usa")
  expect_equal(result$ret, 0.01)
})

test_that("download_jkp_file aborts when the archive has no CSV", {
  local_mocked_bindings(get_random_user_agent = function() "test-agent")
  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_perform = function(req, path = NULL) {
      if (!is.null(path)) file.create(path)
      invisible(req)
    },
    .package = "httr2"
  )
  local_mocked_bindings(
    unzip = function(zipfile, exdir, ...) invisible(NULL),
    .package = "utils"
  )

  expect_error(
    download_jkp_file("https://example.com/x.zip"),
    "No CSV file"
  )
})

test_that("download_jkp_csv reads a plain CSV file", {
  local_mocked_bindings(get_random_user_agent = function() "test-agent")
  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, ...) req,
    req_timeout = function(req, ...) req,
    req_retry = function(req, ...) req,
    req_perform = function(req, path = NULL) {
      if (!is.null(path)) writeLines(c("eom,n", "2020-01-31,5"), path)
      invisible(req)
    },
    .package = "httr2"
  )

  result <- download_jkp_csv("https://example.com/x.csv")

  expect_equal(result$n, 5L)
})
