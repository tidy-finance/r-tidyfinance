test_that("download_data dispatches supported domains", {
  out <- tibble::tibble(ok = TRUE)

  local_mocked_bindings(
    check_supported_domain = function(domain) NULL,
    download_data_factors_ff = function(...) out,
    download_data_factors_q = function(...) out,
    download_data_macro_predictors = function(...) out,
    download_data_wrds = function(...) out,
    download_data_constituents = function(...) out,
    download_data_fred = function(...) out,
    download_data_stock_prices = function(...) out,
    download_data_osap = function(...) out,
    download_data_risk_free = function(...) out,
    download_data_huggingface = function(...) out
  )

  cases <- list(
    list("famafrench", "d"),
    list("globalq", "d"),
    list("macro_predictors", "d"),
    list("wrds", "d"),
    list("constituents", NULL),
    list("fred", NULL),
    list("stock_prices", NULL),
    list("osap", NULL),
    list("tidyfinance", "risk_free"),
    list("tidyfinance", "factor_library")
  )

  for (case in cases) {
    expect_identical(
      download_data(case[[1]], case[[2]], "2000-01-01", "2000-12-31"),
      out
    )
  }
})

test_that("download_data handles argument errors and legacy inputs", {
  out <- tibble::tibble(ok = TRUE)

  local_mocked_bindings(
    check_supported_domain = function(domain) NULL,
    is_legacy_type = function(x) identical(x, "legacy"),
    parse_type_to_domain_dataset = function(type) {
      list(domain = "fred", dataset = NULL)
    },
    download_data_fred = function(...) out
  )

  expect_warning(
    expect_identical(download_data(type = "legacy"), out),
    "deprecated"
  )
  expect_warning(
    expect_identical(download_data("legacy"), out),
    "deprecated"
  )
})

test_that("is_legacy_type detects legacy types but not simple domains", {
  local_mocked_bindings(
    list_supported_datasets_ff = function() {
      tibble::tibble(type = "ff", dataset_name = "ff_data")
    },
    list_supported_datasets_ff_legacy = function() {
      tibble::tibble(type = "ff_old", dataset_name = "ff_old_data")
    },
    list_supported_datasets_q = function() {
      tibble::tibble(type = "q", dataset_name = "q.csv")
    },
    list_supported_datasets_macro_predictors = function() {
      tibble::tibble(type = "macro_predictors_monthly")
    },
    list_supported_datasets_wrds = function() {
      tibble::tibble(type = "wrds_crsp")
    },
    list_supported_datasets_other = function() {
      tibble::tibble(
        type = c("fred", "other"),
        domain = c("fred", "other")
      )
    }
  )

  expect_false(is_legacy_type("fred"))
  expect_true(is_legacy_type("ff"))
  expect_true(is_legacy_type("other"))
  expect_false(is_legacy_type("missing"))
})

test_that("parse_type_to_domain_dataset parses all legacy formats", {
  local_mocked_bindings(
    list_supported_datasets_ff = function() {
      tibble::tibble(type = "ff", dataset_name = "ff_data")
    },
    list_supported_datasets_ff_legacy = function() {
      tibble::tibble(type = "ff_old", dataset_name = "ff_old_data")
    },
    list_supported_datasets_q = function() {
      tibble::tibble(type = "q", dataset_name = "q.csv")
    },
    list_supported_datasets_macro_predictors = function() {
      tibble::tibble(type = "macro_predictors_monthly")
    },
    list_supported_datasets_wrds = function() {
      tibble::tibble(type = "wrds_crsp")
    }
  )

  expect_equal(
    parse_type_to_domain_dataset("ff"),
    list(domain = "factors_ff", dataset = "ff_data")
  )
  expect_equal(
    parse_type_to_domain_dataset("q"),
    list(domain = "factors_q", dataset = "q")
  )
  expect_equal(
    parse_type_to_domain_dataset("macro_predictors_monthly"),
    list(domain = "macro_predictors", dataset = "monthly")
  )
  expect_equal(
    parse_type_to_domain_dataset("wrds_crsp"),
    list(domain = "wrds", dataset = "crsp")
  )
  expect_equal(
    parse_type_to_domain_dataset("hf_sp500"),
    list(domain = "tidyfinance", dataset = "sp500")
  )
  expect_equal(
    parse_type_to_domain_dataset("osap"),
    list(domain = "osap", dataset = NULL)
  )
})

test_that("check_supported_domain accepts known domains and rejects others", {
  expect_no_error(check_supported_domain("famafrench"))
  expect_no_error(check_supported_domain("tidyfinance"))
})

test_that("download_data requires domain", {
  local_mocked_bindings(
    is_legacy_type = function(x) FALSE
  )

  expect_error(
    download_data(),
    "Argument `domain` is required"
  )
})

test_that("parse_type_to_domain_dataset errors on unknown type", {
  local_mocked_bindings(
    list_supported_datasets_ff = function() {
      tibble::tibble(type = character())
    },
    list_supported_datasets_ff_legacy = function() {
      tibble::tibble(type = character())
    },
    list_supported_datasets_q = function() {
      tibble::tibble(
        type = character(),
        dataset_name = character()
      )
    },
    list_supported_datasets_macro_predictors = function() {
      tibble::tibble(type = character())
    },
    list_supported_datasets_wrds = function() {
      tibble::tibble(type = character())
    }
  )

  expect_error(
    parse_type_to_domain_dataset("unknown"),
    "Cannot parse legacy type: \"unknown\""
  )
})

test_that("check_supported_domain errors for unsupported domain", {
  expect_error(
    check_supported_domain("unknown"),
    'Unsupported domain: "unknown"'
  )
})
