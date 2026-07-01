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
    download_data_jkp = function(...) out,
    download_data_pastor_stambaugh = function(...) out,
    download_data_stambaugh_yuan = function(...) out,
    download_data_risk_free = function(...) out,
    download_data_huggingface = function(...) out
  )

  cases <- list(
    list("Fama-French", "d"),
    list("Global Q", "d"),
    list("Goyal-Welch", "d"),
    list("WRDS", "d"),
    list("Index Constituents", NULL),
    list("FRED", NULL),
    list("Stock Prices", NULL),
    list("Open Source Asset Pricing", NULL),
    list("Global Factor Data", NULL),
    list("Pastor-Stambaugh", NULL),
    list("Stambaugh-Yuan", NULL),
    list("Tidy Finance", "risk_free"),
    list("Tidy Finance", "factor_library")
  )

  for (case in cases) {
    expect_identical(
      download_data(case[[1]], case[[2]], "2000-01-01", "2000-12-31"),
      out
    )
  }
})

test_that("download_data forwards the dataset to Global Factor Data", {
  captured <- NULL

  local_mocked_bindings(
    check_supported_domain = function(domain) NULL,
    download_data_jkp = function(dataset, ...) {
      captured <<- dataset
      tibble::tibble(ok = TRUE)
    }
  )

  # A NULL dataset defaults to "factors".
  download_data("Global Factor Data")
  expect_identical(captured, "factors")

  # An explicit dataset is forwarded unchanged.
  download_data("Global Factor Data", "industry")
  expect_identical(captured, "industry")
})

test_that("download_data forwards the dataset to Stambaugh-Yuan", {
  captured <- NULL

  local_mocked_bindings(
    check_supported_domain = function(domain) NULL,
    download_data_stambaugh_yuan = function(dataset, ...) {
      captured <<- dataset
      tibble::tibble(ok = TRUE)
    }
  )

  # A NULL dataset defaults to "monthly".
  download_data("Stambaugh-Yuan")
  expect_identical(captured, "monthly")

  # An explicit dataset is forwarded unchanged.
  download_data("Stambaugh-Yuan", "daily")
  expect_identical(captured, "daily")
})

test_that("download_data handles argument errors and legacy inputs", {
  out <- tibble::tibble(ok = TRUE)

  local_mocked_bindings(
    check_supported_domain = function(domain) NULL,
    is_legacy_type = function(x) identical(x, "legacy"),
    parse_type_to_domain_dataset = function(type) {
      list(domain = "FRED", dataset = NULL)
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
    list(domain = "Fama-French", dataset = "ff_data")
  )
  expect_equal(
    parse_type_to_domain_dataset("q"),
    list(domain = "Global Q", dataset = "q")
  )
  expect_equal(
    parse_type_to_domain_dataset("macro_predictors_monthly"),
    list(domain = "Goyal-Welch", dataset = "monthly")
  )
  expect_equal(
    parse_type_to_domain_dataset("wrds_crsp"),
    list(domain = "WRDS", dataset = "crsp")
  )
  expect_equal(
    parse_type_to_domain_dataset("hf_sp500"),
    list(domain = "Tidy Finance", dataset = "sp500")
  )
  expect_equal(
    parse_type_to_domain_dataset("osap"),
    list(domain = "Open Source Asset Pricing", dataset = NULL)
  )
})

test_that("resolve_domain_alias maps deprecated names and warns", {
  expect_warning(
    expect_identical(resolve_domain_alias("famafrench"), "Fama-French"),
    "deprecated"
  )
  expect_warning(
    expect_identical(resolve_domain_alias("pseudo"), "Pseudo Data"),
    "deprecated"
  )
  expect_warning(
    expect_identical(resolve_domain_alias("tidyfinance"), "Tidy Finance"),
    "deprecated"
  )
  # Canonical names pass through unchanged without a warning
  expect_identical(resolve_domain_alias("WRDS"), "WRDS")
})

test_that("check_supported_domain accepts known domains and rejects others", {
  expect_no_error(check_supported_domain("Fama-French"))
  expect_no_error(check_supported_domain("Tidy Finance"))
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
