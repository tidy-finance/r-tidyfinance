test_that("determine_frequency_ff: daily, weekly, and monthly paths", {
  expect_equal(
    determine_frequency_ff("Factors [Daily]"),
    "daily"
  )
  expect_equal(
    determine_frequency_ff("Factors [Weekly]"),
    "weekly"
  )
  expect_equal(
    determine_frequency_ff("Fama/French 3 Factors"),
    "monthly"
  )
})

test_that("determine_frequency_q: all five frequency paths", {
  expect_equal(
    determine_frequency_q("q5_factors_daily_2023"),
    "daily"
  )
  expect_equal(
    determine_frequency_q("q5_factors_weekly_2023"),
    "weekly"
  )
  expect_equal(
    determine_frequency_q("q5_factors_monthly_2023"),
    "monthly"
  )
  expect_equal(
    determine_frequency_q("q5_factors_quarterly_2023"),
    "quarterly"
  )
  expect_equal(
    determine_frequency_q("q5_factors_annual_2023"),
    "annual"
  )
})

test_that("determine_frequency_q: aborts on unknown frequency", {
  expect_error(
    determine_frequency_q("q5_factors_2023"),
    "Cannot determine frequency"
  )
})

test_that("check_supported_dataset_ff: no error for known dataset", {
  local_mocked_bindings(
    list_supported_datasets_ff = function() {
      tibble::tibble(
        dataset_name = "FF 3 Factors",
        type = NA_character_,
        file_url = "ftp/dummy_CSV.zip"
      )
    },
    list_supported_datasets_ff_legacy = function() {
      tibble::tibble(
        dataset_name = character(),
        type = character(),
        file_url = character()
      )
    }
  )
  expect_no_error(check_supported_dataset_ff("FF 3 Factors"))
})

test_that("check_supported_dataset_ff: aborts for unknown dataset", {
  local_mocked_bindings(
    list_supported_datasets_ff = function() {
      tibble::tibble(
        dataset_name = "FF 3 Factors",
        type = NA_character_,
        file_url = "ftp/dummy_CSV.zip"
      )
    },
    list_supported_datasets_ff_legacy = function() {
      tibble::tibble(
        dataset_name = character(),
        type = character(),
        file_url = character()
      )
    }
  )
  expect_error(
    check_supported_dataset_ff("Unknown"),
    "Unsupported Fama-French dataset"
  )
})

test_that("check_supported_dataset_q: no error for known dataset", {
  local_mocked_bindings(
    list_supported_datasets_q = function() {
      tibble::tibble(
        dataset_name = "q5_monthly",
        type = NA_character_
      )
    }
  )
  expect_no_error(check_supported_dataset_q("q5_monthly"))
})

test_that("check_supported_dataset_q: aborts for unknown dataset", {
  local_mocked_bindings(
    list_supported_datasets_q = function() {
      tibble::tibble(
        dataset_name = "q5_monthly",
        type = NA_character_
      )
    }
  )
  expect_error(
    check_supported_dataset_q("unknown"),
    "Unsupported Global Q dataset"
  )
})

test_that("is_legacy_type_ff: TRUE for legacy type, FALSE otherwise", {
  local_mocked_bindings(
    list_supported_datasets_ff = function() {
      tibble::tibble(
        dataset_name = "x",
        type = "three_factors"
      )
    },
    list_supported_datasets_ff_legacy = function() {
      tibble::tibble(
        dataset_name = "y",
        type = "another_legacy"
      )
    }
  )
  expect_true(is_legacy_type_ff("three_factors"))
  expect_false(is_legacy_type_ff("not_a_legacy_type"))
})

test_that("is_legacy_type_q: TRUE for legacy type, FALSE otherwise", {
  local_mocked_bindings(
    list_supported_datasets_q = function() {
      tibble::tibble(dataset_name = "x", type = "q_monthly")
    }
  )
  expect_true(is_legacy_type_q("q_monthly"))
  expect_false(is_legacy_type_q("not_a_legacy_type"))
})

test_that("download_data_factors_ff: aborts when dataset is NULL", {
  expect_error(
    download_data_factors_ff(NULL),
    "dataset.*required"
  )
})

test_that("download_data_factors_ff: deprecated `type` warns", {
  local_mocked_bindings(
    parse_type_to_domain_dataset = function(x) {
      list(dataset = "FF 3 Factors")
    },
    is_legacy_type_ff = function(x) FALSE,
    check_supported_dataset_ff = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) {
      tibble::tibble(date = as.Date(character()))
    }
  )
  expect_warning(
    suppressMessages(
      download_data_factors_ff(type = "three_factors")
    ),
    "deprecated"
  )
})

test_that("download_data_factors_ff: legacy dataset arg warns", {
  local_mocked_bindings(
    is_legacy_type_ff = function(x) TRUE,
    parse_type_to_domain_dataset = function(x) {
      list(dataset = "FF 3 Factors")
    },
    check_supported_dataset_ff = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) {
      tibble::tibble(date = as.Date(character()))
    }
  )
  expect_warning(
    suppressMessages(
      download_data_factors_ff("three_factors")
    ),
    "deprecated"
  )
})

test_that("download_data_factors_ff: empty tibble on download fail", {
  local_mocked_bindings(
    is_legacy_type_ff = function(x) FALSE,
    check_supported_dataset_ff = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) {
      tibble::tibble(date = as.Date(character()))
    }
  )
  expect_message(
    result <- download_data_factors_ff("FF 3 Factors"),
    "empty data set"
  )
  expect_equal(nrow(result), 0)
})

test_that("download_data_factors_ff: monthly path with date filter", {
  raw_df <- tibble::tibble(
    date = c("202001", "202002"),
    `Mkt-RF` = c(1.0, 2.0),
    RF = c(0.1, 0.1)
  )
  local_mocked_bindings(
    is_legacy_type_ff = function(x) FALSE,
    check_supported_dataset_ff = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(
        start_date = as.Date("2020-01-01"),
        end_date = as.Date("2020-01-31")
      )
    },
    handle_download_error = function(fn, ...) raw_df,
    determine_frequency_ff = function(x) "monthly"
  )
  result <- download_data_factors_ff(
    "FF 3 Factors",
    "2020-01-01",
    "2020-01-31"
  )
  expect_equal(nrow(result), 1)
  expect_named(result, c("date", "mkt_excess", "risk_free"))
})

test_that("download_data_factors_ff: threads registry url to downloader", {
  captured_url <- NULL
  local_mocked_bindings(
    is_legacy_type_ff = function(x) FALSE,
    determine_frequency_ff = function(x) "monthly",
    download_french_data_factors = function(file_url, ...) {
      captured_url <<- file_url
      tibble::tibble(date = "202001", `Mkt-RF` = 1.0, RF = 0.1)
    }
  )
  suppressMessages(
    download_data_factors_ff("Fama/French 3 Factors")
  )
  expect_equal(captured_url, "ftp/F-F_Research_Data_Factors_CSV.zip")
})

test_that("download_data_factors_ff: daily path, no date filter", {
  raw_df <- tibble::tibble(
    date = c("20200101", "20200102"),
    `Mkt-RF` = c(1.0, 2.0),
    RF = c(0.1, 0.1)
  )
  local_mocked_bindings(
    is_legacy_type_ff = function(x) FALSE,
    check_supported_dataset_ff = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) raw_df,
    determine_frequency_ff = function(x) "daily"
  )
  result <- download_data_factors_ff("Factors [Daily]")
  expect_equal(nrow(result), 2)
})

test_that("download_data_factors_ff: aborts on unknown frequency", {
  raw_df <- tibble::tibble(
    date = "20200101",
    `Mkt-RF` = 1.0,
    RF = 0.1
  )
  local_mocked_bindings(
    is_legacy_type_ff = function(x) FALSE,
    check_supported_dataset_ff = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) raw_df,
    determine_frequency_ff = function(x) "unknown"
  )
  expect_error(
    download_data_factors_ff("some_dataset"),
    "neither daily, weekly, nor monthly"
  )
})

test_that("download_data_factors_q: download lambda reads CSV", {
  mock_csv <- data.frame(
    year = c(2020L, 2020L),
    month = c(1L, 2L),
    R_F = c(0.1, 0.1),
    R_MKT = c(1.0, 2.0)
  )
  local_mocked_bindings(
    is_legacy_type_q = function(x) FALSE,
    check_supported_dataset_q = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    determine_frequency_q = function(x) "monthly"
  )
  local_mocked_bindings(
    read.csv = function(file, ...) mock_csv,
    .package = "utils"
  )
  result <- download_data_factors_q("q5_factors_monthly_2024")
  expect_equal(nrow(result), 2)
  expect_true("risk_free" %in% colnames(result))
})

test_that("download_data_factors_q: aborts when dataset is NULL", {
  expect_error(
    download_data_factors_q(NULL),
    "dataset.*required"
  )
})

test_that("download_data_factors_q: deprecated `type` warns", {
  local_mocked_bindings(
    parse_type_to_domain_dataset = function(x) {
      list(dataset = "q5_factors_monthly_2024")
    },
    is_legacy_type_q = function(x) FALSE,
    check_supported_dataset_q = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) tibble::tibble()
  )
  expect_warning(
    suppressMessages(
      download_data_factors_q(type = "q_monthly")
    ),
    "deprecated"
  )
})

test_that("download_data_factors_q: legacy dataset arg warns", {
  local_mocked_bindings(
    is_legacy_type_q = function(x) TRUE,
    parse_type_to_domain_dataset = function(x) {
      list(dataset = "q5_factors_monthly_2024")
    },
    check_supported_dataset_q = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) tibble::tibble()
  )
  expect_warning(
    suppressMessages(
      download_data_factors_q("q_monthly")
    ),
    "deprecated"
  )
})

test_that("download_data_factors_q: empty tibble on download fail", {
  local_mocked_bindings(
    is_legacy_type_q = function(x) FALSE,
    check_supported_dataset_q = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) tibble::tibble()
  )
  expect_message(
    result <- download_data_factors_q("q5_monthly"),
    "empty data set"
  )
  expect_equal(nrow(result), 0)
})

test_that("download_data_factors_q: monthly path with date filter", {
  raw_df <- tibble::tibble(
    year = c(2020L, 2020L),
    month = c(1L, 2L),
    R_F = c(0.1, 0.1),
    R_MKT = c(1.0, 2.0)
  )
  local_mocked_bindings(
    is_legacy_type_q = function(x) FALSE,
    check_supported_dataset_q = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(
        start_date = as.Date("2020-01-01"),
        end_date = as.Date("2020-01-31")
      )
    },
    handle_download_error = function(fn, ...) raw_df,
    determine_frequency_q = function(x) "monthly"
  )
  result <- download_data_factors_q(
    "q5_factors_monthly_2024",
    "2020-01-01",
    "2020-01-31"
  )
  expect_equal(nrow(result), 1)
  expect_true("risk_free" %in% colnames(result))
  expect_true("mkt_excess" %in% colnames(result))
})

test_that("download_data_factors_q: daily path, no date filter", {
  raw_df <- tibble::tibble(
    DATE = c("20200101", "20200102"),
    R_F = c(0.1, 0.1),
    R_MKT = c(1.0, 2.0)
  )
  local_mocked_bindings(
    is_legacy_type_q = function(x) FALSE,
    check_supported_dataset_q = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) raw_df,
    determine_frequency_q = function(x) "daily"
  )
  result <- download_data_factors_q("q5_factors_daily_2024")
  expect_equal(nrow(result), 2)
})

test_that("download_data_factors_q: annual path", {
  raw_df <- tibble::tibble(
    year = c(2020L, 2021L),
    R_F = c(0.1, 0.1),
    R_MKT = c(1.0, 2.0)
  )
  local_mocked_bindings(
    is_legacy_type_q = function(x) FALSE,
    check_supported_dataset_q = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) raw_df,
    determine_frequency_q = function(x) "annual"
  )
  result <- download_data_factors_q("q5_factors_annual_2024")
  expect_equal(nrow(result), 2)
})

test_that("download_data_factors_q: weekly path", {
  raw_df <- tibble::tibble(
    year = c(2020L, 2020L),
    month = c(1L, 1L),
    day = c(6L, 13L),
    R_F = c(0.1, 0.1),
    R_MKT = c(1.0, 2.0)
  )
  local_mocked_bindings(
    is_legacy_type_q = function(x) FALSE,
    check_supported_dataset_q = function(x) invisible(NULL),
    validate_dates = function(s, e) {
      list(start_date = NULL, end_date = NULL)
    },
    handle_download_error = function(fn, ...) raw_df,
    determine_frequency_q = function(x) "weekly"
  )
  result <- download_data_factors_q("q5_factors_weekly_2024")
  expect_equal(nrow(result), 2)
})

test_that("check_supported_dataset_ff: returns file_url for known dataset", {
  expect_equal(
    check_supported_dataset_ff("Fama/French 3 Factors"),
    "ftp/F-F_Research_Data_Factors_CSV.zip"
  )
})

test_that("parse_french_data_factors: keeps first table, names date column", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "This file was created by a test",
      "",
      ",Mkt-RF,SMB,RF",
      "202001,  1.00, -0.50, 0.10",
      "202002,  2.00,  0.50, 0.10",
      "",
      "  Annual Factors:",
      ",Mkt-RF,SMB,RF",
      "2020, 12.00, -1.00, 1.20"
    ),
    csv
  )

  result <- parse_french_data_factors(csv)

  expect_named(result, c("date", "Mkt-RF", "SMB", "RF"))
  expect_equal(nrow(result), 2)
  expect_equal(result$date, c(202001, 202002))
  expect_equal(result[["Mkt-RF"]], c(1.0, 2.0))
})

test_that("parse_french_data_factors: aborts when no table present", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Only header text", "no data here"), csv)
  expect_error(
    parse_french_data_factors(csv),
    "Could not locate a data table"
  )
})

test_that("parse_french_data_factors: parses positionally without header", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("202001,  1.00, -0.50", "202002,  2.00,  0.25"), csv)
  result <- parse_french_data_factors(csv)
  expect_equal(nrow(result), 2)
  expect_named(result, c("date", "V2", "V3"))
  expect_equal(result$date, c(202001, 202002))
})

test_that("parse_french_data_factors: blank line above data is headerless", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Some title text", "", "202001,  1.00, -0.50"), csv)
  result <- parse_french_data_factors(csv)
  expect_equal(nrow(result), 1)
  expect_named(result, c("date", "V2", "V3"))
})

test_that("parse_french_data_factors: prose line above data is headerless", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Average Value Weight Returns", "202001,1.0,0.1"), csv)
  result <- parse_french_data_factors(csv)
  expect_equal(nrow(result), 1)
  expect_named(result, c("date", "V2", "V3"))
})

test_that("parse_french_data_factors: accepts named first column", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c("Count,P5,P10", "202001,100,5.0", "202002,101,5.1"),
    csv
  )
  result <- parse_french_data_factors(csv)
  expect_equal(nrow(result), 2)
  expect_named(result, c("date", "P5", "P10"))
})

test_that("parse_french_data_factors: parses breakpoints file layout", {
  # Breakpoints files have a prose title, a blank line, then headerless data:
  # date, Count, and percentile columns.
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "This file was created using the CRSP database.",
      "",
      "192512,   488,        1.40,        2.38,     1319.00",
      "192601,   492,        1.38,        2.54,     1331.71"
    ),
    csv
  )
  result <- parse_french_data_factors(csv)
  expect_equal(nrow(result), 2)
  expect_equal(result$date, c(192512, 192601))
  expect_named(result, c("date", "V2", "V3", "V4", "V5"))
  # The integer-looking "Count" column must be coerced to double so the
  # downstream na_if(., -99.99) pipeline does not fail on a strict vctrs cast.
  expect_type(result$V2, "double")
})

test_that("parse_french_data_factors: drops merged annual rows", {
  csv <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      ",Mkt-RF,RF",
      "202001,1.0,0.1",
      "202002,2.0,0.1",
      "2020,12.0,1.2"
    ),
    csv
  )
  expect_warning(
    result <- parse_french_data_factors(csv),
    "different date-key width"
  )
  expect_equal(nrow(result), 2)
  expect_equal(result$date, c(202001, 202002))
})

test_that("download_french_data_factors: parses first table", {
  local_mocked_bindings(
    get_random_user_agent = function() "test-agent"
  )
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
        c(",Mkt-RF,RF", "202001,1.0,0.1", "202002,2.0,0.1"),
        file.path(exdir, "data.csv")
      )
      invisible(NULL)
    },
    .package = "utils"
  )

  result <- download_french_data_factors("ftp/Some_File_CSV.zip")

  expect_named(result, c("date", "Mkt-RF", "RF"))
  expect_equal(nrow(result), 2)
})

test_that("download_data_factors_ff: full path converts integer dates", {
  local_mocked_bindings(
    is_legacy_type_ff = function(x) FALSE,
    determine_frequency_ff = function(x) "monthly",
    get_random_user_agent = function() "test-agent"
  )
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
        c(",Mkt-RF,RF", "202001,1.0,0.1", "202002,2.0,0.1"),
        file.path(exdir, "data.csv")
      )
      invisible(NULL)
    },
    .package = "utils"
  )

  result <- suppressMessages(
    download_data_factors_ff("Fama/French 3 Factors")
  )

  expect_named(result, c("date", "mkt_excess", "risk_free"))
  expect_s3_class(result$date, "Date")
  expect_equal(result$date[1], as.Date("2020-01-01"))
  expect_equal(result$mkt_excess, c(0.01, 0.02))
})

test_that("is_breakpoints_ff: detects breakpoints datasets", {
  expect_true(is_breakpoints_ff("ME Breakpoints"))
  expect_true(is_breakpoints_ff("Prior (2-12) Return Breakpoints"))
  expect_false(is_breakpoints_ff("Fama/French 3 Factors"))
})

test_that("download_data_factors_ff: does not rescale breakpoints", {
  local_mocked_bindings(
    is_legacy_type_ff = function(x) FALSE,
    get_random_user_agent = function() "test-agent"
  )
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
        c(
          "This file was created using the CRSP database.",
          "",
          "202001,   488,   1.40,   1319.00",
          "202002,   492,   1.38,   1331.71"
        ),
        file.path(exdir, "data.csv")
      )
      invisible(NULL)
    },
    .package = "utils"
  )

  result <- suppressMessages(
    download_data_factors_ff("ME Breakpoints")
  )

  expect_s3_class(result$date, "Date")
  # Dollar levels and share counts must be returned unscaled (not divided by
  # 100), unlike percentage-return factor files.
  expect_equal(result$v2, c(488, 492))
  expect_equal(result$v4, c(1319.00, 1331.71))
})

test_that("download_french_data_factors: aborts when archive has no CSV", {
  local_mocked_bindings(
    get_random_user_agent = function() "test-agent"
  )
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
    download_french_data_factors("ftp/Empty_CSV.zip"),
    "No CSV file found"
  )
})
