test_that("CRSP dataset validation rejects unsupported values", {
  expect_error(
    check_supported_dataset_wrds_crsp("bad"),
    "Unsupported CRSP dataset"
  )

  expect_no_error(check_supported_dataset_wrds_crsp("crsp_monthly"))
  expect_no_error(check_supported_dataset_wrds_crsp("crsp_daily"))
})

test_that("CRSP argument validation covers required inputs", {
  local_mocked_bindings(
    get_wrds_connection = function() "con"
  )

  expect_error(
    download_data_wrds_crsp(),
    "dataset"
  )

  expect_error(
    download_data_wrds_crsp("crsp_monthly", batch_size = 0),
    "batch_size"
  )

  expect_error(
    download_data_wrds_crsp("crsp_monthly", version = "bad"),
    "version"
  )

  expect_error(
    download_data_wrds_crsp("bad"),
    "Unsupported CRSP dataset"
  )
})

test_that("deprecated type inputs are translated to dataset", {
  local_mocked_bindings(
    is_legacy_type_wrds = function(dataset) identical(dataset, "wrds_bad"),
    check_supported_dataset_wrds_crsp = function(dataset) {
      expect_equal(dataset, "crsp_monthly")
      cli::cli_abort("stop after translation")
    }
  )

  expect_warning(
    expect_error(
      download_data_wrds_crsp(type = "wrds_crsp_monthly"),
      "stop after translation"
    ),
    "deprecated"
  )

  local_mocked_bindings(
    is_legacy_type_wrds = function(dataset) identical(dataset, "wrds_bad"),
    check_supported_dataset_wrds_crsp = function(dataset) {
      expect_equal(dataset, "bad")
      cli::cli_abort("stop after translation")
    }
  )

  expect_warning(
    expect_error(
      download_data_wrds_crsp(dataset = "wrds_bad"),
      "stop after translation"
    ),
    "deprecated"
  )
})

crsp_dates <- function() {
  list(
    start_date = as.Date("2001-01-01"),
    end_date = as.Date("2020-12-31")
  )
}

risk_free_monthly <- function(start_date, end_date) {
  tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-02-01")),
    risk_free = c(0.01, 0.01)
  )
}

risk_free_daily <- function(start_date, end_date, frequency = NULL) {
  expect_equal(frequency, "daily")

  tibble::tibble(
    date = as.Date(c(
      "2001-01-15",
      "2001-02-15",
      "2002-02-15",
      "2004-02-15",
      "2020-01-02",
      "2020-01-03"
    )),
    risk_free = 0.01
  )
}

mock_crsp_tbl <- function(con, from) {
  table_name <- as.character(from)

  switch(
    table_name,
    "crsp.msf" = tibble::tibble(
      permno = c(1L, 1L, 2L, 3L),
      date = as.Date(c(
        "2020-01-31",
        "2020-02-29",
        "2020-01-31",
        "2020-01-31"
      )),
      ret = c(0.10, 0.20, 0.30, 0.40),
      shrout = c(10, 20, 30, 40),
      altprc = c(5, -6, 7, 8),
      cfacpr = c(1, 2, 1, 0),
      exchcd = c(1L, 2L, 3L, 9L),
      siccd = c(100L, 1200L, 2500L, NA_integer_),
      mthvol = c(11, 12, 13, 14)
    ),
    "crsp.msenames" = tibble::tibble(
      permno = c(1L, 2L, 3L),
      namedt = as.Date(rep("2000-01-01", 3)),
      nameendt = as.Date(rep("2020-12-31", 3)),
      shrcd = c(10L, 11L, 10L),
      exchcd = c(3L, 2L, 1L)
    ),
    "crsp.msedelist" = tibble::tibble(
      permno = c(2L, 3L),
      dlstdt = as.Date(c("2020-01-31", "2020-01-31")),
      dlret = c(NA_real_, -0.50),
      dlstcd = c(552L, 100L)
    ),
    "crsp.msf_v2" = tibble::tibble(
      permno = c(1L, 1L, 2L, 3L),
      mthcaldt = as.Date(c(
        "2020-01-31",
        "2020-02-29",
        "2020-01-31",
        "2020-01-31"
      )),
      mthret = c(0.10, 0.20, 0.30, NA_real_),
      shrout = c(10, 20, 0, 40),
      mthprc = c(5, 6, 7, 8),
      primaryexch = c("N", "A", "Q", "Z"),
      siccd = c(5100L, 5300L, 6500L, 9500L),
      mthvol = c(11, 12, 13, 14)
    ),
    "crsp.stksecurityinfohist" = tibble::tibble(
      permno = c(1L, 2L, 3L),
      secinfostartdt = as.Date(rep("2000-01-01", 3)),
      secinfoenddt = as.Date(rep("2020-12-31", 3)),
      sharetype = "NS",
      securitytype = "EQTY",
      securitysubtype = "COM",
      usincflg = "Y",
      issuertype = "CORP",
      primaryexch = c("Q", "A", "N"),
      conditionaltype = "RW",
      tradingstatusflg = "A",
      siccd = c(1L, 2L, 3L)
    ),
    "crsp.dsf" = tibble::tibble(
      permno = c(1L, 1L, 1L, 1L, 2L, 3L),
      date = as.Date(c(
        "2001-01-15",
        "2001-02-15",
        "2002-02-15",
        "2004-02-15",
        "2020-01-02",
        "2020-01-03"
      )),
      ret = c(0.10, 0.20, 0.30, 0.40, 0.50, NA_real_),
      prc = c(10, 10, 10, 10, 0, 8),
      vol = c(20, 18, 16, -99, 5, 6),
      cfacpr = c(1, 1, 1, 1, 1, 1),
      exchcd = c(3L, 3L, 3L, 3L, 1L, 2L),
      extra = 1:6
    ),
    "crsp.dsf_v2" = tibble::tibble(
      permno = c(1L, 1L, 1L, 1L, 2L, 3L),
      dlycaldt = as.Date(c(
        "2001-01-15",
        "2001-02-15",
        "2002-02-15",
        "2004-02-15",
        "2020-01-02",
        "2020-01-03"
      )),
      dlyret = c(0.10, 0.20, 0.30, 0.40, 0.50, NA_real_),
      dlyprc = c(10, 10, 10, 10, 0, 8),
      dlyvol = c(20, 18, 16, -99, 5, 6),
      dlyfacprc = c(1, 1, 1, 1, 1, 1),
      primaryexch = c("Q", "Q", "Q", "Q", "N", "A"),
      extra = 1:6
    ),
    cli::cli_abort("Unexpected table: {table_name}")
  )
}

with_crsp_mocks <- function(code) {
  local_mocked_bindings(
    validate_dates = function(start_date, end_date, use_default_range) {
      expect_true(use_default_range)
      crsp_dates()
    },
    get_wrds_connection = function() "con",
    disconnect_connection = function(con) invisible(TRUE),
    download_data_risk_free = function(start_date, end_date, ...) {
      dots <- list(...)
      if (identical(dots$frequency, "daily")) {
        risk_free_daily(start_date, end_date, dots$frequency)
      } else {
        risk_free_monthly(start_date, end_date)
      }
    },
    tbl = mock_crsp_tbl,
    .env = globalenv()
  )

  force(code)
}

test_that("monthly CRSP v1 is processed", {
  with_crsp_mocks({
    out <- download_data_wrds_crsp(
      dataset = "crsp_monthly",
      version = "v1",
      additional_columns = "mthvol"
    )
  })

  expect_s3_class(out, "data.frame")
  expect_equal(out$ret_excess[out$permno == 1 & out$date == "2020-01-01"], 0.09)
  expect_equal(out$ret_adj[out$permno == 2], -0.30)
  expect_equal(out$ret_adj[out$permno == 3], -0.50)
  expect_true("mthvol" %in% names(out))
  expect_true(all(c("mktcap", "mktcap_lag", "listing_age") %in% names(out)))
  expect_true(all(c("exchange", "industry", "prc_adj") %in% names(out)))
})

test_that("monthly CRSP v2 is processed", {
  with_crsp_mocks({
    out <- download_data_wrds_crsp(
      dataset = "crsp_monthly",
      version = "v2",
      additional_columns = "mthvol"
    )
  })

  expect_s3_class(out, "data.frame")
  expect_equal(out$ret_excess[out$permno == 1 & out$date == "2020-01-01"], 0.09)
  expect_equal(out$mktcap_lag[out$date == "2020-02-01"], 0.05)
  expect_true("mthvol" %in% names(out))
  expect_false(any(is.na(out$ret_excess)))
  expect_false(any(is.na(out$mktcap)))
})

test_that("daily CRSP v1 validates and adjusts volume", {
  with_crsp_mocks({
    expect_error(
      download_data_wrds_crsp(
        dataset = "crsp_daily",
        version = "v1",
        adjust_volume = TRUE,
        additional_columns = "prc"
      ),
      "adjust_volume = TRUE"
    )

    out <- download_data_wrds_crsp(
      dataset = "crsp_daily",
      version = "v1",
      adjust_volume = TRUE,
      additional_columns = c("prc", "vol", "cfacpr", "exchcd")
    )
  })

  expect_equal(out$ret_excess[out$date == "2001-01-15"], 0.09)
  expect_equal(out$vol_adj[out$date == "2001-01-15"], 10)
  expect_equal(out$vol_adj[out$date == "2001-02-15"], 10)
  expect_equal(out$vol_adj[out$date == "2002-02-15"], 10)
  expect_true(is.na(out$vol_adj[out$date == "2004-02-15"]))
  expect_true(is.na(out$prc_adj[out$permno == 2]))
})

test_that("daily CRSP v1 handles empty batches", {
  with_crsp_mocks({
    out <- download_data_wrds_crsp(
      dataset = "crsp_daily",
      version = "v1",
      batch_size = 1
    )
  })

  expect_s3_class(out, "data.frame")
  expect_false(any(is.na(out$ret)))
  expect_true(nrow(out) > 0)
})

test_that("daily CRSP v2 validates and adjusts volume", {
  with_crsp_mocks({
    expect_error(
      download_data_wrds_crsp(
        dataset = "crsp_daily",
        version = "v2",
        adjust_volume = TRUE,
        additional_columns = "dlyprc"
      ),
      "adjust_volume = TRUE"
    )

    out <- download_data_wrds_crsp(
      dataset = "crsp_daily",
      version = "v2",
      adjust_volume = TRUE,
      additional_columns = c(
        "dlyprc",
        "dlyvol",
        "dlyfacprc",
        "primaryexch"
      )
    )
  })

  expect_equal(out$ret_excess[out$date == "2001-01-15"], 0.09)
  expect_equal(out$vol_adj[out$date == "2001-01-15"], 10)
  expect_equal(out$vol_adj[out$date == "2001-02-15"], 10)
  expect_equal(out$vol_adj[out$date == "2002-02-15"], 10)
  expect_true(is.na(out$vol_adj[out$date == "2004-02-15"]))
  expect_true(is.na(out$prc_adj[out$permno == 2]))
  expect_false("dlyvol" %in% names(out))
})

test_that("daily CRSP v2 handles empty batches", {
  with_crsp_mocks({
    out <- download_data_wrds_crsp(
      dataset = "crsp_daily",
      version = "v2",
      batch_size = 1
    )
  })

  expect_s3_class(out, "data.frame")
  expect_false(any(is.na(out$ret)))
  expect_true(nrow(out) > 0)
})

test_that("CCM links are added when requested", {
  ccm_links <- tibble::tibble(
    permno = c(1L, 1L),
    gvkey = c("001", NA_character_),
    linkdt = as.Date(c("2019-01-01", "2019-01-01")),
    linkenddt = as.Date(c("2020-12-31", "2020-12-31"))
  )

  with_crsp_mocks({
    local_mocked_bindings(
      download_data_wrds_ccm_links = function() ccm_links,
      .env = globalenv()
    )

    out <- download_data_wrds_crsp(
      dataset = "crsp_monthly",
      version = "v2",
      add_ccm_links = TRUE
    )
  })

  expect_true("gvkey" %in% names(out))
  expect_equal(out$gvkey[out$permno == 1 & out$date == "2020-01-01"], "001")
  expect_true(all(is.na(out$gvkey[out$permno != 1])))
})
