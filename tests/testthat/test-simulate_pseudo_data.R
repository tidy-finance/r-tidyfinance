test_that("simulate_pseudo_data validates the dataset argument", {
  expect_error(
    simulate_pseudo_data(),
    "Argument `dataset` is required"
  )
  expect_error(
    simulate_pseudo_data("bad"),
    "Unsupported synthetic dataset"
  )
})

test_that("simulate_pseudo_data dispatches to per-dataset generators", {
  local_mocked_bindings(
    download_data_pseudo_crsp = function(dataset, start_date, end_date, ...) {
      list("crsp", dataset, list(...))
    },
    download_data_pseudo_compustat = function(
      dataset,
      start_date,
      end_date,
      ...
    ) {
      list("compustat", dataset, list(...))
    },
    download_data_pseudo_ccm_links = function(...) {
      list("ccm", list(...))
    }
  )

  expect_identical(
    simulate_pseudo_data("crsp_monthly", n_assets = 5)[[1]],
    "crsp"
  )
  expect_identical(
    simulate_pseudo_data("compustat_annual", n_assets = 5)[[1]],
    "compustat"
  )
  expect_identical(
    simulate_pseudo_data("ccm_links", n_assets = 5)[[1]],
    "ccm"
  )
})

test_that("download_data routes pseudo to simulate_pseudo_data", {
  local_mocked_bindings(
    simulate_pseudo_data = function(dataset, start_date, end_date, ...) {
      list(dataset = dataset, start_date = start_date, end_date = end_date)
    }
  )

  out <- download_data(
    domain = "pseudo",
    dataset = "crsp_monthly",
    start_date = "2020-01-01",
    end_date = "2020-06-30"
  )

  expect_identical(out$dataset, "crsp_monthly")
  expect_identical(out$start_date, "2020-01-01")
  expect_identical(out$end_date, "2020-06-30")
})

test_that("synthetic CRSP monthly returns expected schema", {
  crsp <- download_data_pseudo_crsp(
    dataset = "crsp_monthly",
    start_date = "2020-01-01",
    end_date = "2020-06-30",
    n_assets = 5,
    seed = 1234
  )

  expected_cols <- c(
    "permno",
    "date",
    "calculation_date",
    "ret",
    "shrout",
    "prc",
    "primaryexch",
    "siccd",
    "listing_age",
    "mktcap",
    "mktcap_lag",
    "exchange",
    "industry",
    "ret_excess"
  )
  expect_true(all(expected_cols %in% names(crsp)))
  expect_false("gvkey" %in% names(crsp))
  expect_equal(nrow(crsp), 5 * 6)
  expect_s3_class(crsp$date, "Date")
})

test_that("add_ccm_links = TRUE appends gvkey from the shared universe", {
  crsp <- download_data_pseudo_crsp(
    dataset = "crsp_monthly",
    start_date = "2020-01-01",
    end_date = "2020-06-30",
    add_ccm_links = TRUE,
    n_assets = 5,
    seed = 1234
  )
  ccm <- download_data_pseudo_ccm_links(n_assets = 5, seed = 1234)

  expect_true("gvkey" %in% names(crsp))
  expect_setequal(unique(crsp$gvkey), ccm$gvkey)
})

test_that("synthetic Compustat annual returns expected schema", {
  comp <- download_data_pseudo_compustat(
    dataset = "compustat_annual",
    start_date = "2020-01-01",
    end_date = "2024-12-31",
    n_assets = 5,
    seed = 1234
  )

  expected_cols <- c(
    "gvkey",
    "date",
    "datadate",
    "at",
    "ib",
    "be",
    "op",
    "inv",
    "at_lag"
  )
  expect_true(all(expected_cols %in% names(comp)))
  expect_equal(nrow(comp), 5 * 5)
  expect_s3_class(comp$datadate, "Date")
})

test_that("additional_columns are honored on Compustat", {
  comp <- download_data_pseudo_compustat(
    dataset = "compustat_annual",
    start_date = "2020-01-01",
    end_date = "2022-12-31",
    additional_columns = c("ib", "ni"),
    n_assets = 3,
    seed = 1234
  )
  expect_true("ni" %in% names(comp))
  expect_true("ib" %in% names(comp))
})

test_that("synthetic CCM links cover the full identifier universe", {
  ccm <- download_data_pseudo_ccm_links(n_assets = 10, seed = 1234)
  expect_equal(nrow(ccm), 10)
  expect_named(ccm, c("permno", "gvkey", "linkdt", "linkenddt"))
})

test_that("output is deterministic in (seed, n_assets) across datasets", {
  a1 <- download_data_pseudo_crsp(
    "crsp_monthly",
    "2020-01-01",
    "2020-03-31",
    n_assets = 5,
    seed = 1234
  )
  a2 <- download_data_pseudo_crsp(
    "crsp_monthly",
    "2020-01-01",
    "2020-03-31",
    n_assets = 5,
    seed = 1234
  )
  expect_identical(a1, a2)

  b1 <- download_data_pseudo_crsp(
    "crsp_monthly",
    "2020-01-01",
    "2020-03-31",
    n_assets = 5,
    seed = 42
  )
  expect_false(identical(a1$ret, b1$ret))
})

test_that("identifier universe matches across CRSP, Compustat, and CCM", {
  crsp <- download_data_pseudo_crsp(
    "crsp_monthly",
    "2020-01-01",
    "2020-06-30",
    add_ccm_links = TRUE,
    n_assets = 7,
    seed = 1234
  )
  comp <- download_data_pseudo_compustat(
    "compustat_annual",
    "2020-01-01",
    "2024-12-31",
    n_assets = 7,
    seed = 1234
  )
  ccm <- download_data_pseudo_ccm_links(n_assets = 7, seed = 1234)

  expect_setequal(unique(crsp$gvkey), ccm$gvkey)
  expect_setequal(unique(crsp$gvkey), unique(comp$gvkey))
  expect_setequal(unique(crsp$permno), ccm$permno)
})

test_that("synthetic CRSP daily returns expected schema (weekdays only)", {
  crsp_daily <- download_data_pseudo_crsp(
    dataset = "crsp_daily",
    start_date = "2020-01-01",
    end_date = "2020-01-31",
    n_assets = 4,
    seed = 1234
  )

  expect_named(crsp_daily, c("permno", "date", "ret", "ret_excess"))
  expect_s3_class(crsp_daily$date, "Date")
  # 2020-01 has 23 weekdays
  expect_equal(nrow(crsp_daily), 4 * 23)
  # No Saturdays or Sundays
  expect_true(all(lubridate::wday(crsp_daily$date, week_start = 1) <= 5L))
})

test_that("synthetic Compustat quarterly returns expected schema", {
  compq <- download_data_pseudo_compustat(
    dataset = "compustat_quarterly",
    start_date = "2020-01-01",
    end_date = "2020-12-31",
    n_assets = 5,
    seed = 1234
  )

  expect_named(compq, c("gvkey", "date", "datadate", "atq", "ceqq"))
  expect_equal(nrow(compq), 5 * 4)
  expect_s3_class(compq$datadate, "Date")
  # datadates are quarter-end days
  expect_true(all(
    format(compq$datadate, "%m-%d") %in%
      c("03-31", "06-30", "09-30", "12-31")
  ))
})

test_that("additional_columns and add_ccm_links work on daily and quarterly", {
  crsp_daily <- download_data_pseudo_crsp(
    "crsp_daily",
    "2020-01-01",
    "2020-01-15",
    additional_columns = "vol",
    add_ccm_links = TRUE,
    n_assets = 3,
    seed = 1234
  )
  expect_true(all(c("vol", "gvkey") %in% names(crsp_daily)))

  compq <- download_data_pseudo_compustat(
    "compustat_quarterly",
    "2020-01-01",
    "2020-06-30",
    additional_columns = c("saleq", "niq"),
    n_assets = 3,
    seed = 1234
  )
  expect_true(all(c("saleq", "niq") %in% names(compq)))
})

test_that("simulate_pseudo_data emits the synthetic-data notice", {
  expect_message(
    simulate_pseudo_data(
      "crsp_monthly",
      start_date = "2020-01-01",
      end_date = "2020-03-31",
      n_assets = 2
    ),
    "synthetic"
  )
})

test_that("per-dataset CRSP generator validates its dataset argument", {
  expect_error(download_data_pseudo_crsp(), "Argument `dataset` is required")
  expect_error(
    download_data_pseudo_crsp("compustat_annual"),
    "Unsupported CRSP dataset"
  )
})

test_that("per-dataset Compustat generator validates its dataset argument", {
  expect_error(
    download_data_pseudo_compustat(),
    "Argument `dataset` is required"
  )
  expect_error(
    download_data_pseudo_compustat("crsp_monthly"),
    "Unsupported Compustat dataset"
  )
})

test_that("additional_columns are honored on monthly CRSP", {
  crsp <- download_data_pseudo_crsp(
    "crsp_monthly", "2020-01-01", "2020-03-31",
    additional_columns = "vol",
    n_assets = 3, seed = 1234
  )
  expect_true("vol" %in% names(crsp))
})

test_that("simulate_pseudo_identifiers rejects invalid n_assets", {
  expect_error(
    download_data_pseudo_ccm_links(n_assets = 0L),
    "must be a single positive integer"
  )
  expect_error(
    download_data_pseudo_ccm_links(n_assets = NA_integer_),
    "must be a single positive integer"
  )
})
