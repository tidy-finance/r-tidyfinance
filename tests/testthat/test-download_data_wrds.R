test_that("WRDS helper functions validate inputs", {
  expect_true(is_legacy_type_wrds("wrds_crsp_monthly"))
  expect_false(is_legacy_type_wrds("crsp_monthly"))

  expect_null(check_supported_dataset_wrds("crsp_monthly"))

  expect_error(
    check_supported_dataset_wrds("bad"),
    "Unsupported WRDS dataset"
  )
})

test_that("download_data_wrds requires dataset", {
  expect_error(
    download_data_wrds(),
    "Argument `dataset` is required"
  )
})

test_that("download_data_wrds dispatches CRSP datasets", {
  local_mocked_bindings(
    download_data_wrds_crsp = function(dataset, start_date, end_date, ...) {
      list(dataset, start_date, end_date, list(...))
    }
  )

  out <- download_data_wrds(
    dataset = "crsp_monthly",
    start_date = "2020-01-01",
    end_date = "2020-12-31",
    permno = 1
  )

  expect_identical(out[[1]], "crsp_monthly")
  expect_identical(out[[2]], "2020-01-01")
  expect_identical(out[[3]], "2020-12-31")
  expect_identical(out[[4]]$permno, 1)
})

test_that("download_data_wrds dispatches Compustat datasets", {
  local_mocked_bindings(
    download_data_wrds_compustat = function(
      dataset,
      start_date,
      end_date,
      ...
    ) {
      list(dataset, start_date, end_date, list(...))
    }
  )

  out <- download_data_wrds(
    dataset = "compustat_annual",
    start_date = "2020-01-01",
    end_date = "2020-12-31",
    gvkey = "001690"
  )

  expect_identical(out[[1]], "compustat_annual")
  expect_identical(out[[2]], "2020-01-01")
  expect_identical(out[[3]], "2020-12-31")
  expect_identical(out[[4]]$gvkey, "001690")
})

test_that("download_data_wrds dispatches link and bond datasets", {
  local_mocked_bindings(
    download_data_wrds_ccm_links = function(...) list("ccm", list(...)),
    download_data_wrds_fisd = function(...) list("fisd", list(...)),
    download_data_wrds_trace_enhanced = function(start_date, end_date, ...) {
      list("trace", start_date, end_date, list(...))
    }
  )

  expect_identical(
    download_data_wrds("ccm_links", linktype = "LU"),
    list("ccm", list(linktype = "LU"))
  )

  expect_identical(
    download_data_wrds("fisd", issuer = "ABC"),
    list("fisd", list(issuer = "ABC"))
  )

  expect_identical(
    download_data_wrds("trace_enhanced", cusips = "00101JAH9"),
    list(
      "trace",
      NULL,
      NULL,
      list(cusips = "00101JAH9")
    )
  )
})

test_that("download_data_wrds handles deprecated type argument", {
  local_mocked_bindings(
    download_data_wrds_crsp = function(dataset, start_date, end_date, ...) {
      dataset
    }
  )

  expect_warning(
    out <- download_data_wrds(type = "wrds_crsp_monthly"),
    class = "lifecycle_warning_deprecated"
  )

  expect_identical(out, "crsp_monthly")
})

test_that("download_data_wrds handles legacy dataset values", {
  local_mocked_bindings(
    download_data_wrds_crsp = function(dataset, start_date, end_date, ...) {
      dataset
    }
  )

  expect_warning(
    out <- download_data_wrds("wrds_crsp_daily"),
    class = "lifecycle_warning_deprecated"
  )

  expect_identical(out, "crsp_daily")
})

test_that("download_data_wrds rejects unsupported datasets", {
  expect_error(
    download_data_wrds("bad"),
    "Unsupported WRDS dataset"
  )
})

test_that("download_data_wrds final fallback errors", {
  local_mocked_bindings(
    check_supported_dataset_wrds = function(dataset) NULL
  )

  expect_error(
    download_data_wrds("other"),
    "Unsupported WRDS dataset"
  )
})
