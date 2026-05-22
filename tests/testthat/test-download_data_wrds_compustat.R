test_that("dataset is required and validated", {
  expect_error(
    download_data_wrds_compustat(),
    "dataset"
  )

  expect_error(
    download_data_wrds_compustat("bad"),
    "Unsupported Compustat dataset"
  )
})

test_that("annual data are downloaded and transformed", {
  funda <- tibble::tibble(
    gvkey = c("001", "001", "002"),
    datadate = as.Date(c("2020-12-31", "2019-12-31", "2020-12-31")),
    indfmt = "INDL",
    datafmt = "STD",
    consol = "C",
    seq = c(10, 9, NA),
    ceq = c(NA, NA, 20),
    at = c(100, 50, 30),
    lt = c(70, 40, 15),
    txditc = c(1, NA, NA),
    txdb = c(NA, 1, NA),
    itcb = c(NA, 1, NA),
    pstkrv = c(2, NA, NA),
    pstkl = c(NA, 1, NA),
    pstk = c(NA, NA, 1),
    capx = 1,
    oancf = 1,
    sale = c(20, 18, 25),
    cogs = c(5, 4, 6),
    xint = c(1, 1, 2),
    xsga = c(2, 2, 3),
    ib = c(3, 3, 4),
    curcd = c("USD", "USD", "CAD"),
    aodo = c(7, 8, 9)
  )

  local_mocked_bindings(
    get_wrds_connection = function() "con",
    disconnect_connection = function(con) NULL,
    tbl = function(src, from) funda
  )

  out <- download_data_wrds_compustat(
    "compustat_annual",
    "2019-01-01",
    "2020-12-31",
    additional_columns = "aodo",
    only_usd = TRUE
  )

  out_2020 <- dplyr::filter(out, datadate == as.Date("2020-12-31"))

  expect_equal(nrow(out), 2)
  expect_equal(out_2020$gvkey, "001")
  expect_equal(out_2020$be, 9)
  expect_equal(out_2020$op, 12 / 9)
  expect_equal(out_2020$inv, 1)
  expect_equal(out_2020$aodo, 7)
})

test_that("annual data handle pi and invalid lagged assets", {
  funda <- tibble::tibble(
    gvkey = c("001", "001"),
    datadate = as.Date(c("2019-12-31", "2020-12-31")),
    indfmt = "INDL",
    datafmt = "STD",
    consol = "C",
    seq = c(10, 12),
    ceq = c(NA, NA),
    at = c(0, 10),
    lt = c(1, 1),
    txditc = c(NA, NA),
    txdb = c(NA, NA),
    itcb = c(NA, NA),
    pstkrv = c(NA, NA),
    pstkl = c(NA, NA),
    pstk = c(NA, NA),
    capx = 1,
    oancf = 1,
    sale = c(2, 3),
    cogs = c(NA, NA),
    xint = c(NA, NA),
    xsga = c(NA, NA),
    ib = 1,
    curcd = "CAD"
  )

  local_mocked_bindings(
    get_wrds_connection = function() "con",
    disconnect_connection = function(con) NULL,
    tbl = function(src, from) funda
  )

  out <- download_data_wrds_compustat(
    "compustat_annual",
    "2019-01-01",
    "2020-12-31",
    additional_columns = "pi"
  )

  inv_2020 <- out$inv[out$datadate == as.Date("2020-12-31")]

  expect_equal(nrow(out), 2)
  expect_true("pi" %in% names(out))
  expect_true(is.na(inv_2020))
})

test_that("quarterly data are cleaned and filtered", {
  fundq <- tibble::tibble(
    gvkey = c("001", "001", "001", NA, "002"),
    datadate = as.Date(
      c("2020-03-31", "2020-03-31", "2020-06-30", "2020-06-30", "2020-03-31")
    ),
    rdq = as.Date(c("2020-04-30", "2020-03-01", NA, NA, NA)),
    fqtr = c(1, 1, 2, 2, 1),
    fyearq = c(2020, 2020, 2020, 2020, 2020),
    atq = c(10, 11, 12, 12, 13),
    ceqq = c(8, 9, 10, 10, 11),
    curcdq = c("USD", "USD", "USD", "USD", "CAD"),
    indfmt = "INDL",
    datafmt = "STD",
    consol = "C",
    xrdq = c(1, 2, 3, 4, 5)
  )

  local_mocked_bindings(
    get_wrds_connection = function() "con",
    disconnect_connection = function(con) NULL,
    tbl = function(src, from) fundq
  )

  out <- download_data_wrds_compustat(
    "compustat_quarterly",
    "2020-01-01",
    "2020-12-31",
    additional_columns = "xrdq",
    only_usd = TRUE
  )

  expect_equal(out$gvkey, c("001", "001"))
  expect_equal(out$xrdq, c(1, 3))
  expect_named(out, c("gvkey", "date", "datadate", "atq", "ceqq", "xrdq"))
})

test_that("quarterly data can return non-USD observations", {
  fundq <- tibble::tibble(
    gvkey = "002",
    datadate = as.Date("2020-03-31"),
    rdq = as.Date(NA),
    fqtr = 1,
    fyearq = 2020,
    atq = 13,
    ceqq = 11,
    curcdq = "CAD",
    indfmt = "INDL",
    datafmt = "STD",
    consol = "C"
  )

  local_mocked_bindings(
    get_wrds_connection = function() "con",
    disconnect_connection = function(con) NULL,
    tbl = function(src, from) fundq
  )

  out <- download_data_wrds_compustat(
    "compustat_quarterly",
    "2020-01-01",
    "2020-12-31"
  )

  expect_equal(out$gvkey, "002")
})

test_that("deprecated arguments are supported", {
  funda <- tibble::tibble(
    gvkey = "001",
    datadate = as.Date("2020-12-31"),
    indfmt = "INDL",
    datafmt = "STD",
    consol = "C",
    seq = 10,
    ceq = NA,
    at = 100,
    lt = 70,
    txditc = NA,
    txdb = NA,
    itcb = NA,
    pstkrv = NA,
    pstkl = NA,
    pstk = NA,
    capx = 1,
    oancf = 1,
    sale = 20,
    cogs = 5,
    xint = 1,
    xsga = 2,
    ib = 3,
    curcd = "USD"
  )

  local_mocked_bindings(
    get_wrds_connection = function() "con",
    disconnect_connection = function(con) NULL,
    tbl = function(src, from) funda
  )
  withr::local_options(lifecycle_verbosity = "warning")

  expect_warning(
    out <- download_data_wrds_compustat(
      type = "wrds_compustat_annual",
      start_date = "2020-01-01",
      end_date = "2020-12-31"
    ),
    "deprecated"
  )
  expect_equal(out$gvkey, "001")

  expect_warning(
    download_data_wrds_compustat(
      "compustat_annual",
      "2020-01-01",
      "2020-12-31",
      only_us = TRUE
    ),
    "deprecated"
  )

  expect_warning(
    download_data_wrds_compustat(
      "wrds_compustat_annual",
      "2020-01-01",
      "2020-12-31"
    ),
    "deprecated"
  )
})

test_that("defensive unsupported branch is covered", {
  local_mocked_bindings(
    check_supported_dataset_wrds_compustat = function(dataset) NULL,
    get_wrds_connection = function() "con"
  )

  expect_error(
    download_data_wrds_compustat("other"),
    "Unsupported Compustat dataset"
  )
})
