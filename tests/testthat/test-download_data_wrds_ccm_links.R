test_that("downloads default CCM links and replaces missing end dates", {
  con <- new.env()
  disconnected <- FALSE

  links <- tibble::tibble(
    lpermno = c(1, 2, 3, 4),
    gvkey = c("001", "002", "003", "004"),
    linkdt = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01")),
    linkenddt = as.Date(c(NA, "2021-02-01", "2021-03-01", NA)),
    linktype = c("LU", "LC", "XX", "LU"),
    linkprim = c("P", "C", "P", "X")
  )

  testthat::local_mocked_bindings(
    get_wrds_connection = function() con,
    tbl = function(src, from, ...) {
      expect_identical(src, con)
      expect_identical(as.character(from), "crsp.ccmxpf_lnkhist")
      links
    },
    today = function() as.Date("2024-01-31"),
    disconnect_connection = function(src) {
      expect_identical(src, con)
      disconnected <<- TRUE
    }
  )

  result <- download_data_wrds_ccm_links()

  expect_true(disconnected)
  expect_named(result, c("permno", "gvkey", "linkdt", "linkenddt"))
  expect_equal(result$permno, c(1, 2))
  expect_equal(result$linkenddt[1], as.Date("2024-01-31"))
  expect_equal(result$linkenddt[2], as.Date("2021-02-01"))
})

test_that("passes custom link filters to the CCM query", {
  con <- new.env()

  links <- tibble::tibble(
    lpermno = c(1, 2, 3),
    gvkey = c("001", "002", "003"),
    linkdt = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")),
    linkenddt = as.Date(c("2021-01-01", "2021-02-01", "2021-03-01")),
    linktype = c("LU", "LC", "LU"),
    linkprim = c("P", "P", "C")
  )

  testthat::local_mocked_bindings(
    get_wrds_connection = function() con,
    tbl = function(src, from, ...) links,
    disconnect_connection = function(src) NULL
  )

  result <- download_data_wrds_ccm_links(
    linktype = "LU",
    linkprim = "C"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$permno, 3)
  expect_equal(result$gvkey, "003")
})
