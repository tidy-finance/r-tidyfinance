test_that("downloads filtered FISD data for USA issuers", {
  con <- new.env()
  disconnected <- FALSE

  issue <- tibble::tibble(
    complete_cusip = c("111111111", "222222222"),
    maturity = as.Date(c("2030-01-01", "2031-01-01")),
    offering_amt = c(100, 200),
    offering_date = as.Date(c("2020-01-01", "2020-02-01")),
    dated_date = as.Date(c("2020-01-02", "2020-02-02")),
    interest_frequency = c("2", "2"),
    coupon = c(5, 6),
    last_interest_date = as.Date(c("2029-12-31", "2030-12-31")),
    issue_id = c("i1", "i2"),
    issuer_id = c("a", "b"),
    security_level = "SEN",
    slob = NA_character_,
    security_pledge = NA_character_,
    asset_backed = "N",
    defeased = "N",
    defeased_date = as.Date(NA),
    bond_type = "CDEB",
    pay_in_kind = NA_character_,
    pay_in_kind_exp_date = as.Date(NA),
    yankee = "N",
    canadian = "N",
    foreign_currency = "N",
    coupon_type = "F",
    fix_frequency = NA_character_,
    coupon_change_indicator = "N",
    rule_144a = "N",
    private_placement = "N",
    defaulted = "N",
    filing_date = as.Date(NA),
    settlement = NA_character_,
    convertible = "N",
    exchange = NA_character_,
    putable = "N",
    unit_deal = "N",
    exchangeable = "N",
    perpetual = "N",
    preferred_security = "N"
  )

  issuer <- tibble::tibble(
    issuer_id = c("a", "b"),
    sic_code = c("1234", "9999"),
    country_domicile = c("USA", "CAN")
  )

  testthat::local_mocked_bindings(
    get_wrds_connection = function() con,
    tbl = function(src, from, ...) {
      expect_identical(src, con)
      switch(
        as.character(from),
        "fisd.fisd_mergedissue" = issue,
        "fisd.fisd_mergedissuer" = issuer
      )
    },
    disconnect_connection = function(src) {
      expect_identical(src, con)
      disconnected <<- TRUE
    }
  )

  result <- download_data_wrds_fisd()

  expect_true(disconnected)
  expect_named(
    result,
    c(
      "complete_cusip",
      "maturity",
      "offering_amt",
      "offering_date",
      "dated_date",
      "interest_frequency",
      "coupon",
      "last_interest_date",
      "issue_id",
      "issuer_id",
      "sic_code"
    )
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$complete_cusip, "111111111")
  expect_equal(result$sic_code, "1234")
})

test_that("returns requested additional columns", {
  con <- new.env()

  issue <- tibble::tibble(
    complete_cusip = "111111111",
    maturity = as.Date("2030-01-01"),
    offering_amt = 100,
    offering_date = as.Date("2020-01-01"),
    dated_date = as.Date("2020-01-02"),
    interest_frequency = "2",
    coupon = 5,
    last_interest_date = as.Date("2029-12-31"),
    issue_id = "i1",
    issuer_id = "a",
    asset_backed = "N",
    defeased = "N",
    security_level = "SEN",
    slob = "N",
    security_pledge = NA_character_,
    defeased_date = as.Date(NA),
    bond_type = "CMTN",
    pay_in_kind = "N",
    pay_in_kind_exp_date = as.Date(NA),
    yankee = NA_character_,
    canadian = NA_character_,
    foreign_currency = "N",
    coupon_type = "Z",
    fix_frequency = NA_character_,
    coupon_change_indicator = "N",
    rule_144a = "N",
    private_placement = NA_character_,
    defaulted = "N",
    filing_date = as.Date(NA),
    settlement = NA_character_,
    convertible = "N",
    exchange = NA_character_,
    putable = NA_character_,
    unit_deal = NA_character_,
    exchangeable = NA_character_,
    perpetual = "N",
    preferred_security = NA_character_
  )

  issuer <- tibble::tibble(
    issuer_id = "a",
    sic_code = "1234",
    country_domicile = "USA"
  )

  testthat::local_mocked_bindings(
    get_wrds_connection = function() con,
    tbl = function(src, from, ...) {
      switch(
        as.character(from),
        "fisd.fisd_mergedissue" = issue,
        "fisd.fisd_mergedissuer" = issuer
      )
    },
    disconnect_connection = function(src) NULL
  )

  result <- download_data_wrds_fisd(
    additional_columns = c("asset_backed", "defeased")
  )

  expect_named(
    result,
    c(
      "complete_cusip",
      "maturity",
      "offering_amt",
      "offering_date",
      "dated_date",
      "interest_frequency",
      "coupon",
      "last_interest_date",
      "issue_id",
      "issuer_id",
      "asset_backed",
      "defeased",
      "sic_code"
    )
  )
  expect_equal(result$asset_backed, "N")
  expect_equal(result$defeased, "N")
})
