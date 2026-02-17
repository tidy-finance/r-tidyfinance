# Tests for crsp_adjust_prices() ----------------------------------------------

test_that("crsp_adjust_prices() correctly calculates adjusted prices", {
  crsp_data <- data.frame(
    permno = c(10001, 10001, 10001),
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03")),
    altprc = c(-25.50, 30.00, 40.00),
    cfacpr = c(1.0, 2.0, 1.5)
  )

  result <- crsp_adjust_prices(crsp_data)

  # Check that prc_adj column is created
  expect_true("prc_adj" %in% names(result))

  # Check calculations: |altprc| / cfacpr
  expect_equal(result$prc_adj[1], 25.50 / 1.0)
  expect_equal(result$prc_adj[2], 30.00 / 2.0)
  expect_equal(result$prc_adj[3], 40.00 / 1.5)
})

test_that("crsp_adjust_prices() handles zero altprc values as NA", {
  crsp_data <- data.frame(
    permno = c(10001, 10001),
    date = as.Date(c("2020-01-01", "2020-01-02")),
    altprc = c(0, 25.50),
    cfacpr = c(1.0, 1.0)
  )

  result <- crsp_adjust_prices(crsp_data)

  # Zero altprc should result in NA
  expect_true(is.na(result$prc_adj[1]))
  expect_equal(result$prc_adj[2], 25.50)
})

test_that("crsp_adjust_prices() handles negative altprc values", {
  crsp_data <- data.frame(
    permno = 10001,
    date = as.Date("2020-01-01"),
    altprc = -25.50,
    cfacpr = 1.0
  )

  result <- crsp_adjust_prices(crsp_data)

  # Negative values should be converted to positive
  expect_equal(result$prc_adj[1], 25.50)
})

test_that("crsp_adjust_prices() handles infinite values", {
  crsp_data <- data.frame(
    permno = c(10001, 10001),
    date = as.Date(c("2020-01-01", "2020-01-02")),
    altprc = c(25.50, 30.00),
    cfacpr = c(0, 1.0)
  )

  result <- crsp_adjust_prices(crsp_data)

  # Division by zero should result in NA
  expect_true(is.na(result$prc_adj[1]))
  expect_equal(result$prc_adj[2], 30.00)
})

test_that("crsp_adjust_prices() preserves original columns", {
  crsp_data <- data.frame(
    permno = 10001,
    date = as.Date("2020-01-01"),
    altprc = 25.50,
    cfacpr = 1.0,
    extra_col = "test"
  )

  result <- crsp_adjust_prices(crsp_data)

  expect_true("extra_col" %in% names(result))
  expect_equal(result$extra_col, "test")
})

# Tests for volume_adjustment_gao_ritter() ------------------------------------

test_that("volume_adjustment_gao_ritter() correctly adjusts NASDAQ volume", {
  crsp_data <- data.frame(
    permno = c(10001, 10001, 10001, 10001),
    date = as.Date(c("2000-06-15", "2001-06-15", "2002-06-15", "2004-06-15")),
    exchcd = c(3, 3, 3, 3), # All NASDAQ
    prc = c(25.50, 30.00, 40.25, 35.75),
    vol = c(1000000, 1000000, 1000000, 1000000),
    cfacpr = c(1.0, 1.0, 1.0, 1.0)
  )

  result <- volume_adjustment_gao_ritter(crsp_data)

  # Check that vol_adj and prc_adj columns are created
  expect_true("vol_adj" %in% names(result))
  expect_true("prc_adj" %in% names(result))

  # Check NASDAQ adjustments for different time periods
  expect_equal(result$vol_adj[1], 1000000 / 2.0) # Before 2001-02-01
  expect_equal(result$vol_adj[2], 1000000 / 1.8) # 2001-02-01 to 2001-12-31
  expect_equal(result$vol_adj[3], 1000000 / 1.6) # 2002-01-01 to 2003-12-31
  expect_equal(result$vol_adj[4], 1000000 / 1.0) # 2004-01-01 onwards
})

test_that("volume_adjustment_gao_ritter() does not adjust non-NASDAQ volume", {
  crsp_data <- data.frame(
    permno = c(10001, 10002),
    date = as.Date(c("2000-06-15", "2000-06-15")),
    exchcd = c(1, 2), # NYSE and AMEX
    prc = c(25.50, 30.00),
    vol = c(1000000, 500000),
    cfacpr = c(1.0, 1.0)
  )

  result <- volume_adjustment_gao_ritter(crsp_data)

  # Non-NASDAQ stocks should retain original volume
  expect_equal(result$vol_adj[1], 1000000)
  expect_equal(result$vol_adj[2], 500000)
})

test_that("volume_adjustment_gao_ritter() handles -99 volume values as NA", {
  crsp_data <- data.frame(
    permno = c(10001, 10001),
    date = as.Date(c("2020-01-01", "2020-01-02")),
    exchcd = c(3, 3),
    prc = c(25.50, 30.00),
    vol = c(-99, 1000000),
    cfacpr = c(1.0, 1.0)
  )

  result <- volume_adjustment_gao_ritter(crsp_data)

  # -99 should be converted to NA
  expect_true(is.na(result$vol[1]))
  expect_true(is.na(result$vol_adj[1]))
  expect_false(is.na(result$vol_adj[2]))
})

test_that("volume_adjustment_gao_ritter() handles zero price values as NA", {
  crsp_data <- data.frame(
    permno = c(10001, 10001),
    date = as.Date(c("2020-01-01", "2020-01-02")),
    exchcd = c(3, 3),
    prc = c(0, 25.50),
    vol = c(1000000, 1000000),
    cfacpr = c(1.0, 1.0)
  )

  result <- volume_adjustment_gao_ritter(crsp_data)

  # Zero price should be converted to NA
  expect_true(is.na(result$prc[1]))
  expect_true(is.na(result$prc_adj[1]))
  expect_equal(result$prc_adj[2], 25.50)
})

test_that("volume_adjustment_gao_ritter() calculates prc_adj correctly", {
  crsp_data <- data.frame(
    permno = c(10001, 10001),
    date = as.Date(c("2020-01-01", "2020-01-02")),
    exchcd = c(3, 3),
    prc = c(-25.50, 30.00), # Negative price
    vol = c(1000000, 1000000),
    cfacpr = c(1.0, 2.0)
  )

  result <- volume_adjustment_gao_ritter(crsp_data)

  # Check prc_adj calculations: |prc| / cfacpr
  expect_equal(result$prc_adj[1], 25.50 / 1.0)
  expect_equal(result$prc_adj[2], 30.00 / 2.0)
})

test_that("volume_adjustment_gao_ritter() handles boundary dates correctly", {
  crsp_data <- data.frame(
    permno = rep(10001, 4),
    date = as.Date(c("2001-02-01", "2001-01-31", "2002-01-01", "2004-01-01")),
    exchcd = rep(3, 4),
    prc = rep(25.00, 4),
    vol = rep(1000000, 4),
    cfacpr = rep(1.0, 4)
  )

  result <- volume_adjustment_gao_ritter(crsp_data)

  # Exact boundary dates
  expect_equal(result$vol_adj[1], 1000000 / 1.8) # 2001-02-01 (starts 1.8 period)
  expect_equal(result$vol_adj[2], 1000000 / 2.0) # 2001-01-31 (still 2.0 period)
  expect_equal(result$vol_adj[3], 1000000 / 1.6) # 2002-01-01 (starts 1.6 period)
  expect_equal(result$vol_adj[4], 1000000 / 1.0) # 2004-01-01 (starts 1.0 period)
})

# Tests for add_ccm_link_to_crsp() --------------------------------------------

test_that("add_ccm_link_to_crsp() correctly adds gvkey to CRSP data", {
  crsp_data <- data.frame(
    permno = c(10001, 10001, 10002),
    date = as.Date(c("2020-01-31", "2020-02-29", "2020-01-31")),
    ret = c(0.05, -0.02, 0.03)
  )

  ccm_link <- data.frame(
    permno = c(10001, 10002),
    gvkey = c("001234", "005678"),
    linkdt = as.Date(c("2010-01-01", "2015-01-01")),
    linkenddt = as.Date(c("2025-12-31", "2025-12-31"))
  )

  result <- add_ccm_link_to_crsp(crsp_data, ccm_link)

  # Check that gvkey column is added
  expect_true("gvkey" %in% names(result))

  # Check that correct gvkeys are assigned
  expect_equal(result$gvkey[result$permno == 10001], c("001234", "001234"))
  expect_equal(result$gvkey[result$permno == 10002], "005678")
})

test_that("add_ccm_link_to_crsp() respects link date ranges", {
  crsp_data <- data.frame(
    permno = c(10001, 10001, 10001),
    date = as.Date(c("2019-06-30", "2020-06-30", "2026-06-30")),
    ret = c(0.05, -0.02, 0.03)
  )

  ccm_link <- data.frame(
    permno = 10001,
    gvkey = "001234",
    linkdt = as.Date("2020-01-01"),
    linkenddt = as.Date("2025-12-31")
  )

  result <- add_ccm_link_to_crsp(crsp_data, ccm_link)

  # Date before linkdt should have NA
  expect_true(is.na(result$gvkey[1]))

  # Date within range should have gvkey
  expect_equal(result$gvkey[2], "001234")

  # Date after linkenddt should have NA
  expect_true(is.na(result$gvkey[3]))
})

test_that("add_ccm_link_to_crsp() handles multiple links per permno", {
  crsp_data <- data.frame(
    permno = c(10001, 10001),
    date = as.Date(c("2015-06-30", "2025-06-30")),
    ret = c(0.05, -0.02)
  )

  # Same permno with different gvkeys for different time periods
  ccm_link <- data.frame(
    permno = c(10001, 10001),
    gvkey = c("001234", "009999"),
    linkdt = as.Date(c("2010-01-01", "2020-01-01")),
    linkenddt = as.Date(c("2019-12-31", "2029-12-31"))
  )

  result <- add_ccm_link_to_crsp(crsp_data, ccm_link)

  # First date should match first gvkey
  expect_equal(result$gvkey[1], "001234")

  # Second date should match second gvkey
  expect_equal(result$gvkey[2], "009999")
})

test_that("add_ccm_link_to_crsp() returns NA for unmatched permnos", {
  crsp_data <- data.frame(
    permno = c(10001, 10002),
    date = as.Date(c("2020-01-31", "2020-01-31")),
    ret = c(0.05, -0.02)
  )

  ccm_link <- data.frame(
    permno = 10001,
    gvkey = "001234",
    linkdt = as.Date("2010-01-01"),
    linkenddt = as.Date("2025-12-31")
  )

  result <- add_ccm_link_to_crsp(crsp_data, ccm_link)

  # permno 10001 should have gvkey
  expect_equal(result$gvkey[result$permno == 10001], "001234")

  # permno 10002 should have NA (no link)
  expect_true(is.na(result$gvkey[result$permno == 10002]))
})

test_that("add_ccm_link_to_crsp() preserves original CRSP columns", {
  crsp_data <- data.frame(
    permno = 10001,
    date = as.Date("2020-01-31"),
    ret = 0.05,
    extra_col = "test"
  )

  ccm_link <- data.frame(
    permno = 10001,
    gvkey = "001234",
    linkdt = as.Date("2010-01-01"),
    linkenddt = as.Date("2025-12-31")
  )

  result <- add_ccm_link_to_crsp(crsp_data, ccm_link)

  # Original columns should be preserved
  expect_true("extra_col" %in% names(result))
  expect_equal(result$extra_col, "test")
  expect_true("ret" %in% names(result))
  expect_equal(result$ret, 0.05)
})

test_that("add_ccm_link_to_crsp() preserves row count", {
  crsp_data <- data.frame(
    permno = c(10001, 10001, 10002),
    date = as.Date(c("2020-01-31", "2020-02-29", "2020-01-31")),
    ret = c(0.05, -0.02, 0.03)
  )

  ccm_link <- data.frame(
    permno = c(10001, 10002),
    gvkey = c("001234", "005678"),
    linkdt = as.Date(c("2010-01-01", "2015-01-01")),
    linkenddt = as.Date(c("2025-12-31", "2025-12-31"))
  )

  result <- add_ccm_link_to_crsp(crsp_data, ccm_link)

  # Row count should be preserved
  expect_equal(nrow(result), nrow(crsp_data))
})

test_that("add_ccm_link_to_crsp() handles empty CRSP data", {
  crsp_data <- data.frame(
    permno = integer(),
    date = as.Date(character()),
    ret = numeric()
  )

  ccm_link <- data.frame(
    permno = 10001,
    gvkey = "001234",
    linkdt = as.Date("2010-01-01"),
    linkenddt = as.Date("2025-12-31")
  )

  result <- add_ccm_link_to_crsp(crsp_data, ccm_link)

  # Should return empty data frame with gvkey column
  expect_equal(nrow(result), 0)
  expect_true("gvkey" %in% names(result))
})

test_that("add_ccm_link_to_crsp() handles boundary dates correctly", {
  crsp_data <- data.frame(
    permno = rep(10001, 3),
    date = as.Date(c("2020-01-01", "2020-01-02", "2025-12-31")),
    ret = c(0.01, 0.02, 0.03)
  )

  ccm_link <- data.frame(
    permno = 10001,
    gvkey = "001234",
    linkdt = as.Date("2020-01-01"),
    linkenddt = as.Date("2025-12-31")
  )

  result <- add_ccm_link_to_crsp(crsp_data, ccm_link)

  # All dates should have gvkey (inclusive of boundaries)
  expect_true(all(!is.na(result$gvkey)))
  expect_true(all(result$gvkey == "001234"))
})
