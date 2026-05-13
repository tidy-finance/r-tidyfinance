make_portfolio_panel <- function(
  n_portfolios = 5L,
  n_months = 12L,
  seed = 42L
) {
  set.seed(seed)
  dates <- seq.Date(
    from = as.Date("2020-01-01"),
    by = "month",
    length.out = n_months
  )
  expand.grid(portfolio = seq_len(n_portfolios), date = dates) |>
    dplyr::mutate(
      ret_excess_ew = rnorm(dplyr::n(), 0, 0.05),
      ret_excess_vw = rnorm(dplyr::n(), 0, 0.05)
    ) |>
    tibble::as_tibble()
}

test_that("compute_long_short_returns returns top - bottom by default", {
  panel <- make_portfolio_panel()
  out <- compute_long_short_returns(panel)
  expect_named(out, c("date", "ret_excess_ew", "ret_excess_vw"))
  expect_equal(nrow(out), 12L)

  # Sanity-check one date
  d <- panel$date[1]
  rows <- panel[panel$date == d, ]
  expected_ew <- rows$ret_excess_ew[rows$portfolio == max(rows$portfolio)] -
    rows$ret_excess_ew[rows$portfolio == min(rows$portfolio)]
  expect_equal(out$ret_excess_ew[out$date == d], expected_ew)
})

test_that("compute_long_short_returns returns NA when only one portfolio", {
  # Mirrors the case where assign_portfolio collapses to a single bucket
  # because the sorting variable is constant. The long-short is undefined
  # (no investment), and the function should return NA rather than error
  # with `object 'top' not found`.
  panel <- make_portfolio_panel(n_portfolios = 1L)
  out <- compute_long_short_returns(panel)
  expect_named(out, c("date", "ret_excess_ew", "ret_excess_vw"))
  expect_equal(nrow(out), 12L)
  expect_true(all(is.na(out$ret_excess_ew)))
  expect_true(all(is.na(out$ret_excess_vw)))
})

test_that("compute_long_short_returns handles a single per-date long leg", {
  # All dates have a bottom portfolio; only some dates have a top portfolio.
  panel <- make_portfolio_panel(n_portfolios = 2L, n_months = 4L)
  panel <- panel[!(panel$date == panel$date[1] & panel$portfolio == 2L), ]
  out <- compute_long_short_returns(panel)
  expect_equal(nrow(out), 4L)
  expect_true(is.na(out$ret_excess_ew[out$date == panel$date[1]]))
  expect_true(all(!is.na(out$ret_excess_ew[out$date != panel$date[1]])))
})
