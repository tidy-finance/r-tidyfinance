set.seed(1234)

data_monthly <- tibble::tibble(
  date = rep(
    seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2020-12-01"),
      by = "month"
    ),
    each = 50
  ),
  permno = rep(1:50, times = 12),
  ret_excess = rnorm(600, 0, 0.1),
  mkt_excess = rnorm(600, 0, 0.1),
  smb = rnorm(600, 0, 0.1),
  hml = rnorm(600, 0, 0.1)
)

data_daily <- tibble::tibble(
  date = rep(
    seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2020-12-31"),
      by = "day"
    ),
    each = 50
  ),
  permno = rep(1:50, times = 366),
  ret_excess = rnorm(18300, 0, 0.02),
  mkt_excess = rnorm(18300, 0, 0.02),
  smb = rnorm(18300, 0, 0.02),
  hml = rnorm(18300, 0, 0.02)
) |>
  dplyr::mutate(date = lubridate::floor_date(date, "month"))


test_that("uses default data_options when NULL", {
  result <- estimate_betas(
    data_monthly,
    "ret_excess ~ mkt_excess",
    months(3)
  )
  expect_true("permno" %in% colnames(result))
  expect_true("date" %in% colnames(result))
})

test_that("uses custom data_options when provided", {
  df <- dplyr::rename(data_monthly, id = permno)
  result <- estimate_betas(
    df,
    "ret_excess ~ mkt_excess",
    months(3),
    data_options = data_options(id = "id")
  )
  expect_true("id" %in% colnames(result))
})

test_that("lookback in months is parsed correctly", {
  result <- estimate_betas(
    data_monthly,
    "ret_excess ~ mkt_excess",
    months(3)
  )
  expect_s3_class(result, "data.frame")
  expect_true("beta_mkt_excess" %in% colnames(result))
})

test_that("lookback in days is parsed correctly", {
  result <- estimate_betas(
    data_daily,
    "ret_excess ~ mkt_excess",
    lubridate::days(90)
  )
  expect_s3_class(result, "data.frame")
})

test_that("lookback in hours triggers hour period", {
  df <- data_monthly |>
    dplyr::mutate(
      date = as.POSIXct(date) + rep(seq(0, 11) * 3600, each = 50)
    )
  result <- estimate_betas(
    df,
    "ret_excess ~ mkt_excess",
    lubridate::hours(5)
  )
  expect_s3_class(result, "data.frame")
})

test_that("lookback in minutes triggers minute period", {
  df <- data_monthly |>
    dplyr::mutate(
      date = as.POSIXct(date) + rep(seq(0, 11) * 60, each = 50)
    )
  result <- estimate_betas(
    df,
    "ret_excess ~ mkt_excess",
    lubridate::minutes(5)
  )
  expect_s3_class(result, "data.frame")
})

test_that("lookback in seconds triggers second period", {
  df <- data_monthly |>
    dplyr::mutate(
      date = as.POSIXct(date) + rep(seq(0, 11), each = 50)
    )
  result <- estimate_betas(
    df,
    "ret_excess ~ mkt_excess",
    lubridate::seconds(5)
  )
  expect_s3_class(result, "data.frame")
})

test_that("invalid lookback (zero period) raises error", {
  bad_lookback <- lubridate::period(0)
  expect_error(
    estimate_betas(
      data_monthly,
      "ret_excess ~ mkt_excess",
      bad_lookback
    ),
    regexp = "lookback"
  )
})

test_that("min_obs defaults to 80% of lookback when NULL", {
  # Should run without error; coverage confirms NULL branch taken
  result <- estimate_betas(
    data_monthly,
    "ret_excess ~ mkt_excess",
    months(10),
    min_obs = NULL
  )
  expect_s3_class(result, "data.frame")
})

test_that("min_obs <= 0 raises an error", {
  expect_error(
    estimate_betas(
      data_monthly,
      "ret_excess ~ mkt_excess",
      months(3),
      min_obs = 0
    ),
    regexp = "min_obs"
  )
})

test_that("non-logical use_furrr raises an error", {
  expect_error(
    estimate_betas(
      data_monthly,
      "ret_excess ~ mkt_excess",
      months(3),
      use_furrr = "yes"
    ),
    regexp = "use_furrr"
  )
})

test_that("lookback < num_params issues a warning", {
  expect_warning(
    estimate_betas(
      data_monthly,
      "ret_excess ~ mkt_excess + smb + hml",
      months(2)
    ),
    regexp = "lookback"
  )
})

test_that("use_furrr = TRUE uses furrr path", {
  future::plan("sequential")
  result <- estimate_betas(
    data_monthly,
    "ret_excess ~ mkt_excess",
    months(3),
    use_furrr = TRUE
  )
  expect_s3_class(result, "data.frame")
  expect_true("beta_mkt_excess" %in% colnames(result))
})

test_that("intercept column is renamed from beta_intercept", {
  result <- estimate_betas(
    data_monthly,
    "ret_excess ~ mkt_excess",
    months(3)
  )
  expect_true("intercept" %in% colnames(result))
  expect_false("beta_intercept" %in% colnames(result))
})
