#' Estimate Rolling Betas
#'
#' This function estimates rolling betas for a given model using the provided data.
#' It supports parallel processing for faster computation using the `furrr` package.
#'
#' @param data A tibble containing the data with columns `date`, `permno`, and other variables used in the model.
#' @param model A formula representing the model to be estimated (e.g., `ret_excess ~ mkt_excess + smb + hml`).
#' @param months_lookback An integer specifying the number of months to look back when estimating the rolling model.
#' @param min_obs An integer specifying the minimum number of observations required to estimate the model.
#'  Defaults to 80% of `months_lookback`.
#' @param future_strategy A character specifying the strategy for resolving `future`'s parallelization capabilites
#'  ("sequential", "multisession", "multicore", "cluster"). Defaults to "multisession".
#' @param future_workers An integer specifying the number of workers to use for processing (e.g. number of sessions
#'  for "multisession"). Defaults to 1.
#'
#' @return A tibble with the estimated betas for each time period.
#'
#' @export
#'
#' @examples
#' # Estimate monthly betas using monthly return data
#' set.seed(1234)
#' data_monthly <- tibble::tibble(
#'   date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 50),
#'   permno = rep(1:50, times = 12),
#'   ret_excess = rnorm(600, 0, 0.1),
#'   mkt_excess = rnorm(600, 0, 0.1),
#'   smb = rnorm(600, 0, 0.1),
#'   hml = rnorm(600, 0, 0.1),
#' )
#'
#' estimate_betas(data_monthly,  "ret_excess ~ mkt_excess", 3)
#' estimate_betas(data_monthly,  "ret_excess ~ mkt_excess + smb + hml", 6)
#'
#' Estimate monthly betas using daily return data
#' data_daily <- tibble::tibble(
#'   date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-31"), by = "day"), each = 50),
#'   permno = rep(1:50, times = 366),
#'   ret_excess = rnorm(18300, 0, 0.02),
#'   mkt_excess = rnorm(18300, 0, 0.02),
#'   smb = rnorm(18300, 0, 0.02),
#'   hml = rnorm(18300, 0, 0.02),
#' )
#'
#' data_daily <- data_daily |>
#'   mutate(date = lubridate::floor_date(date, "month"))
#'
#' estimate_betas(data_daily, "ret_excess ~ mkt_excess + smb + hml", months_lookback = 6, future_workers = 4)
#'
estimate_betas <- function(
    data,
    model,
    months_lookback,
    min_obs = round(months_lookback * 0.8, 0),
    future_strategy = "multisession",
    future_workers = 1
) {

  # Check for valid parameters
  if (months_lookback <= 0) {
    cli::cli_abort("{.arg months_lookback} must be a positive integer.")
  }

  if (min_obs <= 0 ) {
    cli::cli_abort("{.arg min_obs} must be a positive integer.")
  }

  if (future_workers <= 0) {
    cli::cli_abort("{.arg future_workers} must be a positive integer.")
  }

  # Warning if months_lookback is too low to estimate all model parameters
  num_params <- length(all.vars(as.formula(model))) - 1
  if (months_lookback < num_params) {
    cli::cli_warn("{.arg months_lookback} is too low to estimate all model parameters. Consider increasing it.")
  }

  roll_model_estimation <- function(data, model, months_lookback, min_obs) {
    data <- data |>
      arrange(date)

    betas <- slider::slide_period_dfr(
      .x = data,
      .i = data$date,
      .period = "month",
      .f = ~ estimate_model(., model, min_obs),
      .before = months_lookback - 1,
      .complete = FALSE
    )

    bind_cols(
      tibble(date = unique(data$date)),
      betas
    )
  }

  if (future_workers == 1) {
    betas <- data |>
      tidyr::nest(data = -permno) |>
      mutate(
        beta = purrr::map(data, ~ roll_model_estimation(., model, months_lookback, min_obs))
      )
  }

  if (future_workers > 1) {
    rlang::check_installed("furrr")
    rlang::check_installed("future")

    future::plan(strategy = future_strategy, workers = future_workers)

    betas <- data |>
      tidyr::nest(data = -permno) |>
      mutate(
        beta = furrr::future_map(data, ~ roll_model_estimation(., model, months_lookback, min_obs))
      )
  }
  betas |>
    tidyr::unnest(beta, names_sep = "_") |>
    select(-data)
}
