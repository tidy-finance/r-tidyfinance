#' Estimate Rolling Betas
#'
#' This function estimates rolling betas for a given model using the provided data.
#' It supports parallel processing for faster computation using the `furrr` package.
#'
#' @param data A tibble containing the data with a date identifier (defaults to `date`), a stock
#'  identifier (defaults to `permno`), and other variables used in the model.
#' @param model A formula representing the model to be estimated (e.g.,
#'   `ret_excess ~ mkt_excess + smb + hml`).
#' @param lookback A Period object specifying the number of months, days, hours, minutes, or
#'  seconds to look back when estimating the rolling model.
#' @param min_obs An integer specifying the minimum number of observations required to estimate
#'  the model. Defaults to 80% of `lookback`.
#' @param use_furrr A logical indicating whether to use the `furrr` package and its paralellization
#'  capabilities. Defaults to FALSE.
#' @param data_options A named list of \link{data_options} with characters, indicating the column
#'  names required to run this function. The required column names identify dates and the stocks.
#'  Defaults to `date = date` and `id = permno`.
#' @return A tibble with the estimated betas for each time period.
#'
#' @export
#'
#' @examples
#' # Estimate monthly betas using monthly return data
#' set.seed(1234)
#' data_monthly <- tibble::tibble(
#'   date = rep(seq.Date(from = as.Date("2020-01-01"),
#'                       to = as.Date("2020-12-01"), by = "month"), each = 50),
#'   permno = rep(1:50, times = 12),
#'   ret_excess = rnorm(600, 0, 0.1),
#'   mkt_excess = rnorm(600, 0, 0.1),
#'   smb = rnorm(600, 0, 0.1),
#'   hml = rnorm(600, 0, 0.1),
#' )
#'
#' estimate_betas(data_monthly,  "ret_excess ~ mkt_excess", months(3))
#' estimate_betas(data_monthly,  "ret_excess ~ mkt_excess + smb + hml", months(6))
#'
#' data_monthly |>
#'   dplyr::rename(id = permno) |>
#'   estimate_betas("ret_excess ~ mkt_excess", months(3),
#'                  data_options = data_options(id = "id"))
#'
#' # Estimate monthly betas using daily return data and parallelization
#' data_daily <- tibble::tibble(
#'   date = rep(seq.Date(from = as.Date("2020-01-01"),
#'                       to = as.Date("2020-12-31"), by = "day"), each = 50),
#'   permno = rep(1:50, times = 366),
#'   ret_excess = rnorm(18300, 0, 0.02),
#'   mkt_excess = rnorm(18300, 0, 0.02),
#'   smb = rnorm(18300, 0, 0.02),
#'   hml = rnorm(18300, 0, 0.02),
#' )
#'
#' data_daily <- data_daily |>
#'   dplyr::mutate(date = lubridate::floor_date(date, "month"))
#'
#' # Change settings via future::plan(strategy = "multisession", workers = 4)
#' estimate_betas(data_daily, "ret_excess ~ mkt_excess", lubridate::days(90), use_furrr = TRUE)
#'
estimate_betas <- function(
  data,
  model,
  lookback,
  min_obs = NULL,
  use_furrr = FALSE,
  data_options = NULL
) {
  if (is.null(data_options)) {
    data_options <- data_options()
  }

  if (lookback@month > 0) {
    lookback <- lookback@month
    period <- "month"
  } else if (lookback@day > 0) {
    lookback <- lookback@day
    period <- "day"
  } else if (lookback@hour > 0) {
    lookback <- lookback@hour
    period <- "hour"
  } else if (lookback@minute > 0) {
    lookback <- lookback@minute
    period <- "minute"
  } else if (lookback@second > 0) {
    lookback <- lookback@second
    period <- "second"
  } else {
    cli::cli_abort(
      "{.arg lookback} must contain either a positive month, days, hours, minutes, or seconds"
    )
  }

  if (is.null(min_obs)) {
    min_obs <- round(lookback * 0.8, 0)
  } else if (min_obs <= 0) {
    cli::cli_abort("{.arg min_obs} must be a positive integer.")
  }

  if (!is.logical(use_furrr)) {
    cli::cli_abort("{.arg use_furrr} must be a logical.")
  }

  # Warning if lookback is too low to estimate all model parameters
  num_params <- length(all.vars(as.formula(model))) - 1
  if (lookback < num_params) {
    cli::cli_warn(
      "{.arg lookback} is too low to estimate all model parameters. Consider increasing it."
    )
  }

  roll_model_estimation <- function(
    df,
    model,
    lookback,
    period,
    min_obs,
    data_options
  ) {
    df <- df |>
      arrange(data_options$date)

    betas <- slider::slide_period_dfr(
      .x = df,
      .i = df |> pull(data_options$date),
      .period = period,
      .f = ~ estimate_model(., model, min_obs),
      .before = lookback - 1,
      .complete = FALSE
    )

    bind_cols(
      tibble("{data_options$date}" := unique(df |> pull(data_options$date))),
      betas
    )
  }

  if (use_furrr) {
    betas <- data |>
      tidyr::nest(data = -all_of(data_options$id)) |>
      mutate(
        beta = furrr::future_map(
          data,
          ~ roll_model_estimation(
            .,
            model,
            lookback,
            period,
            min_obs,
            data_options
          )
        )
      )
  } else {
    betas <- data |>
      tidyr::nest(data = -all_of(data_options$id)) |>
      mutate(
        beta = purrr::map(
          data,
          ~ roll_model_estimation(
            .,
            model,
            lookback,
            period,
            min_obs,
            data_options
          )
        )
      )
  }

  betas <- betas |>
    tidyr::unnest(beta, names_sep = "_") |>
    select(-data) |>
    rename("{data_options$date}" := beta_date)

  if ("beta_intercept" %in% colnames(betas)) {
    colnames(betas)[colnames(betas) == "beta_intercept"] <- "intercept"
  }

  betas
}
