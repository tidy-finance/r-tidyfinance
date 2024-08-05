estimate_betas <- function(
    data,
    model,
    months_lookback,
    min_obs = round(months_lookback * 0.8, 0),
    n_cores = 1
) {

  # TODO: add checks and parameter verifications

  # TODO: throw warning for case when months_lookback is too low to estimate all model parameters

  # TODO: throw error for negative months_lookback, min_obs and n_cores

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

  if (n_cores == 1) {
    betas <- data |>
      tidyr::nest(data = -permno) |>
      mutate(
        beta = purrr::map(data, ~ roll_model_estimation(., model, months_lookback, min_obs))
      ) |>
      tidyr::unnest(beta, names_sep = "_") |>
      select(-data)
  }

  if (n_cores > 1) {
    furrr::plan(multisession, workers = n_cores)
    betas <- data |>
      tidyr::nest(data = -permno) |>
      mutate(
        beta = furrr::future_map(data, ~ roll_model_estimation(., model, months_lookback, min_obs))
      ) |>
      tidyr::unnest(beta, names_sep = "_") |>
      select(-data)
  }
  betas
}

set.seed(1234)
data <- tibble::tibble(
  date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 50),
  permno = rep(1:50, times = 12),
  ret_excess = rnorm(600, 0, 0.1),
  mkt_excess = rnorm(600, 0, 0.1),
  smb = rnorm(600, 0, 0.1),
  hml = rnorm(600, 0, 0.1),
)

estimate_betas(data,  "ret_excess ~ mkt_excess", 3)

estimate_betas(data,  "ret_excess ~ mkt_excess + smb + hml", 6)




