estimate_fama_macbeth <- function(data, model, vcov = "newey-west") {

  # TODO: add check for vcov and model (from estimate_model or rely on its checks?)

  # Cross-sectional regressions
  cross_sections <- data |>
    nest(data = -date) |>
    mutate(estimates = map(data, ~ estimate_model(., model))) |>
    unnest(estimates) |>
    select(-data) |>
    pivot_longer(-date)

  compute_standard_error <- function(model, vcov) {
    if (vcov == "iid") {
      sqrt(stats::vcov(model)[1, 1])
    } else if (vcov == "newey-west") {
      sqrt(sandwich::NeweyWest(model))
    }
  }

  # Time-series aggregations
  aggregations <- cross_sections |>
    nest(data = c(date, value)) |>
    mutate(model = map(data, ~ lm("value ~ 1", data = .)),
           risk_premium = map_dbl(model, ~ .$coefficients),
           n = map_dbl(data, nrow),
           standard_error = map_dbl(model, ~ compute_standard_error(., vcov))
    ) |>
    mutate(t_statistic = case_when(
      vcov == "iid" ~ risk_premium / standard_error * sqrt(n),
      vcov == "newey-west" ~ risk_premium / standard_error
    )) |>
    select(factor = name, risk_premium, n, standard_error, t_statistic)

  aggregations

}

data <- tibble(
  date = rep(seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-12-01"), by = "month"), each = 50),
  permno = rep(1:50, times = 12),
  ret_excess = rnorm(600, 0, 0.1),
  beta = rnorm(600, 1, 0.2),
  bm = rnorm(600, 0.5, 0.1),
  log_mktcap = rnorm(600, 10, 1)
)

estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap")
estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap", vcov = "iid")
