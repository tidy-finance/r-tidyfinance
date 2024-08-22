#' Estimate Fama-MacBeth Regressions
#'
#' This function estimates Fama-MacBeth regressions by first running cross-sectional regressions
#' for each time period and then aggregating the results over time to obtain average risk premia
#' and corresponding t-statistics.
#'
#' @param data A data frame containing the data for the regression. It must include a column
#'   representing the time periods (defaults to `date`) and the variables specified in the `model`.
#' @param model A formula representing the regression model to be estimated in each cross-section.
#' @param vcov A character string indicating the type of standard errors to compute. Options are `"iid"`
#'   for independent and identically distributed errors or `"newey-west"` for Newey-West standard errors.
#'   Default is `"newey-west"`.
#' @param data_options A named list with characters, indicating the column names required to run this function. The required column names identify dates. Defaults to `date=date`.
#'
#' @return A data frame with the estimated risk premiums, the number of observations, standard errors,
#'   and t-statistics for each factor in the model.
#'
#' @export
#'
#' @examples
#' set.seed(1234)
#'
#' data <- tibble::tibble(
#'   date = rep(seq.Date(from = as.Date("2020-01-01"),
#'                       to = as.Date("2020-12-01"), by = "month"), each = 50),
#'   permno = rep(1:50, times = 12),
#'   ret_excess = rnorm(600, 0, 0.1),
#'   beta = rnorm(600, 1, 0.2),
#'   bm = rnorm(600, 0.5, 0.1),
#'   log_mktcap = rnorm(600, 10, 1)
#' )
#'
#' estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap")
#' estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap", vcov = "iid")
#'
#' data <- data |> dplyr::rename(month = date)
#' estimate_fama_macbeth(data, "ret_excess ~ beta + bm + log_mktcap", data_options=list(date="month"))
#'
estimate_fama_macbeth <- function(data, model, vcov = "newey-west", data_options = list(date ="date")) {

  # Check that vcov is one of the allowed options
  if (!vcov %in% c("iid", "newey-west")) {
    cli::cli_abort("{.arg vcov} must be either 'iid' or 'newey-west'.")
  }

  # Check that the data has a date column
  if (!data_options$date %in% colnames(data)) {
    cli::cli_abort("The data must contain a {data_options$date} column.")
  }

  # Cross-sectional regressions
  cross_sections <- data |>
    tidyr::nest(data = -all_of(data_options$date)) |>
    mutate(
      row_check = purrr::map_lgl(data, ~ nrow(.) > length(all.vars(as.formula(model))))
    )

  # Check if any date grouping has fewer rows than columns in the model
  if (any(!cross_sections$row_check)) {
    cli::cli_abort(
      "Each date grouping must have more rows than the number of predictors in the model to estimate coefficients. Please check your data."
    )
  }

  # Proceed with estimation if all checks pass
  cross_sections <- cross_sections |>
    select(-row_check) |>
    mutate(estimates = purrr::map(data, ~ estimate_model(., model))) |>
    tidyr::unnest(estimates) |>
    select(-data) |>
    tidyr::pivot_longer(-all_of(data_options$date))

  # Function to compute the standard error based on the specified vcov
  compute_standard_error <- function(model, vcov) {
    if (vcov == "iid") {
      sqrt(stats::vcov(model)[1, 1])
    } else if (vcov == "newey-west") {
      rlang::check_installed(
        "sandwich", reason = "to use `vcov = newey-west` in `estimate_fama_macbeth()`."
      )
      sqrt(sandwich::NeweyWest(model))
    }
  }

  # Time-series aggregations
  aggregations <- cross_sections |>
    tidyr::nest(data = c(all_of(data_options$date), value)) |>
    mutate(model = purrr::map(data, ~ lm("value ~ 1", data = .)),
           risk_premium = purrr::map_dbl(model, ~ .$coefficients),
           n = purrr::map_dbl(data, nrow),
           standard_error = purrr::map_dbl(model, ~ compute_standard_error(., vcov))
    ) |>
    mutate(t_statistic = case_when(
      vcov == "iid" ~ risk_premium / standard_error * sqrt(n),
      vcov == "newey-west" ~ risk_premium / standard_error
    )) |>
    select(factor = name, risk_premium, n, standard_error, t_statistic)

  aggregations

}
