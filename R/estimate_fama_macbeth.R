#' Estimate Fama-MacBeth Regressions
#'
#' Estimates Fama-MacBeth regressions by first running cross-sectional
#' regressions for each time period and then aggregating the results
#' over time to obtain average risk premia and corresponding
#' t-statistics.
#'
#' @param data A data frame containing the data for the regression. It
#'   must include a column representing the time periods (defaults to
#'   `date`) and the variables specified in the `model`.
#' @param model A formula representing the regression model to be
#'   estimated in each cross-section.
#' @param vcov A character string indicating the type of standard
#'   errors to compute. Options are `"iid"` for independent and
#'   identically distributed errors or `"newey-west"` for Newey-West
#'   standard errors. Default is `"newey-west"`.
#' @param vcov_options A list of additional arguments to be passed to
#'   the `NeweyWest()` function when `vcov = "newey-west"`. These can
#'   include options such as `lag`, which specifies the number of lags
#'   to use in the Newey-West covariance matrix estimation, and
#'   `prewhite`, which indicates whether to apply a prewhitening
#'   transformation. Default is an empty list.
#' @param data_options A named list of \link{data_options} with
#'   characters, indicating the column names required to run this
#'   function. The required column names identify dates. Defaults to
#'   `date = date`.
#' @param detail A logical value indicating whether to return
#'   additional summary statistics. If `FALSE` (default), the function
#'   returns only the coefficient estimates. If `TRUE`, it returns a
#'   list with two elements: `coefficients` (the usual estimates
#'   table) and `summary_statistics` (a one-row tibble with the
#'   average cross-sectional R-squared and the average number of
#'   observations per cross-section).
#'
#' @returns If `detail = FALSE` (default), a tibble with columns
#'   `factor`, `risk_premium`, `n` (number of time periods),
#'   `standard_error`, and `t_statistic`.
#'
#'   If `detail = TRUE`, a named list with two elements:
#'   \describe{
#'     \item{coefficients}{The same tibble described above.}
#'     \item{summary_statistics}{A one-row tibble with `r_squared` (mean
#'       cross-sectional R-squared) and `n_obs` (mean cross-sectional
#'       observation count).}
#'   }
#'
#' @family estimation functions
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
#' estimate_fama_macbeth(
#'   data,
#'   "ret_excess ~ beta + bm + log_mktcap",
#'   vcov = "iid"
#' )
#' estimate_fama_macbeth(
#'   data,
#'   "ret_excess ~ beta + bm + log_mktcap",
#'   vcov = "newey-west",
#'   vcov_options = list(lag = 6, prewhite = FALSE)
#' )
#'
#' # Return detailed output including R-squared and observation counts
#' estimate_fama_macbeth(
#'   data,
#'   "ret_excess ~ beta + bm + log_mktcap",
#'   detail = TRUE
#' )
#'
#' # Use different column name for date
#' data |>
#'   dplyr::rename(month = date) |>
#'   estimate_fama_macbeth(
#'     "ret_excess ~ beta + bm + log_mktcap",
#'     data_options = data_options(date = "month")
#'  )
#'
estimate_fama_macbeth <- function(
  data,
  model,
  vcov = "newey-west",
  vcov_options = NULL,
  data_options = NULL,
  detail = FALSE
) {
  if (is.null(data_options)) {
    data_options <- data_options()
  }

  if (!vcov %in% c("iid", "newey-west")) {
    cli::cli_abort("{.arg vcov} must be either 'iid' or 'newey-west'.")
  }

  if (!data_options$date %in% colnames(data)) {
    cli::cli_abort("The data must contain a {data_options$date} column.")
  }

  cross_sections <- data |>
    tidyr::nest(data = -all_of(data_options$date)) |>
    mutate(
      row_check = purrr::map_lgl(
        data,
        ~ nrow(.) > length(all.vars(as.formula(model)))
      )
    )

  if (any(!cross_sections$row_check)) {
    cli::cli_abort(
      paste(
        "Each date grouping must have more rows than the number of predictors",
        "in the model to estimate coefficients. Please check your data."
      )
    )
  }

  cross_sections <- cross_sections |>
    select(-row_check) |>
    mutate(
      cross_fit = purrr::map(data, ~ lm(as.formula(model), data = .)),
      estimates = purrr::map(
        .data$cross_fit,
        ~ {
          coefs <- stats::coef(.)
          if ("(Intercept)" %in% names(coefs)) {
            names(coefs)[names(coefs) == "(Intercept)"] <- "intercept"
          }
          tibble::as_tibble(t(coefs))
        }
      ),
      r_squared = purrr::map_dbl(.data$cross_fit, ~ summary(.)$r.squared),
      adj_r_squared = purrr::map_dbl(
        .data$cross_fit,
        ~ summary(.)$adj.r.squared
      ),
      n_obs = purrr::map_dbl(.data$cross_fit, ~ nrow(.$model))
    ) |>
    select(-"cross_fit")

  cross_section_stats <- cross_sections |>
    select(all_of(data_options$date), "r_squared", "adj_r_squared", "n_obs")

  cross_sections <- cross_sections |>
    select(-"r_squared", -"adj_r_squared", -"n_obs") |>
    tidyr::unnest(estimates) |>
    select(-data) |>
    tidyr::pivot_longer(-all_of(data_options$date))

  compute_standard_error <- function(model, vcov, vcov_options = NULL) {
    if (vcov == "iid") {
      sqrt(stats::vcov(model)[1, 1])
    } else if (vcov == "newey-west") {
      sqrt(do.call(sandwich::NeweyWest, c(list(model), vcov_options)))
    }
  }

  aggregations <- cross_sections |>
    tidyr::nest(data = c(all_of(data_options$date), value)) |>
    mutate(
      model = purrr::map(data, ~ lm("value ~ 1", data = .)),
      risk_premium = purrr::map_dbl(model, ~ .$coefficients),
      n = purrr::map_dbl(data, nrow),
      standard_error = purrr::map_dbl(
        model,
        \(x) compute_standard_error(x, vcov, vcov_options)
      ),
      t_statistic = risk_premium / standard_error
    )

  if (vcov == "iid") {
    aggregations <- aggregations |>
      mutate(t_statistic = t_statistic * sqrt(n))
  }

  aggregations <- aggregations |>
    select(factor = name, risk_premium, n, standard_error, t_statistic)

  if (detail) {
    avg_r_squared <- mean(cross_section_stats$r_squared)
    avg_adj_r_squared <- mean(cross_section_stats$adj_r_squared)
    avg_n_obs <- mean(cross_section_stats$n_obs)

    summary_statistics <- tibble::tibble(
      r_squared = avg_r_squared,
      adj_r_squared = avg_adj_r_squared,
      n_obs = avg_n_obs
    )

    list(
      coefficients = aggregations,
      summary_statistics = summary_statistics
    )
  } else {
    aggregations
  }
}
