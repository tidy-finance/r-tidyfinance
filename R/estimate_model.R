#' Estimate Model Coefficients
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function estimates the coefficients of a linear model specified by one
#' or more independent variables. It checks for the presence of the specified
#' independent variables in the dataset and whether the dataset has a sufficient
#' number of observations. It returns the model's coefficients as either a
#' numeric value (for a single independent variable) or a data frame (for
#' multiple independent variables).
#'
#' @param data A data frame containing the dependent variable and one or more
#'   independent variables.
#' @param model A character that describes the model to estimate (e.g.
#'   "ret_excess ~ mkt_excess + hmb + sml").
#' @param min_obs The minimum number of observations required to estimate the
#'   model. Defaults to 1.
#'
#' @returns If a single independent variable is specified, a numeric value
#'   representing the coefficient of that variable. If multiple independent
#'   variables are specified, a data frame with a row for each coefficient and
#'   column names corresponding to the independent variables.
#'
#' @seealso [stats::lm()] for details on the underlying linear model fitting used.
#' @export
#' @examples
#' data <- data.frame(
#'   ret_excess = rnorm(100),
#'   mkt_excess = rnorm(100),
#'   smb = rnorm(100),
#'   hml = rnorm(100)
#' )
#' # Estimate model with a single independent variable
#' single_var_model <- estimate_model(data, "ret_excess ~ mkt_excess")
#'
#' # Estimate model with multiple independent variables
#' multi_var_model <- estimate_model(data, "ret_excess ~ mkt_excess + smb + hml")
estimate_model <- function(data, model, min_obs = 1) {
  model_parts <- strsplit(model, "~", fixed = TRUE)[[1]]
  response_var <- trimws(model_parts[1])
  independent_vars <- strsplit(trimws(model_parts[2]), "[ +]")[[1]]
  independent_vars <- independent_vars[nzchar(independent_vars)]

  if (nrow(data) < min_obs) {
    if (length(independent_vars) == 0) {
      return(NA_real_)
    }
    beta <- setNames(rep(NA_real_, length(independent_vars)), independent_vars)
    return(as_tibble(t(beta)))
  }

  missing_vars <- independent_vars[!independent_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    cli::cli_abort(
      "The following independent variables are missing in the data: {toString(missing_vars)}."
    )
  }

  fit <- stats::lm(stats::as.formula(model), data = data)
  beta <- stats::coef(fit)
  beta <- beta[names(beta) %in% independent_vars]

  as_tibble(t(beta))
}
