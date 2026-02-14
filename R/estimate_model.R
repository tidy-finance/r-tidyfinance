#' Estimate Model
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' This function estimates a linear model specified by one or more independent
#' variables. It checks for the presence of the specified independent variables
#' in the dataset and whether the dataset has a sufficient number of
#' observations. Depending on the `output` parameter, it returns the model's
#' coefficients, t-statistics, residuals, or any combination in a named list.
#'
#' @param data A data frame containing the dependent variable and one or more
#'   independent variables.
#' @param model A character that describes the model to estimate (e.g.
#'   `"ret_excess ~ mkt_excess + hmb + sml"`).
#' @param min_obs The minimum number of observations required to estimate the
#'   model. Defaults to 1.
#' @param output A character vector specifying what to return. Must contain one
#'   or more of `"coefficients"` (default), `"residuals"`, and `"tstats"`. If a
#'   single value is provided, the corresponding object is returned directly.
#'   If multiple values are provided, a named list is returned.
#'
#' @returns If `output` contains a single value: a data frame of coefficients
#'   or t-statistics, or a numeric vector of residuals. If `output` contains
#'   multiple values: a named list with the requested elements. Coefficients
#'   and t-statistics are returned as data frames with column names
#'   corresponding to the model terms. Residuals are returned as a numeric
#'   vector of length `nrow(data)` with `NA` for rows with missing data or
#'   insufficient observations.
#'
#' @export
#'
#' @examples
#' data <- data.frame(
#'   ret_excess = rnorm(100),
#'   mkt_excess = rnorm(100),
#'   smb = rnorm(100),
#'   hml = rnorm(100)
#' )
#'
#' # Estimate model with a single independent variable
#' estimate_model(data, "ret_excess ~ mkt_excess")
#'
#' # Estimate model with multiple independent variables
#' estimate_model(data, "ret_excess ~ mkt_excess + smb + hml")
#'
#' # Estimate model without intercept
#' estimate_model(data, "ret_excess ~ mkt_excess - 1")
#'
#' # Calculate residuals
#' estimate_model(data, "ret_excess ~ mkt_excess + smb + hml",
#'   output = "residuals"
#' )
#'
#' # Return t-statistics
#' estimate_model(data, "ret_excess ~ mkt_excess + smb + hml",
#'   output = "tstats"
#' )
#'
#' # Return coefficients, t-statistics, and residuals
#' estimate_model(data, "ret_excess ~ mkt_excess + smb + hml",
#'   output = c("coefficients", "tstats", "residuals")
#' )
#'
estimate_model <- function(data, model, min_obs = 1, output = "coefficients") {
  valid_outputs <- c("coefficients", "tstats", "residuals")
  invalid_outputs <- setdiff(output, valid_outputs)
  if (length(invalid_outputs) > 0) {
    cli::cli_abort(
      "{.arg output} must contain one or more of {.val {valid_outputs}}, not {.val {invalid_outputs}}."
    )
  }

  model_parts <- strsplit(model, "~", fixed = TRUE)[[1]]
  response_var <- trimws(model_parts[1])
  independent_vars <- strsplit(trimws(model_parts[2]), "[ +]")[[1]]
  independent_vars <- independent_vars[nzchar(independent_vars)]
  independent_vars <- independent_vars[!independent_vars %in% c("-", "1")]

  if ("intercept" %in% independent_vars) {
    cli::cli_abort(
      "None of the columns in {.arg model} may be called 'intercept'. Please rename the column and try again."
    )
  }

  missing_vars <- independent_vars[!independent_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    cli::cli_abort(
      "The following independent variables are missing in the data: {toString(missing_vars)}."
    )
  }

  formula <- stats::as.formula(model)

  return_multiple <- length(output) > 1
  needs_residuals <- "residuals" %in% output
  needs_coefficients <- "coefficients" %in% output
  needs_tstats <- "tstats" %in% output

  if (needs_residuals) {
    complete <- stats::complete.cases(stats::model.frame(
      formula,
      data = data,
      na.action = stats::na.pass
    ))
    insufficient_residuals <- sum(complete) < min_obs ||
      sum(complete) <= length(independent_vars)
  }

  if (needs_coefficients || needs_tstats) {
    insufficient_summary <- nrow(data) < min_obs ||
      nrow(data) <= length(independent_vars)
  }

  fit <- NULL
  if (needs_residuals && !insufficient_residuals) {
    fit <- stats::lm(formula, data = data[complete, ])
  } else if ((needs_coefficients || needs_tstats) && !insufficient_summary) {
    fit <- stats::lm(formula, data = data)
  }

  to_tibble <- function(x) {
    if ("(Intercept)" %in% names(x)) {
      names(x)[names(x) == "(Intercept)"] <- "intercept"
    }
    as_tibble(t(x))
  }

  na_tibble <- function() {
    if (length(independent_vars) == 0) {
      return(NA_real_)
    }
    beta <- setNames(rep(NA_real_, length(independent_vars)), independent_vars)
    as_tibble(t(beta))
  }

  result <- list()

  if (needs_coefficients) {
    if (insufficient_summary) {
      result$coefficients <- na_tibble()
    } else {
      result$coefficients <- to_tibble(stats::coef(fit))
    }
  }

  if (needs_tstats) {
    if (insufficient_summary) {
      result$tstats <- na_tibble()
    } else {
      result$tstats <- to_tibble(summary(fit)$coefficients[, "t value"])
    }
  }

  if (needs_residuals) {
    if (insufficient_residuals) {
      result$residuals <- rep(NA_real_, nrow(data))
    } else {
      resid_result <- rep(NA_real_, nrow(data))
      resid_result[complete] <- fit$residuals
      result$residuals <- resid_result
    }
  }

  if (return_multiple) {
    return(result)
  } else {
    return(result[[1]])
  }
}
