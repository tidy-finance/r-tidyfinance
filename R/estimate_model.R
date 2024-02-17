#' Estimate Model Coefficients
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function estimates the coefficients of a linear model specified by one or more independent variables.
#' It checks for the presence of the specified independent variables in the dataset and whether the dataset has
#' a sufficient number of observations. It returns the model's coefficients as either a numeric value (for a single
#' independent variable) or a data frame (for multiple independent variables).
#'
#' @param data A data frame containing the dependent variable and one or more independent variables.
#' @param ... Named arguments where each name is an independent variable in `data` and each value should be
#'        a formula specifying the variable (e.g., x = ~ x). This allows for flexible specification of the model.
#' @param min_obs The minimum number of observations required to estimate the model. Defaults to 1.
#'
#' @return If a single independent variable is specified, a numeric value representing the coefficient of that
#'         variable. If multiple independent variables are specified, a data frame with a row for each coefficient
#'         and column names corresponding to the independent variables.
#'
#' @examples
#' data <- data.frame(
#'   ret_excess = rnorm(100),
#'   mkt_excess = rnorm(100),
#'   smb = rnorm(100),
#'   hml = rnorm(100)
#' )
#' # Estimate model with a single independent variable
#' single_var_model <- estimate_model(data, mkt_excess)
#'
#' # Estimate model with multiple independent variables
#' multi_var_model <- estimate_model(data, mkt_excess, smb, hml)
#'
#' @export
#'
#' @importFrom stats setNames lm reformulate coefficients
#'
#' @seealso \code{\link[stats]{lm}} for details on the underlying linear model fitting used.
estimate_model <- function(data, ..., min_obs = 1) {
  independent_vars_syms <- ensyms(...)
  independent_vars <- sapply(independent_vars_syms, as.character)

  if (nrow(data) < min_obs) {
    beta <- stats::setNames(as.numeric(rep(NA, length(independent_vars))), independent_vars)
    if (length(beta) == 1) {
      return(as.numeric(NA))
    } else {
      return(data.frame(t(beta)))
    }
  } else {
    if (!all(independent_vars %in% names(data))) {
      missing_vars <- independent_vars[!independent_vars %in% names(data)]
      stop("The following independent variables are missing in the data: ",
           paste(missing_vars, collapse=", "), ".")
    }

    formula <- stats::reformulate(termlabels = independent_vars, response = "ret_excess")

    fit <- stats::lm(formula, data = data)
    beta <- stats::coefficients(fit)[names(stats::coefficients(fit)) %in% independent_vars]

    if (length(beta) == 1) {
      return(as.numeric(beta))
    } else {
      return(data.frame(t(beta)))
    }
  }
}
