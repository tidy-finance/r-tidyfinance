#' Estimate Rolling Betas
#'
#' Estimates rolling betas for a given linear model using a fast, vectorized
#' approach. Instead of fitting one regression per stock and estimation window,
#' the function collapses the data to additive cumulants (the entries of the
#' moment matrices \eqn{X'X} and \eqn{X'y}) per stock and period, aggregates
#' these cumulants over rolling calendar windows in a single pass with
#' [slider::slide_index_sum()], and recovers the coefficients via the
#' closed-form OLS solution \eqn{\hat\beta = (X'X)^{-1} X'y}. This produces
#' estimates that are numerically identical to a per-window regression, but
#' fast enough that no per-stock nesting or parallelization is required.
#'
#' @param data A data frame containing the data with a date identifier
#'   (defaults to `date`), a stock identifier (defaults to `permno`), and other
#'   variables used in the model.
#' @param model A character string describing the model to be estimated (e.g.,
#'   `"ret_excess ~ mkt_excess + hml + smb"`).
#' @param lookback A Period object specifying the number of months, days,
#'   hours, minutes, or seconds to look back when estimating the rolling model.
#' @param min_obs An integer specifying the minimum number of observations
#'   required to estimate the model. Defaults to 80% of `lookback`. Windows
#'   with fewer observations are dropped from the output.
#' @param data_options A list of class `tidyfinance_data_options` (created via
#'   [data_options()]) specifying column name mappings. The `id` is used to
#'   specify the entity (i.e., firm), and the `date` element is used to specify
#'   the date column. Uses [data_options()] default if `NULL`:
#'   `"id" = "permno"` and `"date" = "date"`.
#'
#' @returns A data frame with the estimated betas for each entity and period.
#'   It contains the entity and date identifiers, an `intercept` column (if the
#'   model includes one), and one `beta_<variable>` column per regressor.
#'
#' @family estimation functions
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
#' estimate_betas(
#'   data_monthly,
#'   "ret_excess ~ mkt_excess + smb + hml",
#'   months(6)
#' )
#'
#' data_monthly |>
#'   dplyr::rename(id = permno) |>
#'   estimate_betas("ret_excess ~ mkt_excess", months(3),
#'                  data_options = data_options(id = "id"))
#'
#' # Estimate monthly betas using daily return data
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
#' estimate_betas(
#'   data_daily,
#'   "ret_excess ~ mkt_excess",
#'   lubridate::days(90)
#' )
#'
estimate_betas <- function(
  data,
  model,
  lookback,
  min_obs = NULL,
  data_options = NULL
) {
  if (is.null(data_options)) {
    data_options <- data_options()
  }

  # Resolve the rolling window into a unit and an integer length
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
  } else if (lookback@.Data > 0) {
    lookback <- lookback@.Data
    period <- "second"
  } else {
    cli::cli_abort(
      paste(
        "{.arg lookback} must contain either a positive",
        "month, days, hours, minutes, or seconds"
      )
    )
  }

  if (is.null(min_obs)) {
    min_obs <- round(lookback * 0.8, 0)
  } else if (min_obs <= 0) {
    cli::cli_abort("{.arg min_obs} must be a positive integer.")
  }

  formula <- stats::as.formula(model)

  if ("intercept" %in% all.vars(formula)) {
    cli::cli_abort(
      paste(
        "None of the columns in {.arg model} may be called 'intercept'.",
        "Please rename the column and try again."
      )
    )
  }

  # Warning if lookback is too low to estimate all model parameters
  num_params <- length(all.vars(formula)) - 1
  if (lookback < num_params) {
    cli::cli_warn(
      "{.arg lookback} is too low to estimate all model parameters.",
      "Consider increasing it."
    )
  }

  id_col <- data_options$id
  date_col <- data_options$date

  # Build the design matrix and response, keeping rows aligned with the data so
  # that incomplete observations can be dropped consistently afterwards.
  model_frame <- stats::model.frame(
    formula,
    data = data,
    na.action = stats::na.pass
  )
  response <- stats::model.response(model_frame)
  design <- stats::model.matrix(stats::terms(formula), model_frame)
  colnames(design)[colnames(design) == "(Intercept)"] <- "intercept"

  coef_names <- colnames(design)
  num_coefs <- ncol(design)

  complete <- stats::complete.cases(design) & !is.na(response)
  design <- design[complete, , drop = FALSE]
  response <- response[complete]

  period_index <- period_to_index(data[[date_col]][complete], period)
  period_date <- lubridate::floor_date(data[[date_col]][complete], period)

  # Cross-product (upper-triangular) and design-times-response columns make up
  # the additive cumulants of the moment matrices X'X and X'y.
  pairs <- which(upper.tri(diag(num_coefs), diag = TRUE), arr.ind = TRUE)
  pairs <- pairs[order(pairs[, "row"], pairs[, "col"]), , drop = FALSE]
  xx <- design[, pairs[, "row"], drop = FALSE] *
    design[, pairs[, "col"], drop = FALSE]
  colnames(xx) <- paste0("xx_", seq_len(nrow(pairs)))
  xy <- design * response
  colnames(xy) <- paste0("xy_", seq_len(num_coefs))

  moment_cols <- c(colnames(xx), colnames(xy))

  cumulants <- dplyr::bind_cols(
    tibble::tibble(
      "{id_col}" := data[[id_col]][complete],
      period_date = period_date,
      period_index = period_index,
      n = 1
    ),
    tibble::as_tibble(xx),
    tibble::as_tibble(xy)
  )

  # Collapse to one row of cumulants per entity and period, then aggregate the
  # cumulants over rolling windows with a single index-aware C-level sum.
  rolled <- cumulants |>
    dplyr::summarize(
      dplyr::across(dplyr::all_of(c("n", moment_cols)), sum),
      .by = dplyr::all_of(c(id_col, "period_date", "period_index"))
    ) |>
    dplyr::arrange(.data[[id_col]], .data[["period_index"]]) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("n", moment_cols)),
        \(col) slider::slide_index_sum(
          col,
          .data[["period_index"]],
          before = lookback - 1
        )
      ),
      .by = dplyr::all_of(id_col)
    ) |>
    dplyr::filter(.data[["n"]] >= min_obs)

  # Recover the coefficients window by window via closed-form OLS. Each solve
  # operates on tiny precomputed moment matrices, so this stays cheap.
  betas <- solve_moment_systems(
    as.matrix(rolled[colnames(xx)]),
    as.matrix(rolled[colnames(xy)]),
    pairs,
    num_coefs
  )
  colnames(betas) <- ifelse(
    coef_names == "intercept",
    "intercept",
    paste0("beta_", coef_names)
  )

  dplyr::bind_cols(
    tibble::tibble(
      "{id_col}" := rolled[[id_col]],
      "{date_col}" := rolled[["period_date"]]
    ),
    tibble::as_tibble(betas)
  )
}

#' Map a date-time vector to an integer period counter
#'
#' Builds a monotonically increasing integer that advances by one per period
#' unit, defining rolling calendar windows unambiguously. Indexing months by
#' `year * 12 + month` (rather than by the calendar date) sidesteps
#' end-of-month arithmetic.
#'
#' @param x A date or date-time vector.
#' @param period One of `"month"`, `"day"`, `"hour"`, `"minute"`, `"second"`.
#'
#' @returns A numeric vector of period counters.
#'
#' @noRd
period_to_index <- function(x, period) {
  switch(
    period,
    month = lubridate::year(x) * 12 + lubridate::month(x),
    day = as.numeric(as.Date(x)),
    hour = floor(as.numeric(as.POSIXct(x)) / 3600),
    minute = floor(as.numeric(as.POSIXct(x)) / 60),
    second = floor(as.numeric(as.POSIXct(x)))
  )
}

#' Solve a batch of closed-form OLS systems
#'
#' Reconstructs the symmetric moment matrix `X'X` and vector `X'y` for each row
#' of aggregated cumulants and solves the normal equations. Singular systems
#' yield a row of `NA`s.
#'
#' @param xx A matrix of summed upper-triangular cross-products, one row per
#'   window.
#' @param xy A matrix of summed design-times-response terms, one row per window.
#' @param pairs A two-column matrix mapping each cross-product column to its
#'   `(row, col)` position in the moment matrix.
#' @param num_coefs The number of coefficients (dimension of the system).
#'
#' @returns A matrix of coefficients with one row per window.
#'
#' @noRd
solve_moment_systems <- function(xx, xy, pairs, num_coefs) {
  betas <- matrix(NA_real_, nrow = nrow(xx), ncol = num_coefs)
  rows <- pairs[, "row"]
  cols <- pairs[, "col"]
  for (i in seq_len(nrow(xx))) {
    moment <- matrix(0, num_coefs, num_coefs)
    moment[cbind(rows, cols)] <- xx[i, ]
    moment[cbind(cols, rows)] <- xx[i, ]
    betas[i, ] <- tryCatch(
      solve(moment, xy[i, ]),
      error = function(e) rep(NA_real_, num_coefs)
    )
  }
  betas
}
