# Compiled ("cpp11") alternative engine for compute_portfolio_returns().
#
# The R baseline in R/compute_portfolio_returns.R stays the default and is
# untouched. This file holds the opt-in compiled engine: the C++ kernels in
# src/portfolio_sorts_core.h replace the per-group R work (capped weights,
# portfolio assignment, carrying annual assignments forward, and the return
# aggregation) while the argument validation, the conditions, and the
# completion of the output panel remain the baseline's.
#
# The engine was conceived and contributed by Claude (Anthropic). It is only
# reached when cpp11_engine_reject_reason() finds nothing that could make it
# diverge from the baseline; every other call keeps running the R body.

#' Number of portfolios implied by a set of breakpoint options
#'
#' @param options A `tidyfinance_breakpoint_options` object.
#'
#' @returns A single integer.
#' @noRd
breakpoint_n_portfolios <- function(options) {
  length(breakpoint_probs(options)) - 1L
}

#' Probabilities `compute_breakpoints()` passes to `quantile()`
#'
#' @param options A `tidyfinance_breakpoint_options` object.
#'
#' @returns A numeric vector of probabilities in `[0, 1]`.
#' @noRd
breakpoint_probs <- function(options) {
  if (!is.null(options$n_portfolios)) {
    seq(0, 1, length.out = options$n_portfolios + 1L)
  } else {
    c(0, options$percentiles, 1)
  }
}

#' Check that a value is usable as a probability by the kernels
#'
#' `quantile7()` in the kernels clamps out-of-range probabilities, but the
#' baseline's `stats::quantile()` raises an error instead, so anything outside
#' `[0, 1]` (or missing, or infinite) must keep the R path.
#'
#' @param x A numeric vector.
#'
#' @returns `TRUE` when every element is finite and in `[0, 1]`.
#' @noRd
is_valid_probs <- function(x) {
  is.numeric(x) &&
    length(x) > 0L &&
    !anyNA(x) &&
    all(is.finite(x)) &&
    all(x >= 0) &&
    all(x <= 1)
}

#' Reason the compiled engine must not be used for this call
#'
#' The gate is deliberately conservative: it refuses every call whose result,
#' conditions, or errors the compiled kernels are not known to reproduce, and
#' refuses every input that the kernels are not hardened against. Each reject
#' is a fall back to the R baseline, never an error.
#'
#' @param all_dates The distinct dates of `data`, in order of appearance.
#' @inheritParams compute_portfolio_returns
#'
#' @returns `NULL` when the compiled engine may be used, otherwise a single
#'   string naming the reason it may not.
#' @noRd
cpp11_engine_reject_reason <- function(
  data,
  sorting_variables,
  sorting_method,
  rebalancing_month,
  breakpoint_options_main,
  breakpoint_options_secondary,
  breakpoint_function_main,
  breakpoint_function_secondary,
  min_portfolio_size,
  cap_weight,
  data_options,
  quiet,
  all_dates
) {
  # The kernels hard-code `typedef long double LDOUBLE` so that sum(), mean()
  # and weighted.mean() accumulate exactly as in R's summary.c. R does not
  # expose HAVE_LONG_DOUBLE in its installed headers, so the kernels cannot
  # follow that build flag with an #ifdef; the engine is refused here
  # instead, which is how bit-identity survives CRAN's noLD flavour.
  if (!isTRUE(capabilities("long.double"))) {
    return("this R was built without long double")
  }

  if (!is.data.frame(data)) {
    return("`data` is not a data frame")
  }
  if (!inherits(data_options, "tidyfinance_data_options")) {
    return("`data_options` is not a `tidyfinance_data_options` object")
  }
  if (!rlang::is_bool(quiet)) {
    return("`quiet` is not a single logical")
  }
  if (!rlang::is_string(sorting_method)) {
    return("`sorting_method` is not a single string")
  }

  n_variables <- switch(
    sorting_method,
    "univariate" = 1L,
    "bivariate-dependent" = 2L,
    "bivariate-independent" = 2L,
    return("`sorting_method` is not a supported sorting method")
  )
  bivariate <- n_variables == 2L
  if (
    !is.character(sorting_variables) ||
      length(sorting_variables) != n_variables ||
      anyNA(sorting_variables)
  ) {
    return("`sorting_variables` does not match `sorting_method`")
  }

  if (
    !is.numeric(cap_weight) ||
      length(cap_weight) != 1L ||
      !is.finite(cap_weight) ||
      cap_weight < 0 ||
      cap_weight > 1
  ) {
    return("`cap_weight` is not a single finite number in [0, 1]")
  }
  if (
    !is.numeric(min_portfolio_size) ||
      length(min_portfolio_size) != 1L ||
      !is.finite(min_portfolio_size)
  ) {
    return("`min_portfolio_size` is not a single finite number")
  }
  if (
    !is.null(rebalancing_month) &&
      (!is.numeric(rebalancing_month) ||
        length(rebalancing_month) != 1L ||
        !is.finite(rebalancing_month) ||
        rebalancing_month < 1L ||
        rebalancing_month > 12L)
  ) {
    return("`rebalancing_month` is not a single month between 1 and 12")
  }

  id_col <- data_options$id
  date_col <- data_options$date
  ret_col <- data_options$ret_excess
  w_col <- data_options$mktcap_lag

  required_columns <- c(sorting_variables, id_col, date_col, ret_col)
  if (!all(required_columns %in% colnames(data))) {
    return("`data` is missing a required column")
  }
  # The kernels read doubles; a non-numeric column would be coerced here and
  # silently sorted on something else than the baseline sorts on.
  numeric_columns <- c(
    sorting_variables,
    ret_col,
    intersect(w_col, colnames(data))
  )
  if (!all(vapply(data[numeric_columns], is.numeric, logical(1L)))) {
    return("a sorting variable, the return, or the weight is not numeric")
  }
  if (!inherits(data[[date_col]], "Date")) {
    return("the date column is not a `Date`")
  }

  if (!identical(breakpoint_function_main, compute_breakpoints)) {
    return("`breakpoint_function_main` is not `compute_breakpoints()`")
  }
  if (
    bivariate &&
      !identical(breakpoint_function_secondary, compute_breakpoints)
  ) {
    return("`breakpoint_function_secondary` is not `compute_breakpoints()`")
  }

  reason <- breakpoint_options_reject_reason(
    breakpoint_options_main,
    data,
    data_options,
    "main"
  )
  if (!is.null(reason)) {
    return(reason)
  }
  if (bivariate) {
    reason <- breakpoint_options_reject_reason(
      breakpoint_options_secondary,
      data,
      data_options,
      "secondary"
    )
    if (!is.null(reason)) {
      return(reason)
    }
  }

  # The aggregation kernels address cells with int ids and refuse a grid that
  # does not fit; keep such a sort on the R path instead of erroring.
  n_cells <- as.double(length(all_dates)) *
    breakpoint_n_portfolios(breakpoint_options_main)
  if (bivariate) {
    n_cells <- n_cells *
      (breakpoint_n_portfolios(breakpoint_options_secondary) + 1)
  }
  if (n_cells > .Machine$integer.max) {
    return("the portfolio-date grid is too large for the compiled engine")
  }

  if (!is.null(rebalancing_month)) {
    keep <- stats::complete.cases(data[sorting_variables])
    rebalancing <- keep & month(data[[date_col]]) == rebalancing_month
    # The baseline joins the annual assignments with
    # `relationship = "many-to-one"` and errors on duplicated firm-months.
    if (anyDuplicated(data[rebalancing, c(id_col, date_col)]) > 0L) {
      return("`data` has duplicated firm-months at the rebalancing dates")
    }
    # With no row to form portfolios from, the baseline returns an empty
    # panel for bivariate sorts while the kernels require a positive firm
    # count. (Univariate sorts abort in both engines, with the same message.)
    if (bivariate && !any(rebalancing)) {
      return("no complete observation falls in `rebalancing_month`")
    }
  }

  NULL
}

#' Reason a set of breakpoint options must keep the R path
#'
#' `compute_portfolio_returns()` accepts a plain list of breakpoint options,
#' and the package's own tests pass plain lists in many places. A plain list
#' has not been through [breakpoint_options()]'s validation, and neither has
#' an object whose elements were replaced after it was built, so every value
#' the kernels turn into a probability or a count is checked again here.
#'
#' @param options The breakpoint options to check.
#' @param data The sorting data.
#' @param data_options A `tidyfinance_data_options` object.
#' @param which Either `"main"` or `"secondary"`, used in the reason.
#'
#' @returns `NULL` when the options are supported, otherwise a single string.
#' @noRd
breakpoint_options_reject_reason <- function(
  options,
  data,
  data_options,
  which
) {
  label <- paste0("`breakpoint_options_", which, "`")

  if (!inherits(options, "tidyfinance_breakpoint_options")) {
    return(paste(label, "is not a `tidyfinance_breakpoint_options` object"))
  }

  n_portfolios <- options$n_portfolios
  percentiles <- options$percentiles
  if (is.null(n_portfolios) == is.null(percentiles)) {
    return(paste(
      label,
      "does not set exactly one of `n_portfolios` and `percentiles`"
    ))
  }
  if (!is.null(n_portfolios)) {
    if (
      !is.numeric(n_portfolios) ||
        length(n_portfolios) != 1L ||
        !is.finite(n_portfolios) ||
        n_portfolios <= 1 ||
        n_portfolios %% 1 != 0
    ) {
      return(paste(label, "has an unsupported `n_portfolios`"))
    }
  } else {
    if (!is_valid_probs(percentiles) || is.unsorted(percentiles)) {
      return(paste(label, "has an unsupported `percentiles`"))
    }
  }

  smooth_bunching <- options$smooth_bunching
  if (!rlang::is_bool(smooth_bunching)) {
    return(paste(label, "has a non-logical `smooth_bunching`"))
  }

  exchanges <- options$breakpoints_exchanges
  if (!is.null(exchanges)) {
    if (
      !is.character(exchanges) || length(exchanges) == 0L || anyNA(exchanges)
    ) {
      return(paste(label, "has an unsupported `breakpoints_exchanges`"))
    }
    if (!(data_options$exchange %in% colnames(data))) {
      return(paste(label, "needs an exchange column that `data` lacks"))
    }
  }

  size_threshold <- options$breakpoints_min_size_threshold
  if (!is.null(size_threshold)) {
    if (
      !is_valid_probs(size_threshold) ||
        length(size_threshold) != 1L ||
        size_threshold <= 0 ||
        size_threshold >= 1
    ) {
      return(paste(
        label,
        "has an unsupported `breakpoints_min_size_threshold`"
      ))
    }
    if (!(data_options$mktcap_lag %in% colnames(data))) {
      return(paste(label, "needs a market capitalization column that",
                   "`data` lacks"))
    }
  }

  NULL
}

#' Portfolio returns from the compiled kernels
#'
#' Replaces the per-group R work of [compute_portfolio_returns()] between the
#' validation of the arguments and the completion of the output panel. Only
#' called once [cpp11_engine_reject_reason()] has returned `NULL`.
#'
#' @param keep A logical vector flagging the rows of `data` without a missing
#'   sorting variable, i.e. the rows [tidyr::drop_na()] keeps in the baseline.
#' @param all_dates The distinct dates of `data`, in order of appearance.
#' @inheritParams compute_portfolio_returns
#'
#' @returns A tibble of `portfolio`, the date column, `ret_excess_vw`,
#'   `ret_excess_ew`, and `ret_excess_vw_capped`, with one row per non-empty
#'   portfolio-date cell.
#' @noRd
portfolio_sorts_cpp11 <- function(
  data,
  keep,
  sorting_variables,
  sorting_method,
  rebalancing_month,
  breakpoint_options_main,
  breakpoint_options_secondary,
  min_portfolio_size,
  cap_weight,
  data_options,
  all_dates
) {
  date_col <- data_options$date
  id_col <- data_options$id

  dates <- data[[date_col]][keep]
  date_index <- match(unclass(dates), unclass(all_dates))
  n_dates <- length(all_dates)

  # Capped weights per date before missing market caps become zero, as in the
  # baseline, so that the quantile is not distorted by the zeros.
  weights <- as.double(data[[data_options$mktcap_lag]][keep])
  weights_capped <- cap_weights_cpp11(
    weights,
    date_index,
    n_dates,
    cap_weight
  )
  weights[is.na(weights)] <- 0
  returns <- as.double(data[[data_options$ret_excess]][keep])

  exchanges <- if (data_options$exchange %in% colnames(data)) {
    data[[data_options$exchange]][keep]
  }

  assign_portfolios <- function(variable, options, rows, group, n_groups) {
    exchange_keep <- if (!is.null(options$breakpoints_exchanges)) {
      exchanges[rows] %in% options$breakpoints_exchanges
    } else {
      logical(0L)
    }
    by_size <- !is.null(options$breakpoints_min_size_threshold)
    probs <- breakpoint_probs(options)
    n_portfolios <- length(probs) - 1L
    assigned <- assign_portfolios_cpp11(
      x = as.double(data[[variable]][keep][rows]),
      group = group,
      n_groups = n_groups,
      exchange_keep = exchange_keep,
      mktcap = if (by_size) weights[rows] else numeric(0L),
      size_threshold = if (by_size) {
        as.double(options$breakpoints_min_size_threshold)
      } else {
        NA_real_
      },
      probs = as.double(probs),
      smooth_bunching = isTRUE(options$smooth_bunching),
      use_percentiles = !is.null(options$percentiles),
      probs_both_edges = seq(0, 1, length.out = n_portfolios - 1L),
      probs_one_edge = seq(0, 1, length.out = n_portfolios)
    )
    signal_assignment_warnings(assigned$warnings)
    assigned$portfolio
  }

  # Portfolios are formed on every date, or only at the rebalancing dates.
  rows <- if (is.null(rebalancing_month)) {
    seq_along(dates)
  } else {
    which(month(dates) == rebalancing_month)
  }
  group <- date_index[rows]

  if (sorting_method == "univariate") {
    main <- assign_portfolios(
      sorting_variables,
      breakpoint_options_main,
      rows,
      group,
      n_dates
    )
    secondary <- NULL
  } else {
    secondary <- assign_portfolios(
      sorting_variables[2L],
      breakpoint_options_secondary,
      rows,
      group,
      n_dates
    )
    if (sorting_method == "bivariate-dependent") {
      # The baseline groups the main sort by date and secondary portfolio,
      # with the missing secondary portfolio as the last group.
      n_slots <- max(
        length(breakpoint_probs(breakpoint_options_secondary)),
        secondary + 1L,
        na.rm = TRUE
      )
      slot <- ifelse(is.na(secondary), n_slots, secondary)
      main <- assign_portfolios(
        sorting_variables[1L],
        breakpoint_options_main,
        rows,
        (group - 1L) * n_slots + slot,
        n_dates * n_slots
      )
    } else {
      main <- assign_portfolios(
        sorting_variables[1L],
        breakpoint_options_main,
        rows,
        group,
        n_dates
      )
    }
  }

  if (!is.null(rebalancing_month)) {
    ids <- data[[id_col]][keep]
    rebalancing_ids <- unique(ids[rows])
    rebalancing_dates <- dates[rows]
    carried <- carry_forward_cpp11(
      id = match(ids, rebalancing_ids),
      date = as.double(dates),
      rebal_id = match(ids[rows], rebalancing_ids),
      rebal_date = as.double(rebalancing_dates),
      rebal_upper = as.double(rebalancing_dates + months(12)),
      n_ids = length(rebalancing_ids)
    )
    main <- main[carried]
    if (!is.null(secondary)) {
      secondary <- secondary[carried]
    }
  }

  n_main <- max(
    breakpoint_n_portfolios(breakpoint_options_main),
    main,
    na.rm = TRUE
  )
  if (sorting_method == "univariate") {
    cells <- aggregate_univariate_cpp11(
      main,
      date_index,
      n_main,
      n_dates,
      returns,
      weights,
      weights_capped,
      as.double(min_portfolio_size)
    )
  } else {
    n_secondary <- max(
      breakpoint_n_portfolios(breakpoint_options_secondary),
      secondary,
      na.rm = TRUE
    )
    cells <- aggregate_bivariate_cpp11(
      main,
      secondary,
      date_index,
      n_main,
      n_secondary,
      n_dates,
      returns,
      weights,
      weights_capped,
      as.double(min_portfolio_size)
    )
  }

  # Cells are laid out portfolio-major; empty ones never reach the panel.
  occupied <- cells$n > 0L
  tibble::tibble(
    portfolio = rep(seq_len(n_main), each = n_dates)[occupied],
    !!date_col := rep(all_dates, times = n_main)[occupied],
    ret_excess_vw = cells$ret_excess_vw[occupied],
    ret_excess_ew = cells$ret_excess_ew[occupied],
    ret_excess_vw_capped = cells$ret_excess_vw_capped[occupied]
  )
}

#' Re-signal the warnings the kernels counted
#'
#' The kernels count, per warning, how many groups triggered it, instead of
#' warning inside the loop. Each warning that occurred in at least one group
#' is signalled once here, with the text [assign_portfolio()] and
#' [compute_breakpoints()] use. The baseline signals them once per group, so
#' the compiled engine raises fewer warnings than the R engine whenever more
#' than one date is affected.
#'
#' @param counts The named integer vector of counts returned by the kernel.
#'
#' @returns `NULL`, invisibly; called for the warnings.
#' @noRd
signal_assignment_warnings <- function(counts) {
  if (counts[["smooth_percentiles"]] > 0L) {
    cli::cli_warn(
      paste(
        "{.arg smooth_bunching} is TRUE and equally-spaced portfolios",
        "are returned for non-edge portfolios."
      )
    )
  }
  if (counts[["constant"]] > 0L) {
    cli::cli_warn(
      "The sorting variable is constant and only one portfolio is returned."
    )
  }
  if (counts[["empty"]] > 0L) {
    cli::cli_warn(
      paste(
        "No breakpoints were calculated, likely due to an insufficient number",
        "of observations after filtering for breakpoint exchanges."
      )
    )
  }
  if (counts[["missing"]] > 0L) {
    cli::cli_warn(
      "No portfolios were assigned due to missing breakpoints."
    )
  }
  if (counts[["clusters"]] > 0L) {
    cli::cli_warn(
      paste(
        "The number of portfolios differs from the",
        "specified parameter due to clusters in the sorting variable."
      )
    )
  }
  invisible(NULL)
}
