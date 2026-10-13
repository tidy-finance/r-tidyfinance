# Drop-in alternatives to compute_portfolio_returns() for the benchmark.
#
# compute_portfolio_returns_fast() has the same signature and returns the same
# result as tidyfinance::compute_portfolio_returns(). Argument validation, the
# messages, and the completion of the output panel are copied from the
# original; only the core (dropping missing sorting variables, capping weights,
# assigning portfolios, aggregating returns) is delegated to an `engine`:
#
# - "cpp11":      C++ kernels (portfolio_sorts_core.h) via cpp11 bindings.
# - "rcpp":       the same C++ kernels via Rcpp bindings.
# - "data.table": data.table grouping, calling the unchanged assign_portfolio()
#                 and compute_breakpoints() of the package for every date.
# - "dplyr":      dplyr, but without the per-group overhead of the original.
#
# Calls the fast path cannot reproduce exactly (custom breakpoint functions,
# invalid options or arguments, duplicated firm-months at rebalancing dates)
# are passed on to the original function, so they behave as before.
#
# Requires setup.R to be sourced first (it provides `tf`).

# cpp_source() compiles a copy of the file inside a temporary package, so the
# shared header has to be placed next to that copy.
compile_cpp11 <- function(file, header, env = parent.frame()) {
  dir <- tempfile("cpp11-")
  dir.create(file.path(dir, "src"), recursive = TRUE)
  file.copy(header, file.path(dir, "src"))
  cpp11::cpp_source(file, env = env, cxx_std = "CXX17", dir = dir)
}

compile_cpp11(
  file.path(bench_dir, "portfolio_sorts_cpp11.cpp"),
  file.path(bench_dir, "portfolio_sorts_core.h")
)
Rcpp::sourceCpp(file.path(bench_dir, "portfolio_sorts_rcpp.cpp"))

kernels_cpp11 <- list(
  cap_weights = cap_weights_cpp11,
  assign_portfolios = assign_portfolios_cpp11,
  carry_forward = carry_forward_cpp11,
  aggregate_univariate = aggregate_univariate_cpp11,
  aggregate_bivariate = aggregate_bivariate_cpp11
)

kernels_rcpp <- list(
  cap_weights = cap_weights_rcpp,
  assign_portfolios = assign_portfolios_rcpp,
  carry_forward = carry_forward_rcpp,
  aggregate_univariate = aggregate_univariate_rcpp,
  aggregate_bivariate = aggregate_bivariate_rcpp
)

compute_portfolio_returns_fast <- function(
  data,
  sorting_variables,
  sorting_method,
  rebalancing_month = NULL,
  breakpoint_options_main,
  breakpoint_options_secondary = NULL,
  breakpoint_function_main = tf$compute_breakpoints,
  breakpoint_function_secondary = tf$compute_breakpoints,
  min_portfolio_size = 1L,
  cap_weight = 0.8,
  data_options = NULL,
  quiet = FALSE,
  engine = c("cpp11", "rcpp", "data.table", "dplyr")
) {
  engine <- match.arg(engine)

  if (
    !fast_path_supported(
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
      quiet
    )
  ) {
    return(tf$compute_portfolio_returns(
      data,
      sorting_variables = sorting_variables,
      sorting_method = sorting_method,
      rebalancing_month = rebalancing_month,
      breakpoint_options_main = breakpoint_options_main,
      breakpoint_options_secondary = breakpoint_options_secondary,
      breakpoint_function_main = breakpoint_function_main,
      breakpoint_function_secondary = breakpoint_function_secondary,
      min_portfolio_size = min_portfolio_size,
      cap_weight = cap_weight,
      data_options = data_options,
      quiet = quiet
    ))
  }

  # Validation and messages as in compute_portfolio_returns() ---------------

  if (is.null(data_options)) {
    data_options <- tf$data_options()
  }

  if (
    (sorting_method %in% c("bivariate-dependent", "bivariate-independent")) &&
      is.null(breakpoint_options_secondary)
  ) {
    cli::cli_warn(
      "No 'breakpoint_options_secondary' specified in bivariate sort."
    )
  }

  date_col <- data_options$date
  w_col <- data_options$mktcap_lag
  w_capped_col <- paste0(w_col, "_capped")

  mktcap_lag_missing <- !(w_col %in% colnames(data))
  if (mktcap_lag_missing) {
    data[[w_col]] <- 1L
  }

  all_dates <- unique(data[[date_col]])

  keep <- vctrs::vec_detect_complete(data[sorting_variables])

  tf$check_new_col(data, w_capped_col)

  if (!any(keep)) {
    if (!quiet) {
      cli::cli_inform(
        paste0(
          "Returning an empty panel: all observations were filtered out ",
          "(n() <= {min_portfolio_size} on every date)."
        )
      )
    }

    empty_result <- tibble::tibble(
      portfolio = integer(0L),
      !!date_col := as.Date(character(0L)),
      ret_excess_ew = numeric(0L)
    )
    if (!mktcap_lag_missing) {
      empty_result <- empty_result |>
        tibble::add_column(
          ret_excess_vw = numeric(0L),
          .before = "ret_excess_ew"
        ) |>
        tibble::add_column(
          ret_excess_vw_capped = numeric(0L),
          .before = "ret_excess_ew"
        )
    }
    return(empty_result)
  }

  if (sorting_method == "univariate") {
    tf$check_new_col(data, "portfolio")
    if (
      !is.null(rebalancing_month) &&
        !any(month(data[[date_col]][keep]) == rebalancing_month)
    ) {
      cli::cli_abort(
        paste0(
          "No observations match {.arg rebalancing_month} = ",
          "{rebalancing_month}. Check that the data contains dates in ",
          "the specified rebalancing month."
        )
      )
    }
  } else {
    tf$check_new_col(data, c("portfolio_secondary", "portfolio_main"))
  }

  # Engine ------------------------------------------------------------------

  engine_fun <- switch(
    engine,
    "cpp11" = \(...) engine_compiled(..., kernels = kernels_cpp11),
    "rcpp" = \(...) engine_compiled(..., kernels = kernels_rcpp),
    "data.table" = engine_data_table,
    "dplyr" = engine_dplyr
  )

  portfolio_returns <- engine_fun(
    data = data,
    keep = keep,
    sorting_variables = sorting_variables,
    sorting_method = sorting_method,
    rebalancing_month = rebalancing_month,
    breakpoint_options_main = breakpoint_options_main,
    breakpoint_options_secondary = breakpoint_options_secondary,
    min_portfolio_size = min_portfolio_size,
    cap_weight = cap_weight,
    data_options = data_options,
    all_dates = all_dates
  )

  # Output panel as in compute_portfolio_returns() --------------------------

  portfolio_returns <- portfolio_returns[!is.na(portfolio_returns$portfolio), ]

  if (!is.null(rebalancing_month)) {
    matching_dates <- all_dates[month(all_dates) == rebalancing_month]
    if (length(matching_dates) == 0L) {
      cli::cli_abort(
        "No dates in data match for rebalancing month = {rebalancing_month}."
      )
    }
    first_rebalancing_date <- min(matching_dates)
    all_dates <- all_dates[all_dates >= first_rebalancing_date]
  }

  all_portfolios <- sort(unique(portfolio_returns$portfolio))

  complete_panel <- tidyr::expand_grid(
    portfolio = all_portfolios,
    !!date_col := all_dates
  )

  return_cols <- if (mktcap_lag_missing) {
    "ret_excess_ew"
  } else {
    c("ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped")
  }

  portfolio_returns <- complete_panel |>
    dplyr::left_join(portfolio_returns, by = c("portfolio", date_col)) |>
    dplyr::select(dplyr::all_of(c("portfolio", date_col, return_cols)))

  n_missing <- sum(is.na(portfolio_returns[["ret_excess_ew"]]))

  if (!quiet && n_missing > 0L) {
    cli::cli_inform(
      paste0(
        "Returning a complete panel with {n_missing} missing value{?s} ",
        "in factor returns due to insufficient observations ",
        "(fewer than {min_portfolio_size} firm{?s} per (portfolio, date) ",
        "cross-section)."
      )
    )
  }

  portfolio_returns
}

# Fast-path eligibility ------------------------------------------------------

fast_path_supported <- function(
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
  quiet
) {
  if (is.null(data_options)) {
    data_options <- tf$data_options()
  }
  if (!inherits(data_options, "tidyfinance_data_options")) {
    return(FALSE)
  }
  if (!is.data.frame(data) || !rlang::is_bool(quiet)) {
    return(FALSE)
  }
  if (!rlang::is_string(sorting_method)) {
    return(FALSE)
  }

  n_vars <- switch(
    sorting_method,
    "univariate" = 1L,
    "bivariate-dependent" = 2L,
    "bivariate-independent" = 2L,
    return(FALSE)
  )
  if (!is.character(sorting_variables) || length(sorting_variables) != n_vars) {
    return(FALSE)
  }

  if (
    !is.numeric(cap_weight) ||
      length(cap_weight) != 1L ||
      is.na(cap_weight) ||
      cap_weight < 0 ||
      cap_weight > 1
  ) {
    return(FALSE)
  }
  if (
    !is.numeric(min_portfolio_size) ||
      length(min_portfolio_size) != 1L ||
      is.na(min_portfolio_size)
  ) {
    return(FALSE)
  }
  if (
    !is.null(rebalancing_month) &&
      (!is.numeric(rebalancing_month) ||
        length(rebalancing_month) != 1L ||
        is.na(rebalancing_month) ||
        rebalancing_month < 1L ||
        rebalancing_month > 12L)
  ) {
    return(FALSE)
  }

  columns <- c(
    sorting_variables,
    data_options$id,
    data_options$date,
    data_options$ret_excess
  )
  if (!all(columns %in% colnames(data))) {
    return(FALSE)
  }
  numeric_columns <- c(
    sorting_variables,
    data_options$ret_excess,
    intersect(data_options$mktcap_lag, colnames(data))
  )
  if (!all(vapply(data[numeric_columns], is.numeric, logical(1L)))) {
    return(FALSE)
  }
  if (!inherits(data[[data_options$date]], "Date")) {
    return(FALSE)
  }

  bivariate <- n_vars == 2L
  is_default <- function(f) identical(f, tf$compute_breakpoints)
  if (!is_default(breakpoint_function_main)) {
    return(FALSE)
  }
  if (bivariate && !is_default(breakpoint_function_secondary)) {
    return(FALSE)
  }
  if (!fast_breakpoints_supported(breakpoint_options_main, data, data_options)) {
    return(FALSE)
  }
  if (
    bivariate &&
      !fast_breakpoints_supported(
        breakpoint_options_secondary,
        data,
        data_options
      )
  ) {
    return(FALSE)
  }

  # The original errors on duplicated firm-months at rebalancing dates.
  if (!is.null(rebalancing_month)) {
    rebalancing <- month(data[[data_options$date]]) == rebalancing_month &
      vctrs::vec_detect_complete(data[sorting_variables])
    if (
      vctrs::vec_duplicate_any(
        data[rebalancing, c(data_options$id, data_options$date)]
      )
    ) {
      return(FALSE)
    }
  }

  TRUE
}

fast_breakpoints_supported <- function(options, data, data_options) {
  if (!is.list(options)) {
    return(FALSE)
  }
  n_portfolios <- options$n_portfolios
  percentiles <- options$percentiles
  if (is.null(n_portfolios) == is.null(percentiles)) {
    return(FALSE)
  }
  if (!is.null(n_portfolios) && (length(n_portfolios) != 1L || n_portfolios <= 1L)) {
    return(FALSE)
  }
  if (!is.null(percentiles) && (length(percentiles) == 0L || anyNA(percentiles))) {
    return(FALSE)
  }
  if (
    !is.null(options$breakpoints_exchanges) &&
      !(data_options$exchange %in% colnames(data))
  ) {
    return(FALSE)
  }
  if (
    !is.null(options$breakpoints_min_size_threshold) &&
      !(data_options$mktcap_lag %in% colnames(data))
  ) {
    return(FALSE)
  }
  TRUE
}

# Shared helpers -------------------------------------------------------------

breakpoint_probs <- function(options) {
  if (!is.null(options$n_portfolios)) {
    seq(0, 1, length.out = options$n_portfolios + 1L)
  } else {
    c(0, options$percentiles, 1)
  }
}

# Columns compute_breakpoints() reads for a given set of options.
breakpoint_columns <- function(sorting_variable, options, data_options) {
  c(
    sorting_variable,
    if (!is.null(options$breakpoints_exchanges)) data_options$exchange,
    if (!is.null(options$breakpoints_min_size_threshold)) {
      data_options$mktcap_lag
    }
  )
}

# Engine: compiled kernels -----------------------------------------------------

# `kernels` is kernels_cpp11 or kernels_rcpp: the same C++ code behind either
# binding.
engine_compiled <- function(
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
  all_dates,
  kernels
) {
  date_col <- data_options$date
  id_col <- data_options$id

  dates <- data[[date_col]][keep]
  date_idx <- match(unclass(dates), unclass(all_dates))
  n_dates <- length(all_dates)

  w <- as.double(data[[data_options$mktcap_lag]][keep])
  w_capped <- kernels$cap_weights(w, date_idx, n_dates, cap_weight)
  w[is.na(w)] <- 0
  ret <- as.double(data[[data_options$ret_excess]][keep])

  assign <- function(variable, options, rows, group, n_groups) {
    exchange_keep <- if (!is.null(options$breakpoints_exchanges)) {
      data[[data_options$exchange]][keep][rows] %in%
        options$breakpoints_exchanges
    } else {
      logical(0L)
    }
    use_size <- !is.null(options$breakpoints_min_size_threshold)
    probs <- breakpoint_probs(options)
    n_portfolios <- length(probs) - 1L
    result <- kernels$assign_portfolios(
      x = as.double(data[[variable]][keep][rows]),
      group = group,
      n_groups = n_groups,
      exchange_keep = exchange_keep,
      mktcap = if (use_size) w[rows] else numeric(0L),
      size_threshold = if (use_size) {
        options$breakpoints_min_size_threshold
      } else {
        NA_real_
      },
      probs = probs,
      smooth_bunching = isTRUE(options$smooth_bunching),
      use_percentiles = !is.null(options$percentiles),
      probs_both_edges = seq(0, 1, length.out = n_portfolios - 1L),
      probs_one_edge = seq(0, 1, length.out = n_portfolios)
    )
    signal_assignment_warnings(result$warnings)
    result$portfolio
  }

  # Portfolios are formed on every date or only at the rebalancing dates.
  rows <- if (is.null(rebalancing_month)) {
    seq_along(dates)
  } else {
    which(month(dates) == rebalancing_month)
  }
  group <- date_idx[rows]

  if (sorting_method == "univariate") {
    main <- assign(sorting_variables, breakpoint_options_main, rows, group, n_dates)
    secondary <- NULL
  } else {
    secondary <- assign(
      sorting_variables[2L],
      breakpoint_options_secondary,
      rows,
      group,
      n_dates
    )
    if (sorting_method == "bivariate-dependent") {
      n_slots <- max(
        length(breakpoint_probs(breakpoint_options_secondary)),
        secondary + 1L,
        na.rm = TRUE
      )
      slot <- ifelse(is.na(secondary), n_slots, secondary)
      main <- assign(
        sorting_variables[1L],
        breakpoint_options_main,
        rows,
        (group - 1L) * n_slots + slot,
        n_dates * n_slots
      )
    } else {
      main <- assign(
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
    match_idx <- kernels$carry_forward(
      id = match(ids, rebalancing_ids),
      date = as.double(dates),
      rebal_id = match(ids[rows], rebalancing_ids),
      rebal_date = as.double(rebalancing_dates),
      rebal_upper = as.double(rebalancing_dates + months(12)),
      n_ids = length(rebalancing_ids)
    )
    main <- main[match_idx]
    if (!is.null(secondary)) secondary <- secondary[match_idx]
  }

  n_main <- max(length(breakpoint_probs(breakpoint_options_main)) - 1L, main, na.rm = TRUE)
  if (sorting_method == "univariate") {
    result <- kernels$aggregate_univariate(
      main,
      date_idx,
      n_main,
      n_dates,
      ret,
      w,
      w_capped,
      as.double(min_portfolio_size)
    )
  } else {
    n_secondary <- max(
      length(breakpoint_probs(breakpoint_options_secondary)) - 1L,
      secondary,
      na.rm = TRUE
    )
    result <- kernels$aggregate_bivariate(
      main,
      secondary,
      date_idx,
      n_main,
      n_secondary,
      n_dates,
      ret,
      w,
      w_capped,
      as.double(min_portfolio_size)
    )
  }

  exists <- result$n > 0L
  tibble::tibble(
    portfolio = rep(seq_len(n_main), each = n_dates)[exists],
    !!date_col := rep(all_dates, times = n_main)[exists],
    ret_excess_vw = result$ret_excess_vw[exists],
    ret_excess_ew = result$ret_excess_ew[exists],
    ret_excess_vw_capped = result$ret_excess_vw_capped[exists]
  )
}

# Re-signal the warnings of assign_portfolio() and compute_breakpoints() that
# occurred in at least one group.
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
}

# Engine: data.table ---------------------------------------------------------

engine_data_table <- function(
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
  # Internal columns are prefixed with tf_ to avoid clashes with the columns
  # compute_breakpoints() reads, which keep their names.
  tf_g <- tf_w <- tf_wc <- tf_ret <- tf_p <- tf_s <- tf_sk <- NULL
  tf_n <- tf_vw <- tf_ew <- tf_vwc <- tf_nf <- tf_ub <- tf_d <- tf_id <- NULL
  `.` <- NULL

  date_col <- data_options$date
  w_col <- data_options$mktcap_lag
  cols_main <- breakpoint_columns(
    sorting_variables[1L],
    breakpoint_options_main,
    data_options
  )
  cols_secondary <- if (length(sorting_variables) == 2L) {
    breakpoint_columns(
      sorting_variables[2L],
      breakpoint_options_secondary,
      data_options
    )
  }

  dates <- data[[date_col]][keep]
  dt <- data.table::data.table(
    tf_g = match(unclass(dates), unclass(all_dates)),
    tf_d = as.double(dates),
    tf_id = data[[data_options$id]][keep],
    tf_ret = data[[data_options$ret_excess]][keep],
    tf_w = data[[w_col]][keep]
  )
  for (col in setdiff(unique(c(cols_main, cols_secondary)), w_col)) {
    data.table::set(dt, j = col, value = data[[col]][keep])
  }

  dt[,
    tf_wc := pmin(tf_w, stats::quantile(tf_w, cap_weight, na.rm = TRUE)),
    by = tf_g
  ]
  dt[is.na(tf_w), c("tf_w", "tf_wc") := list(0, 0)]
  if (w_col %in% c(cols_main, cols_secondary)) {
    dt[, (w_col) := tf_w]
  }

  assign <- function(sd, variable, options) {
    tf$assign_portfolio(sd, variable, options, tf$compute_breakpoints, data_options)
  }

  sorted <- if (is.null(rebalancing_month)) {
    dt
  } else {
    dt[month(dates) == rebalancing_month]
  }

  if (sorting_method == "univariate") {
    sorted[,
      tf_p := assign(.SD, sorting_variables, breakpoint_options_main),
      by = tf_g,
      .SDcols = cols_main
    ]
  } else {
    sorted[,
      tf_s := assign(.SD, sorting_variables[2L], breakpoint_options_secondary),
      by = tf_g,
      .SDcols = cols_secondary
    ]
    by_main <- if (sorting_method == "bivariate-dependent") {
      c("tf_g", "tf_s")
    } else {
      "tf_g"
    }
    sorted[,
      tf_p := assign(.SD, sorting_variables[1L], breakpoint_options_main),
      by = by_main,
      .SDcols = cols_main
    ]
  }

  if (!is.null(rebalancing_month)) {
    # Latest rebalancing date of the same firm, within twelve months.
    sorted[, tf_ub := as.double(all_dates[tf_g] + months(12))]
    carried <- sorted[,
      intersect(c("tf_id", "tf_d", "tf_p", "tf_s", "tf_ub"), names(sorted)),
      with = FALSE
    ][dt[, c("tf_id", "tf_d")], on = c("tf_id", "tf_d"), roll = TRUE]
    valid <- !is.na(carried$tf_ub) & carried$tf_d <= carried$tf_ub
    dt[, tf_p := data.table::fifelse(valid, carried$tf_p, NA_integer_)]
    if (sorting_method != "univariate") {
      dt[, tf_s := data.table::fifelse(valid, carried$tf_s, NA_integer_)]
    }
  }

  if (sorting_method == "univariate") {
    result <- dt[
      !is.na(tf_p),
      .(
        tf_n = .N,
        tf_vw = if (.N < min_portfolio_size || sum(tf_w) == 0) {
          NA_real_
        } else {
          stats::weighted.mean(tf_ret, tf_w)
        },
        tf_ew = if (.N < min_portfolio_size) NA_real_ else base::mean(tf_ret),
        tf_vwc = if (.N < min_portfolio_size || sum(tf_wc) == 0) {
          NA_real_
        } else {
          stats::weighted.mean(tf_ret, tf_wc)
        }
      ),
      by = .(tf_p, tf_g)
    ]
  } else {
    # Secondary portfolios in ascending order with NA last, as in dplyr.
    dt[, tf_sk := data.table::fifelse(is.na(tf_s), .Machine$integer.max, tf_s)]
    cells <- dt[
      !is.na(tf_p),
      .(
        tf_n = .N,
        tf_nf = sum(!is.na(tf_s)),
        tf_vw = if (sum(tf_w) == 0) {
          NA_real_
        } else {
          stats::weighted.mean(tf_ret, tf_w)
        },
        tf_ew = base::mean(tf_ret),
        tf_vwc = if (sum(tf_wc) == 0) {
          NA_real_
        } else {
          stats::weighted.mean(tf_ret, tf_wc)
        }
      ),
      keyby = .(tf_p, tf_sk, tf_g)
    ]
    mean_or_na <- function(x, n_firms) {
      if (n_firms == 0L || n_firms < min_portfolio_size) {
        return(NA_real_)
      }
      m <- base::mean(x, na.rm = TRUE)
      if (is.nan(m)) NA_real_ else m
    }
    result <- cells[,
      .(
        tf_vw = mean_or_na(tf_vw, sum(tf_nf)),
        tf_ew = mean_or_na(tf_ew, sum(tf_nf)),
        tf_vwc = mean_or_na(tf_vwc, sum(tf_nf))
      ),
      by = .(tf_p, tf_g)
    ]
  }

  tibble::tibble(
    portfolio = result$tf_p,
    !!date_col := all_dates[result$tf_g],
    ret_excess_vw = result$tf_vw,
    ret_excess_ew = result$tf_ew,
    ret_excess_vw_capped = result$tf_vwc
  )
}

# Engine: dplyr --------------------------------------------------------------

engine_dplyr <- function(
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
  ret_col <- data_options$ret_excess
  w_col <- data_options$mktcap_lag
  w_capped_col <- paste0(w_col, "_capped")

  cols_main <- breakpoint_columns(
    sorting_variables[1L],
    breakpoint_options_main,
    data_options
  )
  cols_secondary <- if (length(sorting_variables) == 2L) {
    breakpoint_columns(
      sorting_variables[2L],
      breakpoint_options_secondary,
      data_options
    )
  }

  # Only the columns the sort needs, so that slicing and grouping are cheap.
  needed <- unique(c(
    id_col,
    date_col,
    ret_col,
    w_col,
    cols_main,
    cols_secondary
  ))
  data <- vctrs::vec_slice(
    tibble::new_tibble(unclass(data)[needed]),
    keep
  )

  assign <- function(sd, variable, options) {
    tf$assign_portfolio(sd, variable, options, tf$compute_breakpoints, data_options)
  }

  # Capped weights, zeroed missing market caps, and the first portfolio
  # assignment in a single grouped mutate.
  data <- data |>
    dplyr::group_by(.data[[date_col]]) |>
    dplyr::mutate(
      !!w_capped_col := pmin(
        .data[[w_col]],
        stats::quantile(.data[[w_col]], cap_weight, na.rm = TRUE)
      ),
      !!w_capped_col := dplyr::if_else(
        is.na(.data[[w_col]]),
        0,
        .data[[w_capped_col]]
      ),
      !!w_col := dplyr::if_else(is.na(.data[[w_col]]), 0, .data[[w_col]])
    )

  portfolios <- if (is.null(rebalancing_month)) {
    data
  } else {
    dplyr::filter(data, month(.data[[date_col]]) == rebalancing_month)
  }

  if (sorting_method == "univariate") {
    portfolios <- portfolios |>
      dplyr::mutate(
        portfolio = assign(
          dplyr::pick(dplyr::all_of(cols_main)),
          sorting_variables,
          breakpoint_options_main
        )
      )
  } else {
    portfolios <- portfolios |>
      dplyr::mutate(
        portfolio_secondary = assign(
          dplyr::pick(dplyr::all_of(cols_secondary)),
          sorting_variables[2L],
          breakpoint_options_secondary
        )
      )
    if (sorting_method == "bivariate-dependent") {
      portfolios <- portfolios |>
        dplyr::group_by(.data$portfolio_secondary, .add = TRUE)
    }
    portfolios <- portfolios |>
      dplyr::mutate(
        portfolio_main = assign(
          dplyr::pick(dplyr::all_of(cols_main)),
          sorting_variables[1L],
          breakpoint_options_main
        )
      )
  }
  portfolios <- dplyr::ungroup(portfolios)

  if (!is.null(rebalancing_month)) {
    portfolios <- tf$join_rebalanced_portfolios(
      dplyr::ungroup(data),
      portfolios |>
        dplyr::select(
          dplyr::any_of(c(
            id_col,
            date_col,
            "portfolio",
            "portfolio_main",
            "portfolio_secondary"
          ))
        ),
      date_col,
      id_col
    )
  }

  # Return statistics are computed for every group and set to NA afterwards
  # in one vectorised step, instead of a scalar if_else() per group.
  if (sorting_method == "univariate") {
    portfolios |>
      dplyr::filter(!is.na(.data$portfolio)) |>
      dplyr::summarise(
        n = dplyr::n(),
        sum_w = sum(.data[[w_col]]),
        sum_wc = sum(.data[[w_capped_col]]),
        ret_excess_vw = stats::weighted.mean(.data[[ret_col]], .data[[w_col]]),
        ret_excess_ew = mean(.data[[ret_col]]),
        ret_excess_vw_capped = stats::weighted.mean(
          .data[[ret_col]],
          .data[[w_capped_col]]
        ),
        .by = c("portfolio", dplyr::all_of(date_col))
      ) |>
      dplyr::mutate(
        too_small = .data$n < min_portfolio_size,
        ret_excess_vw = dplyr::if_else(
          .data$too_small | .data$sum_w == 0,
          NA_real_,
          .data$ret_excess_vw
        ),
        ret_excess_ew = dplyr::if_else(
          .data$too_small,
          NA_real_,
          .data$ret_excess_ew
        ),
        ret_excess_vw_capped = dplyr::if_else(
          .data$too_small | .data$sum_wc == 0,
          NA_real_,
          .data$ret_excess_vw_capped
        )
      ) |>
      dplyr::select(
        "portfolio",
        dplyr::all_of(date_col),
        "ret_excess_vw",
        "ret_excess_ew",
        "ret_excess_vw_capped"
      )
  } else {
    portfolios |>
      dplyr::filter(!is.na(.data$portfolio_main)) |>
      dplyr::summarise(
        n_firms = sum(!is.na(.data$portfolio_secondary)),
        sum_w = sum(.data[[w_col]]),
        sum_wc = sum(.data[[w_capped_col]]),
        ret_excess_vw = stats::weighted.mean(.data[[ret_col]], .data[[w_col]]),
        ret_excess_ew = mean(.data[[ret_col]]),
        ret_excess_vw_capped = stats::weighted.mean(
          .data[[ret_col]],
          .data[[w_capped_col]]
        ),
        .by = c("portfolio_main", "portfolio_secondary", dplyr::all_of(date_col))
      ) |>
      dplyr::mutate(
        ret_excess_vw = dplyr::if_else(.data$sum_w == 0, NA_real_, .data$ret_excess_vw),
        ret_excess_vw_capped = dplyr::if_else(
          .data$sum_wc == 0,
          NA_real_,
          .data$ret_excess_vw_capped
        )
      ) |>
      # Average over the secondary portfolios in ascending order, NA last.
      dplyr::arrange(.data$portfolio_secondary) |>
      dplyr::summarise(
        n_firms = sum(.data$n_firms),
        dplyr::across(
          c("ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped"),
          \(x) mean(x, na.rm = TRUE)
        ),
        .by = c("portfolio_main", dplyr::all_of(date_col))
      ) |>
      dplyr::rename(portfolio = "portfolio_main") |>
      dplyr::mutate(
        dplyr::across(
          c("ret_excess_vw", "ret_excess_ew", "ret_excess_vw_capped"),
          \(x) {
            dplyr::if_else(
              is.nan(x) | .data$n_firms == 0 | .data$n_firms < min_portfolio_size,
              NA_real_,
              x
            )
          }
        )
      ) |>
      dplyr::select(-"n_firms")
  }
}
