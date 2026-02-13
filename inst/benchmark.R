# benchmark-compute_portfolio_returns.R
#
# Contains original + optimized implementations, correctness checks, and
# benchmarks across data sizes and all sorting methods.
#
# Run with:
#   Rscript benchmark-compute_portfolio_returns.R
#
# Requirements:
#   install.packages(c("bench", "cli", "dplyr", "lubridate"))

library(bench)
library(cli)
library(dplyr)
library(lubridate)

# =============================================================================
# Test data generator
# =============================================================================
make_panel <- function(n_stocks = 50, n_months = 24, seed = 42) {
  set.seed(seed)
  dates <- seq.Date(
    from = as.Date("2020-01-01"),
    by = "month",
    length.out = n_months
  )
  expand.grid(permno = seq_len(n_stocks), date = dates) |>
    mutate(
      mktcap_lag = runif(n(), 100, 10000),
      ret_excess = rnorm(n(), 0, 0.05),
      size = runif(n(), 1, 100),
      bm = runif(n(), 0.5, 3),
      exchange = sample(c("NYSE", "NASDAQ"), n(), replace = TRUE),
      # Extra columns that make pick(everything()) expensive
      extra1 = rnorm(n()),
      extra2 = rnorm(n()),
      extra3 = rnorm(n()),
      extra4 = rnorm(n()),
      extra5 = rnorm(n()),
      extra6 = rnorm(n())
    ) |>
    arrange(date, permno) |>
    as_tibble()
}

# =============================================================================
# ORIGINAL IMPLEMENTATION (verbatim from source, using pick(everything()))
# =============================================================================
compute_portfolio_returns_original <- function(
  sorting_data,
  sorting_variables,
  sorting_method,
  rebalancing_month = NULL,
  breakpoint_options_main,
  breakpoint_options_secondary = NULL,
  breakpoint_function_main = compute_breakpoints,
  breakpoint_function_secondary = compute_breakpoints,
  min_portfolio_size = 0,
  data_options = NULL
) {
  if (is.null(data_options)) {
    data_options <- data_options()
  }

  if (is.null(sorting_variables) || length(sorting_variables) == 0) {
    cli::cli_abort("You must provide at least one sorting variable.")
  }

  if (
    !sorting_method %in%
      c("univariate", "bivariate-dependent", "bivariate-independent")
  ) {
    cli::cli_abort("Invalid sorting method.")
  }

  if (
    (sorting_method %in% c("bivariate-dependent", "bivariate-independent")) &&
      is.null(breakpoint_options_secondary)
  ) {
    cli::cli_warn(
      "No 'breakpoint_options_secondary' specified in bivariate sort."
    )
  }

  required_columns <- c(
    sorting_variables,
    data_options$date,
    data_options$id,
    data_options$ret_excess
  )
  missing_columns <- setdiff(required_columns, colnames(sorting_data))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "Missing columns: {paste(missing_columns, collapse = ', ')}."
    )
  }

  mktcap_lag_missing <- !(data_options$mktcap_lag %in% colnames(sorting_data))
  if (mktcap_lag_missing) {
    sorting_data$mktcap_lag <- 1
  }

  if (
    !is.null(rebalancing_month) &&
      (rebalancing_month > 12 || rebalancing_month < 1)
  ) {
    cli::cli_abort("Invalid rebalancing_month.")
  }

  if (sorting_method == "univariate") {
    if (length(sorting_variables) > 1) {
      cli::cli_abort("Only provide one sorting variable for univariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio = assign_portfolio(
            data = pick(everything()),
            sorting_variable = sorting_variables,
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        )
    } else {
      portfolio_data <- sorting_data |>
        filter(month(.data[[data_options$date]]) == rebalancing_month) |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio = assign_portfolio(
            data = pick(everything()),
            sorting_variable = sorting_variables,
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        select(all_of(c(data_options$id, data_options$date, "portfolio")))

      portfolio_returns <- sorting_data |>
        rename(
          "..date" = all_of(data_options$date),
          "..id" := all_of(data_options$id)
        ) |>
        left_join(
          portfolio_data |>
            rename("..id" = all_of(data_options$id)) |>
            mutate(
              lower_bound = .data[[data_options$date]],
              upper_bound = .data[[data_options$date]] + months(12)
            ) |>
            select(-all_of(data_options$date)),
          join_by(..id, closest(..date >= lower_bound), ..date < upper_bound),
          relationship = "many-to-one"
        ) |>
        rename("{data_options$date}" := "..date", "{data_options$id}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      group_by(portfolio, .data[[data_options$date]]) |>
      summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(
            .data[[data_options$ret_excess]],
            .data[[data_options$mktcap_lag]]
          )
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(.data[[data_options$ret_excess]])
        ),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-dependent") {
    if (length(sorting_variables) != 2) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio_secondary = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        group_by(.data[[data_options$date]], portfolio_secondary) |>
        mutate(
          portfolio_main = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup()
    } else {
      portfolio_data <- sorting_data |>
        filter(month(.data[[data_options$date]]) == rebalancing_month) |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio_secondary = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        group_by(.data[[data_options$date]], portfolio_secondary) |>
        mutate(
          portfolio_main = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        select(all_of(c(
          data_options$id,
          data_options$date,
          "portfolio_main",
          "portfolio_secondary"
        )))

      portfolio_returns <- sorting_data |>
        rename(
          "..date" = all_of(data_options$date),
          "..id" := all_of(data_options$id)
        ) |>
        left_join(
          portfolio_data |>
            rename("..id" = all_of(data_options$id)) |>
            mutate(lower_bound = date, upper_bound = date + months(12)) |>
            select(-all_of(data_options$date)),
          join_by(..id, closest(..date >= lower_bound), ..date < upper_bound),
          relationship = "many-to-one"
        ) |>
        rename("{data_options$date}" := "..date", "{data_options$id}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      group_by(
        portfolio_main,
        portfolio_secondary,
        .data[[data_options$date]]
      ) |>
      summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(ret_excess, mktcap_lag)
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(ret_excess)
        ),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, .data[[data_options$date]]) |>
      summarize(
        across(c(ret_excess_vw, ret_excess_ew), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (sorting_method == "bivariate-independent") {
    if (length(sorting_variables) != 2) {
      cli::cli_abort("Provide two sorting variables for bivariate sorts.")
    }

    if (is.null(rebalancing_month)) {
      portfolio_returns <- sorting_data |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio_secondary = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          ),
          portfolio_main = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup()
    } else {
      portfolio_data <- sorting_data |>
        filter(month(.data[[data_options$date]]) == rebalancing_month) |>
        group_by(.data[[data_options$date]]) |>
        mutate(
          portfolio_secondary = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[2],
            breakpoint_options = breakpoint_options_secondary,
            breakpoint_function = breakpoint_function_secondary,
            data_options = data_options
          ),
          portfolio_main = assign_portfolio(
            pick(everything()),
            sorting_variable = sorting_variables[1],
            breakpoint_options = breakpoint_options_main,
            breakpoint_function = breakpoint_function_main,
            data_options = data_options
          )
        ) |>
        ungroup() |>
        select(all_of(c(
          data_options$id,
          data_options$date,
          "portfolio_main",
          "portfolio_secondary"
        )))

      portfolio_returns <- sorting_data |>
        rename(
          "..date" = all_of(data_options$date),
          "..id" := all_of(data_options$id)
        ) |>
        left_join(
          portfolio_data |>
            rename("..id" = all_of(data_options$id)) |>
            mutate(
              lower_bound = .data[[data_options$date]],
              upper_bound = .data[[data_options$date]] + months(12)
            ) |>
            select(-all_of(data_options$date)),
          join_by(..id, closest(..date >= lower_bound), ..date < upper_bound),
          relationship = "many-to-one"
        ) |>
        rename("{data_options$date}" := "..date", "{data_options$id}" := "..id")
    }

    portfolio_returns <- portfolio_returns |>
      group_by(
        portfolio_main,
        portfolio_secondary,
        .data[[data_options$date]]
      ) |>
      summarize(
        ret_excess_vw = if_else(
          n() < min_portfolio_size,
          NA_real_,
          stats::weighted.mean(ret_excess, mktcap_lag)
        ),
        ret_excess_ew = if_else(
          n() < min_portfolio_size,
          NA_real_,
          mean(ret_excess)
        ),
        .groups = "drop"
      ) |>
      group_by(portfolio = portfolio_main, .data[[data_options$date]]) |>
      summarize(
        across(c(ret_excess_vw, ret_excess_ew), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  if (mktcap_lag_missing) {
    portfolio_returns <- portfolio_returns |> select(-ret_excess_vw)
  }
  portfolio_returns[!is.na(portfolio_returns$portfolio), ]
}

# =============================================================================
# CORRECTNESS CHECKS
# =============================================================================
cat("=== Correctness checks ===\n")

run_check <- function(label, ...) {
  r_orig <- suppressWarnings(compute_portfolio_returns_original(...))
  r_optim <- suppressWarnings(compute_portfolio_returns(...))
  # Compare after arranging to ensure row order doesn't matter
  r_orig <- r_orig |> arrange(across(everything()))
  r_optim <- r_optim |> arrange(across(everything()))
  ok <- isTRUE(all.equal(r_orig, r_optim, tolerance = 1e-12))
  cat(sprintf("  %-55s %s\n", label, if (ok) "PASS" else "FAIL"))
  if (!ok) {
    cat(
      "    Dimensions: orig =",
      paste(dim(r_orig), collapse = "x"),
      " optim =",
      paste(dim(r_optim), collapse = "x"),
      "\n"
    )
    diffs <- which(
      !sapply(seq_len(ncol(r_orig)), function(i) {
        isTRUE(all.equal(r_orig[[i]], r_optim[[i]], tolerance = 1e-12))
      })
    )
    cat(diffs)
    if (length(diffs) > 0) {
      cat("    Differing columns:", colnames(r_orig)[diffs], "\n")
    }
  }
  invisible(ok)
}

data_small <- make_panel(n_stocks = 500, n_months = 240, seed = 42)
bp_main <- breakpoint_options(n_portfolios = 5)
bp_sec <- breakpoint_options(n_portfolios = 3)

run_check(
  "univariate, periodic",
  data_small,
  "size",
  "univariate",
  breakpoint_options_main = bp_main
)

run_check(
  "univariate, annual (July)",
  data_small,
  "size",
  "univariate",
  rebalancing_month = 7,
  breakpoint_options_main = bp_main
)

run_check(
  "bivariate-dependent, periodic",
  data_small,
  c("size", "bm"),
  "bivariate-dependent",
  breakpoint_options_main = bp_main,
  breakpoint_options_secondary = bp_sec
)

run_check(
  "bivariate-dependent, annual (July)",
  data_small,
  c("size", "bm"),
  "bivariate-dependent",
  rebalancing_month = 7,
  breakpoint_options_main = bp_main,
  breakpoint_options_secondary = bp_sec
)

run_check(
  "bivariate-independent, periodic",
  data_small,
  c("size", "bm"),
  "bivariate-independent",
  breakpoint_options_main = bp_main,
  breakpoint_options_secondary = bp_sec
)

run_check(
  "bivariate-independent, annual (July)",
  data_small,
  c("size", "bm"),
  "bivariate-independent",
  rebalancing_month = 7,
  breakpoint_options_main = bp_main,
  breakpoint_options_secondary = bp_sec
)

run_check(
  "univariate, no mktcap_lag",
  data_small |> select(-mktcap_lag),
  "size",
  "univariate",
  breakpoint_options_main = bp_main
)

run_check(
  "univariate, min_portfolio_size = 20",
  data_small,
  "size",
  "univariate",
  breakpoint_options_main = bp_main,
  min_portfolio_size = 20
)

cat("\n")

# =============================================================================
# BENCHMARK 1: Univariate periodic — varying panel size
# =============================================================================
# cat(
#   "=== Benchmark 1: Univariate periodic (5 portfolios) — varying panel size ===\n"
# )
# cat("    (Extra columns included to stress-test pick() overhead)\n")
# cat(sprintf(
#   "%-16s %-8s %-20s %-20s %-10s\n",
#   "Stocks×Months",
#   "Rows",
#   "Original (median)",
#   "Optimized (median)",
#   "Speedup"
# ))
# cat(paste(rep("-", 78), collapse = ""), "\n")

# configs <- list(
#   list(n_stocks = 100, n_months = 240),
#   list(n_stocks = 200, n_months = 600),
#   list(n_stocks = 500, n_months = 600),
#   list(n_stocks = 500, n_months = 1200),
#   list(n_stocks = 1000, n_months = 2400)
# )

# for (cfg in configs) {
#   df <- make_panel(n_stocks = cfg$n_stocks, n_months = cfg$n_months, seed = 1)
#   n_rows <- nrow(df)
#   label <- sprintf("%d × %d", cfg$n_stocks, cfg$n_months)
#   iters <- max(10L, as.integer(200000 / n_rows))

#   bm <- bench::mark(
#     original = suppressWarnings(compute_portfolio_returns_original(
#       df,
#       "size",
#       "univariate",
#       breakpoint_options_main = bp_main
#     )),
#     optimized = suppressWarnings(compute_portfolio_returns(
#       df,
#       "size",
#       "univariate",
#       breakpoint_options_main = bp_main
#     )),
#     iterations = iters,
#     check = FALSE,
#     filter_gc = FALSE
#   )

#   t_orig <- as.numeric(bm$median[1], units = "ms")
#   t_optim <- as.numeric(bm$median[2], units = "ms")
#   speedup <- t_orig / t_optim

#   cat(sprintf(
#     "%-16s %-8s %-20s %-20s %-10s\n",
#     label,
#     format(n_rows, big.mark = ","),
#     sprintf("%.1f ms", t_orig),
#     sprintf("%.1f ms", t_optim),
#     sprintf("%.2fx", speedup)
#   ))
# }

# =============================================================================
# BENCHMARK 2: Univariate annual rebalancing
# =============================================================================
# cat(
#   "\n=== Benchmark 2: Univariate annual rebalancing (July, 5 portfolios) ===\n"
# )
# cat(sprintf(
#   "%-16s %-8s %-20s %-20s %-10s\n",
#   "Stocks x Months",
#   "Rows",
#   "Original (median)",
#   "Optimized (median)",
#   "Speedup"
# ))
# cat(paste(rep("-", 78), collapse = ""), "\n")

# for (cfg in configs[1:3]) {
#   df <- make_panel(n_stocks = cfg$n_stocks, n_months = cfg$n_months, seed = 1)
#   n_rows <- nrow(df)
#   label <- sprintf("%d × %d", cfg$n_stocks, cfg$n_months)
#   iters <- max(3L, as.integer(100000 / n_rows))

#   bm <- bench::mark(
#     original = suppressWarnings(compute_portfolio_returns_original(
#       df,
#       "size",
#       "univariate",
#       rebalancing_month = 7,
#       breakpoint_options_main = bp_main
#     )),
#     optimized = suppressWarnings(compute_portfolio_returns(
#       df,
#       "size",
#       "univariate",
#       rebalancing_month = 7,
#       breakpoint_options_main = bp_main
#     )),
#     iterations = iters,
#     check = FALSE,
#     filter_gc = FALSE
#   )

#   t_orig <- as.numeric(bm$median[1], units = "ms")
#   t_optim <- as.numeric(bm$median[2], units = "ms")
#   speedup <- t_orig / t_optim

#   cat(sprintf(
#     "%-16s %-8s %-20s %-20s %-10s\n",
#     label,
#     format(n_rows, big.mark = ","),
#     sprintf("%.1f ms", t_orig),
#     sprintf("%.1f ms", t_optim),
#     sprintf("%.2fx", speedup)
#   ))
# }

# =============================================================================
# BENCHMARK 3: Memory comparison (univariate periodic, large panel)
# =============================================================================
cat(
  "\n=== Memory comparison: Univariate periodic (5000 stocks × 1200 months) ===\n"
)

df_mem <- make_panel(n_stocks = 5000, n_months = 1200, seed = 1)

bm_mem <- bench::mark(
  original = suppressWarnings(compute_portfolio_returns_original(
    df_mem,
    c("size", "bm"),
    "bivariate-dependent",
    rebalancing_month = 7,
    breakpoint_options_main = bp_main,
    breakpoint_options_secondary = bp_sec
  )),
  optimized = suppressWarnings(compute_portfolio_returns(
    df_mem,
    c("size", "bm"),
    "bivariate-dependent",
    rebalancing_month = 7,
    breakpoint_options_main = bp_main,
    breakpoint_options_secondary = bp_sec
  )),
  iterations = 5,
  check = FALSE,
  filter_gc = FALSE
)

cat(sprintf(
  "  Original  — median: %7.1f ms, mem_alloc: %s\n",
  as.numeric(bm_mem$median[1], units = "ms"),
  format(bm_mem$mem_alloc[1])
))
cat(sprintf(
  "  Optimized — median: %7.1f ms, mem_alloc: %s\n",
  as.numeric(bm_mem$median[2], units = "ms"),
  format(bm_mem$mem_alloc[2])
))

cat("\nDone.\n")
