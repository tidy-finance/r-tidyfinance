# Check that every engine returns exactly what compute_portfolio_returns()
# returns, and signals the same messages and warnings (by text).
#
#   Rscript benchmarks/portfolio-sorts/check_equivalence.R

source(file.path("benchmarks", "portfolio-sorts", "setup.R"))
source(file.path(bench_dir, "fast_sorts.R"))

engines <- c("cpp11", "rcpp", "data.table", "dplyr")

# Run a call and record its value, messages, warnings, and error.
capture <- function(f) {
  messages <- character()
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(f(), error = function(e) structure(conditionMessage(e), class = "error_text")),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = messages, warnings = warnings)
}

# Warnings the original raises inside dplyr::mutate() are wrapped with
# context, so only the distinct underlying texts are compared.
warning_texts <- function(x) {
  known <- c(
    "constant and only one portfolio",
    "No breakpoints were calculated",
    "No portfolios were assigned",
    "number of portfolios differs",
    "equally-spaced portfolios",
    "breakpoint_options_secondary",
    "overwrites"
  )
  known[vapply(known, \(k) any(grepl(k, x, fixed = TRUE)), logical(1L))]
}

check <- function(label, data, ...) {
  ref <- capture(\() tf$compute_portfolio_returns(data, ...))
  for (engine in engines) {
    res <- capture(\() compute_portfolio_returns_fast(data, ..., engine = engine))
    ok_value <- identical(ref$value, res$value)
    ok_messages <- identical(ref$messages, res$messages)
    ok_warnings <- identical(warning_texts(ref$warnings), warning_texts(res$warnings))
    status <- if (ok_value && ok_messages && ok_warnings) "ok" else "DIFFERS"
    cat(sprintf(
      "%-7s %-38s %-10s value=%s messages=%s warnings=%s\n",
      status, label, engine, ok_value, ok_messages, ok_warnings
    ))
    results[[length(results) + 1L]] <<- status == "ok"
  }
}
results <- list()

base <- simulate_panel(48, 600, seed = 1L)
bp10 <- bp_main_10
bp5 <- bp_main_5

# Standard sorts.
check("univariate", base, "bm", "univariate", breakpoint_options_main = bp10)
check("univariate annual", base, "bm", "univariate", rebalancing_month = 7, breakpoint_options_main = bp10)
check("dependent", base, c("bm", "size"), "bivariate-dependent", breakpoint_options_main = bp5, breakpoint_options_secondary = bp5)
check("independent", base, c("bm", "size"), "bivariate-independent", breakpoint_options_main = bp5, breakpoint_options_secondary = bp5)
check("dependent annual", base, c("bm", "size"), "bivariate-dependent", rebalancing_month = 7, breakpoint_options_main = bp5, breakpoint_options_secondary = bp5)
check("independent annual", base, c("mom", "size"), "bivariate-independent", rebalancing_month = 1, breakpoint_options_main = bp5, breakpoint_options_secondary = bp5)

# Breakpoint options.
check("percentiles", base, "bm", "univariate", breakpoint_options_main = tf$breakpoint_options(percentiles = c(0.3, 0.7)))
check("all exchanges", base, "mom", "univariate", breakpoint_options_main = tf$breakpoint_options(n_portfolios = 3))
check("size threshold", base, "bm", "univariate", breakpoint_options_main = tf$breakpoint_options(n_portfolios = 5, breakpoints_exchanges = "NYSE", breakpoints_min_size_threshold = 0.2))
check("min portfolio size 20", base, "bm", "univariate", breakpoint_options_main = bp10, min_portfolio_size = 20L)
check("min portfolio size 0", base, c("bm", "size"), "bivariate-dependent", breakpoint_options_main = bp5, breakpoint_options_secondary = bp5, min_portfolio_size = 0L)
check("cap weight 0.5", base, "bm", "univariate", breakpoint_options_main = bp10, cap_weight = 0.5)
check("messages (quiet = FALSE)", base, "bm", "univariate", breakpoint_options_main = bp10, min_portfolio_size = 30L, quiet = FALSE)

# Bunching: many zeros in the sorting variable.
bunched <- base |> mutate(bm = if_else(row_number() %% 3 == 0, 0, bm))
check("clusters", bunched, "bm", "univariate", breakpoint_options_main = bp10)
check("smooth bunching", bunched, "bm", "univariate", breakpoint_options_main = tf$breakpoint_options(n_portfolios = 5, smooth_bunching = TRUE))
check("smooth bunching percentiles", bunched, "bm", "univariate", breakpoint_options_main = tf$breakpoint_options(percentiles = c(0.3, 0.7), smooth_bunching = TRUE))

# Data quirks.
check("NA returns", base |> mutate(ret_excess = if_else(row_number() %% 50 == 0, NA_real_, ret_excess)), "bm", "univariate", breakpoint_options_main = bp10)
check("no mktcap_lag column", base |> select(-mktcap_lag), "mom", "univariate", breakpoint_options_main = tf$breakpoint_options(n_portfolios = 5))
check("constant date", base |> mutate(bm = if_else(date == min(date), 1, bm)), "bm", "univariate", breakpoint_options_main = bp10)
check("date without NYSE stocks", base |> mutate(exchange = if_else(date == min(date), "NASDAQ", exchange)), "bm", "univariate", breakpoint_options_main = bp10)
check("existing portfolio column", base |> mutate(portfolio = 1L), "bm", "univariate", breakpoint_options_main = bp10)
check("integer sorting variable", base |> mutate(mom = as.integer(round(mom * 10))), "mom", "univariate", breakpoint_options_main = tf$breakpoint_options(n_portfolios = 5))
check("all sorting values missing", base |> mutate(bm = NA_real_), "bm", "univariate", breakpoint_options_main = bp10)

# Calls that fall back to the original implementation.
check("custom breakpoint function", base, "bm", "univariate", breakpoint_options_main = bp10, breakpoint_function_main = \(...) tf$compute_breakpoints(...))
check("invalid sorting method", base, "bm", "trivariate", breakpoint_options_main = bp10)
check("missing secondary options", base, c("bm", "size"), "bivariate-dependent", breakpoint_options_main = bp5)

n_ok <- sum(unlist(results))
cat(sprintf("\n%d of %d checks passed.\n", n_ok, length(results)))
if (n_ok < length(results)) quit(status = 1L)
