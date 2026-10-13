# Benchmark compute_portfolio_returns() against the drop-in engines.
#
#   Rscript benchmarks/portfolio-sorts/run_benchmark.R [small|medium|crsp|daily ...]
#
# Without arguments all sizes are run. Results are written to
# benchmarks/portfolio-sorts/results/. The current implementation takes about
# 10-20 seconds per call at CRSP size, so a full run takes a while.

source(file.path("benchmarks", "portfolio-sorts", "setup.R"))
source(file.path(bench_dir, "fast_sorts.R"))

# All engines run single-threaded so that the comparison is like for like.
data.table::setDTthreads(1L)

sizes <- list(
  small = list(n_dates = 120, n_firms = 1000),
  medium = list(n_dates = 360, n_firms = 3000),
  crsp = list(n_dates = 720, n_firms = 5000),
  # Ten years of daily data: many more dates (groups) per row.
  daily = list(n_dates = 2520, n_firms = 2000)
)

scenarios <- list(
  "univariate" = list(
    sorting_variables = "bm",
    sorting_method = "univariate",
    breakpoint_options_main = bp_main_10
  ),
  "univariate, annual" = list(
    sorting_variables = "bm",
    sorting_method = "univariate",
    rebalancing_month = 7,
    breakpoint_options_main = bp_main_10
  ),
  "bivariate-dependent" = list(
    sorting_variables = c("bm", "size"),
    sorting_method = "bivariate-dependent",
    breakpoint_options_main = bp_main_5,
    breakpoint_options_secondary = bp_secondary_5
  ),
  "bivariate-independent" = list(
    sorting_variables = c("bm", "size"),
    sorting_method = "bivariate-independent",
    breakpoint_options_main = bp_main_5,
    breakpoint_options_secondary = bp_secondary_5
  )
)

args <- commandArgs(trailingOnly = TRUE)
run_sizes <- if (length(args) > 0L) args else names(sizes)

results <- list()
for (size in run_sizes) {
  data <- simulate_panel(sizes[[size]]$n_dates, sizes[[size]]$n_firms)
  cat(sprintf("\n== %s: %s rows\n", size, format(nrow(data), big.mark = ",")))

  for (scenario in names(scenarios)) {
    # Annual rebalancing is a monthly-data feature.
    if (size == "daily" && grepl("annual", scenario)) next
    args_sort <- c(list(data = data), scenarios[[scenario]], quiet = TRUE)

    calls <- list(
      current = \() do.call(tf$compute_portfolio_returns, args_sort),
      dplyr = \() do.call(compute_portfolio_returns_fast, c(args_sort, engine = "dplyr")),
      data.table = \() do.call(compute_portfolio_returns_fast, c(args_sort, engine = "data.table")),
      rcpp = \() do.call(compute_portfolio_returns_fast, c(args_sort, engine = "rcpp")),
      cpp11 = \() do.call(compute_portfolio_returns_fast, c(args_sort, engine = "cpp11"))
    )

    reference <- calls$current()
    for (engine in names(calls)) {
      identical_output <- identical(reference, calls[[engine]]())
      timing <- bench::mark(
        calls[[engine]](),
        iterations = if (engine == "current" && size %in% c("crsp", "daily")) 3L else 5L,
        check = FALSE,
        memory = TRUE,
        filter_gc = FALSE
      )
      results[[length(results) + 1L]] <- tibble(
        size = size,
        rows = nrow(data),
        dates = sizes[[size]]$n_dates,
        scenario = scenario,
        engine = engine,
        median_s = as.numeric(timing$median),
        min_s = as.numeric(timing$min),
        mem_alloc_mb = as.numeric(timing$mem_alloc) / 1024^2,
        identical = identical_output
      )
      cat(sprintf(
        "%-22s %-10s %8.3f s  %8.0f MB  identical=%s\n",
        scenario, engine, as.numeric(timing$median),
        as.numeric(timing$mem_alloc) / 1024^2, identical_output
      ))
    }
  }
}

results <- bind_rows(results) |>
  group_by(size, scenario) |>
  mutate(speedup = median_s[engine == "current"] / median_s) |>
  ungroup()

dir.create(file.path(bench_dir, "results"), showWarnings = FALSE)
out_file <- file.path(
  bench_dir,
  "results",
  paste0("benchmark-", paste(run_sizes, collapse = "-"), ".csv")
)
utils::write.csv(results, out_file, row.names = FALSE)
cat("\nResults written to", out_file, "\n")

info <- sessionInfo()
cat(
  "\nR:", R.version.string,
  "| dplyr", as.character(packageVersion("dplyr")),
  "| data.table", as.character(packageVersion("data.table")),
  "| cpp11", as.character(packageVersion("cpp11")),
  "| Rcpp", as.character(packageVersion("Rcpp")),
  "| CPU:", tryCatch(
    trimws(sub(".*: ", "", grep("model name", readLines("/proc/cpuinfo"), value = TRUE)[1L])),
    error = function(e) "unknown"
  ),
  "\n"
)
