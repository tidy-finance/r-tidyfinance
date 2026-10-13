# Shared setup for the portfolio-sort benchmarks.
#
# Run all scripts from the package root, e.g.
#   Rscript benchmarks/portfolio-sorts/run_benchmark.R
#
# The package's R/ files are sourced into an environment instead of installing
# tidyfinance, so the benchmark runs against the working tree and does not need
# the download-only dependencies (RPostgres, nanoparquet, ...).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(tibble)
  library(purrr)
  library(rlang)
})

bench_dir <- file.path("benchmarks", "portfolio-sorts")

load_tidyfinance <- function(path = "R") {
  env <- new.env(parent = globalenv())
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  for (f in files) {
    sys.source(f, envir = env)
  }
  env
}

tf <- load_tidyfinance()

# Simulate a CRSP-like monthly panel ---------------------------------------
#
# Rows are ordered by permno and date (as in CRSP), so a sort by date has to
# reorder the data. Each firm-month is kept with probability `p_keep`, which
# yields an unbalanced panel with a varying cross-section per date. About 5% of
# the book-to-market values and 1% of the lagged market caps are missing.
simulate_panel <- function(n_dates, n_firms, p_keep = 0.9, seed = 2024L) {
  set.seed(seed)
  dates <- seq.Date(as.Date("1964-01-01"), by = "month", length.out = n_dates)
  panel <- tidyr::expand_grid(permno = seq_len(n_firms), date = dates)
  panel <- panel[stats::runif(nrow(panel)) < p_keep, ]
  n <- nrow(panel)

  mktcap_lag <- exp(stats::rnorm(n, mean = 5, sd = 2))
  mktcap_lag[stats::runif(n) < 0.01] <- NA_real_

  bm <- exp(stats::rnorm(n, mean = -0.5, sd = 0.8))
  bm[stats::runif(n) < 0.05] <- NA_real_

  panel |>
    mutate(
      exchange = sample(
        c("NYSE", "AMEX", "NASDAQ"),
        n,
        replace = TRUE,
        prob = c(0.3, 0.1, 0.6)
      ),
      mktcap_lag = mktcap_lag,
      ret_excess = stats::rnorm(n, mean = 0.01, sd = 0.12),
      size = log(mktcap_lag),
      bm = bm,
      mom = stats::rnorm(n, mean = 0.1, sd = 0.4)
    )
}

# Sorting specifications used throughout the benchmarks.
bp_main_10 <- tf$breakpoint_options(
  n_portfolios = 10,
  breakpoints_exchanges = "NYSE"
)
bp_main_5 <- tf$breakpoint_options(
  n_portfolios = 5,
  breakpoints_exchanges = "NYSE"
)
bp_secondary_5 <- tf$breakpoint_options(
  n_portfolios = 5,
  breakpoints_exchanges = "NYSE"
)
