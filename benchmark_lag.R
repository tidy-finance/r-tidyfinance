library(tidyfinance)
library(tibble)
library(lubridate)
library(bench)
library(dplyr)

# --- Generate test data ---
set.seed(42)
n_groups <- 100
n_months <- 300

data <- tibble(
  permno = rep(seq_len(n_groups), each = n_months),
  date = rep(seq.Date(as.Date("2010-01-01"), by = "month", length.out = n_months), n_groups),
  size = runif(n_groups * n_months, 100, 10000),
  bm = runif(n_groups * n_months, 0.1, 3.0)
)

cat(sprintf("Data: %d groups x %d months = %s rows\n", n_groups, n_months, format(nrow(data), big.mark = ",")))

# --- Verify identical output (exact lag) ---
cat("\nVerifying identical output (exact lag)...\n")

result_old <- add_lagged_columns(data, cols = c("size", "bm"), lag = months(6), by = "permno")
result_new <- exp_add_lagged_columns(data, cols = c("size", "bm"), lag = months(6), by = "permno")

stopifnot(all.equal(result_old$size_lag, result_new$size_lag))
stopifnot(all.equal(result_old$bm_lag, result_new$bm_lag))
cat("Output is identical.\n")

# --- Verify identical output (window lag) ---
cat("\nVerifying identical output (window lag)...\n")

result_old_w <- add_lagged_columns(data, cols = c("size", "bm"), lag = months(3), max_lag = months(6), by = "permno")
result_new_w <- exp_add_lagged_columns(data, cols = c("size", "bm"), lag = months(3), max_lag = months(6), by = "permno")

stopifnot(all.equal(result_old_w$size_lag, result_new_w$size_lag))
stopifnot(all.equal(result_old_w$bm_lag, result_new_w$bm_lag))
cat("Output is identical.\n")

# === EXACT LAG BENCHMARKS (lag == max_lag) ===

cat("\n=== EXACT LAG (lag == max_lag) ===\n")

cat("\n--- 1 column ---\n")
bm_exact_1 <- bench::mark(
  old = add_lagged_columns(data, cols = "size", lag = months(6), by = "permno"),
  new = exp_add_lagged_columns(data, cols = "size", lag = months(6), by = "permno"),
  min_iterations = 10,
  check = FALSE
)

cat("\n--- 2 columns ---\n")
bm_exact_2 <- bench::mark(
  old = add_lagged_columns(data, cols = c("size", "bm"), lag = months(6), by = "permno"),
  new = exp_add_lagged_columns(data, cols = c("size", "bm"), lag = months(6), by = "permno"),
  min_iterations = 10,
  check = FALSE
)

cat("\n--- 1 column, drop_na = TRUE ---\n")
data_na <- data
data_na$size[sample(nrow(data_na), nrow(data_na) / 10)] <- NA

bm_exact_na <- bench::mark(
  old = add_lagged_columns(data_na, cols = "size", lag = months(6), by = "permno", drop_na = TRUE),
  new = exp_add_lagged_columns(data_na, cols = "size", lag = months(6), by = "permno", drop_na = TRUE),
  min_iterations = 10,
  check = FALSE
)

# === WINDOW LAG BENCHMARKS (lag < max_lag) ===

cat("\n=== WINDOW LAG (lag < max_lag) ===\n")

cat("\n--- 1 column ---\n")
bm_window_1 <- bench::mark(
  old = add_lagged_columns(data, cols = "size", lag = months(3), max_lag = months(6), by = "permno"),
  new = exp_add_lagged_columns(data, cols = "size", lag = months(3), max_lag = months(6), by = "permno"),
  min_iterations = 10,
  check = FALSE
)

cat("\n--- 2 columns ---\n")
bm_window_2 <- bench::mark(
  old = add_lagged_columns(data, cols = c("size", "bm"), lag = months(3), max_lag = months(6), by = "permno"),
  new = exp_add_lagged_columns(data, cols = c("size", "bm"), lag = months(3), max_lag = months(6), by = "permno"),
  min_iterations = 10,
  check = FALSE
)

cat("\n--- 1 column, drop_na = TRUE ---\n")
bm_window_na <- bench::mark(
  old = add_lagged_columns(data_na, cols = "size", lag = months(3), max_lag = months(6), by = "permno", drop_na = TRUE),
  new = exp_add_lagged_columns(data_na, cols = "size", lag = months(3), max_lag = months(6), by = "permno", drop_na = TRUE),
  min_iterations = 10,
  check = FALSE
)

# === RESULTS ===

cat("\n\n========================================\n")
cat("RESULTS\n")
cat("========================================\n")

cat("\n--- Exact lag: 1 column ---\n")
print(bm_exact_1[, c("expression", "min", "median", "mem_alloc", "n_itr")])

cat("\n--- Exact lag: 2 columns ---\n")
print(bm_exact_2[, c("expression", "min", "median", "mem_alloc", "n_itr")])

cat("\n--- Exact lag: 1 column, drop_na ---\n")
print(bm_exact_na[, c("expression", "min", "median", "mem_alloc", "n_itr")])

cat("\n--- Window lag: 1 column ---\n")
print(bm_window_1[, c("expression", "min", "median", "mem_alloc", "n_itr")])

cat("\n--- Window lag: 2 columns ---\n")
print(bm_window_2[, c("expression", "min", "median", "mem_alloc", "n_itr")])

cat("\n--- Window lag: 1 column, drop_na ---\n")
print(bm_window_na[, c("expression", "min", "median", "mem_alloc", "n_itr")])
