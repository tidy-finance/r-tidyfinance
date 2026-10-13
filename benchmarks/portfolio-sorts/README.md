# Portfolio-sort benchmark

How much faster could `compute_portfolio_returns()` (and with it
`implement_portfolio_sort()`) be with Rcpp or similar tools, **without changing
anything for the user**: same arguments, same output, same messages?

This folder is build-ignored and not part of the package. It contains
prototypes and a benchmark, not a change to `R/`.

## Files

| File | Purpose |
|---|---|
| `setup.R` | Sources the package's `R/` files into `tf` (no install needed) and simulates a CRSP-like panel. |
| `portfolio_sorts.cpp` | Rcpp kernels: capped weights, breakpoints + assignment, annual carry-forward, return aggregation. |
| `fast_sorts.R` | `compute_portfolio_returns_fast(..., engine = "rcpp" / "data.table" / "dplyr")`, a drop-in variant of `compute_portfolio_returns()`. |
| `check_equivalence.R` | Compares every engine with the current function on 26 scenarios (standard sorts and edge cases). |
| `run_benchmark.R` | Times all engines with `bench::mark()` across data sizes and sort types, and writes `results/*.csv`. |

Run from the package root:

```sh
Rscript benchmarks/portfolio-sorts/check_equivalence.R
Rscript benchmarks/portfolio-sorts/run_benchmark.R small medium   # quick
Rscript benchmarks/portfolio-sorts/run_benchmark.R                # all sizes, takes a while
```

Requirements: dplyr, tidyr, lubridate, tibble, purrr, rlang, cli, vctrs,
data.table, Rcpp (with a C++ compiler), and bench.

## Design: "nothing changes for the user"

`compute_portfolio_returns_fast()` keeps the signature of
`compute_portfolio_returns()`. Argument validation, messages, and the
completion of the output panel are copied verbatim from the original. Only the
core is swapped out: dropping missing sorting variables, capping weights,
assigning portfolios, and aggregating returns.

- **Exact results.** Every engine is checked with `identical()` against the
  current implementation, not `all.equal()`. The C++ kernels reproduce R's
  arithmetic bit for bit: type-7 `quantile()`, `findInterval(all.inside = TRUE)`,
  `sum()`/`mean()` with long double accumulation (as in R's `summary.c`),
  `weighted.mean()` as `sum((x * w)[w != 0]) / sum(w)`, and the original
  within-group row order.
- **Fallback.** Calls the fast path cannot reproduce exactly go to the
  original function unchanged: a custom `breakpoint_function_*`, invalid
  options or arguments, and duplicated firm-months at rebalancing dates (which
  the original rejects in its join).
- **All breakpoint options are supported** on the fast path:
  `n_portfolios`, `percentiles`, `breakpoints_exchanges`,
  `breakpoints_min_size_threshold`, and `smooth_bunching`.

### Engines

- `dplyr`: still pure dplyr and no new dependencies. It fixes the hotspots of
  the original: no scalar `dplyr::if_else()` per group in `summarise()`, one
  grouped `mutate()` instead of two, and slicing only the needed columns.
- `data.table`: data.table grouping, but the breakpoints still come from the
  package's unchanged `assign_portfolio()` and `compute_breakpoints()`, called
  per date.
- `rcpp`: C++ kernels for every per-row step. R only does the glue.

## Where the time goes in the current implementation

Profile of the univariate decile sort (NYSE breakpoints, monthly rebalancing)
on a CRSP-sized panel of 3.2M rows and 720 months:

| Step | Share of runtime |
|---|---|
| `summarise()` with three scalar `dplyr::if_else()` calls per portfolio-month (7,200 groups) | ~47% |
| `tidyr::drop_na()` (slices all columns of the data) | ~20% |
| `group_by()` (twice, by date) | ~12% |
| `assign_portfolio()` incl. `quantile()` and `findInterval()` | ~12% |

So most of the time is per-group R overhead, not the sorting arithmetic.

## Results

Preliminary single runs (after a warm-up) on a CRSP-sized panel (3.24M rows,
720 months, about 4,500 firms per month), all single-threaded. Specs: NYSE
breakpoints, monthly rebalancing. Output is `identical()` to the current
implementation in every case.

| Sort | current | dplyr (tuned) | data.table | Rcpp |
|---|---|---|---|---|
| Univariate, deciles | 5.23 s | 2.89 s (1.8x) | 1.84 s (2.8x) | **1.03 s (5.1x)** |
| Bivariate dependent, 5x5 | 12.15 s | 5.54 s (2.2x) | 3.32 s (3.7x) | **1.58 s (7.7x)** |

Environment: 4-core cloud container, R 4.3.3, dplyr 1.1.4, data.table
1.14.10, Rcpp 1.0.12. `run_benchmark.R` repeats this with `bench::mark()`
over four data sizes (including a daily panel) and all four sort types.

Takeaways so far:

- Roughly half of the gain needs no compiled code. Removing per-group R
  overhead in dplyr alone gives about 2x.
- The C++ kernels themselves take about 0.75 s of the 1.03 s univariate run:
  capping weights 0.27 s, breakpoints + assignment 0.24 s, aggregation
  0.15 s. The rest is R glue. Matching `Date`s with `match()` was the single
  biggest piece of glue (about 1.6 s, because R 4.3 converts both sides to
  character); matching on the underlying numbers removes it. The original
  never calls `match()` on dates, so this is a pitfall to remember, not a
  cost the original pays.
- The gains grow with the number of groups (bivariate, daily data), because
  the original's cost is per group rather than per row.

## Status and open items

Done:

- Rcpp kernels, data.table and dplyr engines, all `identical()` to the current
  implementation on every scenario in `check_equivalence.R`. 73 of 78 checks
  pass in full. The other 5 have identical values and messages and differ
  only in how warnings are presented (items 2 and 3 below).
- Preliminary single-run timings at CRSP scale (above).

Open:

1. **Run the full benchmark** (`run_benchmark.R`) on a local machine with
   current package versions (the numbers above come from a 4-core cloud
   container with R 4.3.3, dplyr 1.1.4, data.table 1.14.10, and Rcpp 1.0.12)
   and paste the table here.
2. **Warnings are presented differently.** In the original, warnings from
   `assign_portfolio()` and `compute_breakpoints()` (clusters, constant
   variable, missing breakpoints) are raised inside `dplyr::mutate()`. dplyr
   wraps them ("There were 2 warnings in `dplyr::mutate()`. The first warning
   was: ...") and keeps the rest for `dplyr::last_dplyr_warnings()`. The rcpp
   engine raises each distinct warning text once, directly. The data.table
   engine raises one per affected date. The texts are the same, but the
   presentation differs, and `check_equivalence.R` flags it for the "date
   without NYSE stocks" case. To change nothing at all, a package version
   could keep the dplyr wrapper format, or the team could accept the clearer
   direct warnings.
3. `check_new_col()` names the calling function. When the original is called
   through `tf$compute_portfolio_returns()`, `rlang::call_name()` returns
   `NULL`, which produces a different but harmless wording in the "existing
   portfolio column" check. This is an artifact of the harness. Inside the
   package both would read `compute_portfolio_returns()`.
4. `implement_portfolio_sort()` also runs `filter_sorting_data()`, which has
   not been profiled (it is vectorised, so it is likely cheap next to the
   sort).
5. If the Rcpp route is adopted: move the kernels to `src/`, add
   `LinkingTo: Rcpp` and `Imports: Rcpp`, and port `check_equivalence.R`
   into `tests/testthat/` as regression tests against the current dplyr code
   path.
