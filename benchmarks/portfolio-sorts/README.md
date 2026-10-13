# Portfolio-sort benchmark

How much faster could `compute_portfolio_returns()` (and with it
`implement_portfolio_sort()`) be with compiled code (cpp11 or Rcpp) or similar
tools, **without changing anything for the user**: same arguments, same
output, same messages?

This folder is build-ignored and not part of the package. It contains
prototypes and a benchmark, not a change to `R/`.

## Files

| File | Purpose |
|---|---|
| `setup.R` | Sources the package's `R/` files into `tf` (no install needed) and simulates a CRSP-like panel. |
| `portfolio_sorts_core.h` | C++ kernels: capped weights, breakpoints + assignment, annual carry-forward, return aggregation. Plain C++ on raw pointers, shared by both bindings. |
| `portfolio_sorts_cpp11.cpp` | cpp11 bindings for the kernels (the proposed route for the package). |
| `portfolio_sorts_rcpp.cpp` | Rcpp bindings for the same kernels, for comparison. |
| `fast_sorts.R` | `compute_portfolio_returns_fast(..., engine = "cpp11" / "rcpp" / "data.table" / "dplyr")`, a drop-in variant of `compute_portfolio_returns()`. |
| `check_equivalence.R` | Compares every engine with the current function on 26 scenarios (standard sorts and edge cases). |
| `run_benchmark.R` | Times all engines with `bench::mark()` across data sizes and sort types, and writes `results/*.csv`. |

Run from the package root:

```sh
Rscript benchmarks/portfolio-sorts/check_equivalence.R
Rscript benchmarks/portfolio-sorts/run_benchmark.R small medium   # quick
Rscript benchmarks/portfolio-sorts/run_benchmark.R                # all sizes, takes a while
```

Requirements: dplyr, tidyr, lubridate, tibble, purrr, rlang, cli, vctrs,
data.table, cpp11, Rcpp, and bench, plus a C++ compiler. `cpp11::cpp_source()`
also needs decor, brio, callr, desc, and glue. Those are needed only to compile
ad hoc here; a package build needs none of them.

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
- `cpp11` and `rcpp`: C++ kernels for every per-row step; R only does the
  glue. Both engines call the same code in `portfolio_sorts_core.h`, so any
  difference between them is binding overhead.

### What each route would add to the package

| Route | `DESCRIPTION` | New runtime dependency | Compiled code in tidyfinance |
|---|---|---|---|
| Tuned dplyr | nothing | none | no |
| data.table | `Imports: data.table` | data.table | no |
| Rcpp | `Imports: Rcpp`, `LinkingTo: Rcpp` | Rcpp | yes |
| cpp11 | `LinkingTo: cpp11` | **none**: cpp11 is header-only and used only at build time | yes |

Rcpp is not in tidyfinance's dependency tree today (traced through all
imports). Its own dependencies are only `methods` and `utils`, which come with
R, so it would add exactly one package. cpp11 is already in the tree as a
build-time dependency of tidyr, RPostgres, and nanoparquet.

Either compiled route turns tidyfinance into a package with compiled code:

- CRAN ships binaries for Windows and macOS, so nothing changes for those
  users.
- Installing from source (e.g. `pak::pak("tidy-finance/r-tidyfinance")`, or
  Linux without binaries) needs a compiler: Rtools on Windows, Xcode
  command-line tools on macOS, or g++ on Linux. Posit Package Manager and
  r-universe provide Linux binaries.
- R CMD check adds compiled-code checks.

For cpp11 in the package:

1. Move `portfolio_sorts_core.h` and the bindings to `src/`.
2. Add `LinkingTo: cpp11` to `DESCRIPTION`.
3. Add `@useDynLib tidyfinance, .registration = TRUE` (roxygen).
4. Run `cpp11::cpp_register()`, which generates `src/cpp11.cpp` and
   `R/cpp11.R`.

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

Preliminary timings on a CRSP-sized panel (3.24M rows, 720 months, about
4,500 firms per month), all single-threaded. Each value is the median of 3
runs after a warm-up. Specs: NYSE breakpoints, monthly rebalancing. Output is
`identical()` to the current implementation in every case.

| Sort | current | dplyr (tuned) | data.table | compiled (cpp11 or Rcpp) |
|---|---|---|---|---|
| Univariate, deciles | 5.47 s | 3.17 s (1.7x) | 2.11 s (2.6x) | **1.08 s (5.1x)** |
| Bivariate dependent, 5x5 | 13.29 s | 6.18 s (2.2x) | 3.87 s (3.4x) | **1.64 s (8.1x)** |

cpp11 and Rcpp call the same C++ code. Run in alternating random order (7
runs each, medians), they take the same time: 1.20 s vs 1.20 s (univariate)
and 1.77 s vs 1.80 s (bivariate dependent, cpp11 vs Rcpp). Binding overhead
is per call, and there are only a handful of calls per sort. An earlier
sequential run showed a 0.35 s gap in the bivariate case. That gap came from
the order of the runs and disappears when they alternate.

Environment: 4-core cloud container, R 4.3.3, dplyr 1.1.4, data.table
1.14.10, cpp11 0.4.7, Rcpp 1.0.12. Timings vary by about 10% between runs.
`run_benchmark.R` repeats this with `bench::mark()` over four data sizes
(including a daily panel) and all four sort types.

Takeaways so far:

- Part of the gain needs no compiled code. Removing per-group R overhead in
  dplyr alone gives about 2x.
- The C++ kernels themselves take about 0.75 s of the ~1.05 s univariate run:
  capping weights 0.27 s, breakpoints + assignment 0.24 s, aggregation
  0.15 s. The rest is R glue. Matching `Date`s with `match()` was the single
  biggest piece of glue (about 1.6 s, because R 4.3 converts both sides to
  character); matching on the underlying numbers removes it. The original
  never calls `match()` on dates, so this is a pitfall to remember, not a
  cost the original pays.
- The gains grow with the number of groups (bivariate, daily data), because
  the original's cost is per group rather than per row. On a small panel
  (108k rows, 120 months) the compiled engines are 18x (univariate) to 37x
  (bivariate dependent) faster, for example 1.83 s vs 0.05 s, because the
  fixed cost per group dominates there.

## Status and open items

Done:

- C++ kernels with cpp11 and Rcpp bindings, plus data.table and dplyr
  engines. All are `identical()` to the current implementation on every
  scenario in `check_equivalence.R`. 97 of 104 checks pass in full. The other
  7 have identical values and messages and differ only in how warnings are
  presented (items 2 and 3 below).
- Preliminary timings at CRSP scale (above).

Open:

1. **Run the full benchmark** (`run_benchmark.R`) on a local machine with
   current package versions (the numbers above come from a 4-core cloud
   container with R 4.3.3, dplyr 1.1.4, data.table 1.14.10, and Rcpp 1.0.12)
   and paste the table here.
2. **Warnings are presented differently.** In the original, warnings from
   `assign_portfolio()` and `compute_breakpoints()` (clusters, constant
   variable, missing breakpoints) are raised inside `dplyr::mutate()`. dplyr
   wraps them ("There were 2 warnings in `dplyr::mutate()`. The first warning
   was: ...") and keeps the rest for `dplyr::last_dplyr_warnings()`. The
   cpp11 and rcpp engines raise each distinct warning text once, directly. The data.table
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
5. If a compiled route is adopted (cpp11 recommended), follow the steps in
   "What each route would add to the package", and port `check_equivalence.R`
   into `tests/testthat/` as regression tests against the current dplyr code
   path.
