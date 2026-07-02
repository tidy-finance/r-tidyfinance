# Changelog

## tidyfinance (development version)

### Improvements

- The `sorting_variable` column of the `factor_library_grid` dataset no
  longer carries a `"sv_"` prefix, so its values now match the
  `sorting_variable` argument of
  `download_data("Tidy Finance", "factor_library", ...)` (e.g. `"bm"`
  rather than `"sv_bm"`).
  `download_data("Tidy Finance", "factor_library_grid")` returns the
  bare values accordingly
  ([\#284](https://github.com/tidy-finance/r-tidyfinance/issues/284)).

### New features

- Added
  [`download_data_pastor_stambaugh()`](https://r.tidy-finance.org/reference/download_data_pastor_stambaugh.md)
  and the `"Pastor-Stambaugh"` domain for
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
  which downloads the liquidity factors of Pastor and Stambaugh (2003)
  from [Lubos Pastor’s data
  library](https://faculty.chicagobooth.edu/lubos-pastor/data). The
  result carries the levels of aggregate liquidity, the non-traded
  liquidity factor (innovations), and the traded liquidity factor
  `LIQ_V`.
- Added
  [`download_data_stambaugh_yuan()`](https://r.tidy-finance.org/reference/download_data_stambaugh_yuan.md)
  and the `"Stambaugh-Yuan"` domain for
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
  which downloads the mispricing factors (`mgmt` and `perf`) of
  Stambaugh and Yuan (2017) from [Robert Stambaugh’s data
  library](https://finance.wharton.upenn.edu/~stambaug/). The `dataset`
  argument selects between `"monthly"` and `"daily"` data. The source
  files currently end in December 2016.
- Added
  [`download_data_jkp()`](https://r.tidy-finance.org/reference/download_data_jkp.md)
  and the `"Global Factor Data"` domain for
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
  which downloads data from [Global Factor
  Data](https://jkpfactors.com/data) (Jensen, Kelly, and Pedersen,
  2023). The `dataset` argument selects between factor returns
  (`"factors"`), the underlying long-short portfolios (`"portfolios"`),
  industry returns (`"industry"`), and the reference files
  `"nyse_cutoffs"` and `"return_cutoffs"`. The requested selection is
  validated against the library’s live availability manifest, and the
  helper
  [`list_supported_jkp_factors()`](https://r.tidy-finance.org/reference/list_supported_jkp_factors.md)
  lists the available regions and selectors.

### Improvements

- Added a `tidyfinance` vignette that walks through the complete
  factor-construction workflow — download, signal construction,
  fiscal-year lagging, portfolio sorting, and a Fama-MacBeth test — end
  to end. It builds entirely on `download_data(domain = "Pseudo Data")`,
  so it compiles without a WRDS subscription or network access. `knitr`
  and `rmarkdown` are added back to `Suggests`, and
  `VignetteBuilder: knitr` is restored to `DESCRIPTION`.

- `download_data("Open Source Asset Pricing")` now aligns the `date`
  column to the beginning of the month (the dataset previously returned
  end-of-month dates), matching the convention used by the other
  download functions. All predictor columns are monthly long-short
  returns expressed in percent and are now divided by 100 to return
  plain numeric (decimal) returns.

### Bug fixes

- `download_data_huggingface("factor_library", ...)` now treats an
  explicit `n_portfolios_secondary = NULL` as “remove the filter and
  return all values” (univariate and bivariate sorts alike), consistent
  with the documented behavior for every other column. Previously an
  explicit `NULL` was coerced to `NA`, restricting the result to
  univariate sorts.

## tidyfinance 0.7.0

CRAN release: 2026-06-25

### Improvements

- [`estimate_betas()`](https://r.tidy-finance.org/reference/estimate_betas.md)
  now uses a fast, vectorized closed-form approach based on rolling
  cumulants of the moment matrices instead of fitting one regression per
  stock and window. This removes the need for per-stock nesting and the
  optional `furrr` parallelization, so the `use_furrr` argument and the
  `furrr` dependency have been dropped. Estimates are numerically
  identical to the previous regression-based implementation. Windows
  with fewer than `min_obs` observations are now dropped from the output
  rather than returned with `NA` coefficients.
- The package website moved from `package.tidy-finance.org` to
  `r.tidy-finance.org`.
- [`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
  now uses the human-readable domain names returned by
  [`list_supported_datasets()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)
  (e.g., `"Fama-French"`, `"Global Q"`, `"WRDS"`, `"Tidy Finance"`). The
  `"pseudo"` and `"tidyfinance"` domains were renamed to `"Pseudo Data"`
  and `"Tidy Finance"`. The previous machine-readable domain names
  (e.g., `"famafrench"`, `"wrds"`, `"pseudo"`, `"tidyfinance"`) are
  soft-deprecated but still accepted.
- [`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md)
  now errors informatively when `version = "v1"` is used with an
  `end_date` later than December 2024, reflecting the discontinuation of
  the CRSP legacy version at the end of 2024.
- Removed the “experimental” lifecycle badge from
  [`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md),
  [`compute_breakpoints()`](https://r.tidy-finance.org/reference/compute_breakpoints.md),
  [`compute_rolling_value()`](https://r.tidy-finance.org/reference/compute_rolling_value.md),
  [`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md),
  and
  [`join_lagged_values()`](https://r.tidy-finance.org/reference/join_lagged_values.md),
  which are now considered stable.

## tidyfinance 0.6.0

CRAN release: 2026-05-31

### New features

- Added `domain = "pseudo"` to
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
  for generating pseudo data with the same schema as the corresponding
  real domain. Supported datasets in this release: `"crsp_monthly"`,
  `"crsp_daily"`, `"compustat_annual"`, `"compustat_quarterly"`, and
  `"ccm_links"` (all mirroring `domain = "wrds"`). Internally, every
  `domain = "pseudo"` call funnels through `simulate_pseudo_data()`, the
  unexported router that dispatches to per-dataset generators.
  Per-dataset entry points
  ([`download_data_pseudo_crsp()`](https://r.tidy-finance.org/reference/download_data_pseudo_crsp.md),
  [`download_data_pseudo_compustat()`](https://r.tidy-finance.org/reference/download_data_pseudo_compustat.md),
  [`download_data_pseudo_ccm_links()`](https://r.tidy-finance.org/reference/download_data_pseudo_ccm_links.md))
  remain exported for direct use. All generators accept `n_assets` and
  `seed` arguments; identical `(seed, n_assets)` yields the same
  identifier universe across datasets, so pseudo CRSP and Compustat join
  cleanly via `add_ccm_links = TRUE` or `ccm_links`. Daily CRSP is
  generated on weekdays only.
- Added
  [`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md)
  to fetch the `tidy-finance/factor-library-grid` dataset from Hugging
  Face. Also accessible via
  `download_data("tidyfinance", "factor_library_grid")`.

### Improvements

- Added `test-coverage.yaml` workflow and badge to README.
- Added tests to get coverage to 100% (excl.
  [`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)).
- Fama-French factor data is now downloaded and parsed internally via
  `httr2`, so `frenchdata` is no longer declared in `Imports`. The
  behavior of
  [`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md)
  is unchanged.
- `download_data("tidyfinance", "factor_library", ...)` now honors the
  canonical `start_date` and `end_date` arguments, filtering the
  returned portfolio returns to the requested range. When both are
  omitted, the full history is returned and the standard “Returning the
  full data set” message is emitted (via
  [`validate_dates()`](https://r.tidy-finance.org/reference/validate_dates.md)).
  Previously these arguments were accepted but silently ignored for the
  factor library.
- Removed the `using-tidyfinance` and `dates-in-tidyfinance` vignettes.
  Both predated the current
  [`download_data()`](https://r.tidy-finance.org/reference/download_data.md)
  interface and are superseded by the package manuscript. `knitr` and
  `rmarkdown` are no longer declared in `Suggests`, and
  `VignetteBuilder` has been dropped from `DESCRIPTION`.
- `download_data("tidyfinance", "factor_library", ids = <vector>)` now
  delegates directly to
  [`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md),
  bypassing the grid filter. Passing `ids` together with filter
  arguments raises an informative error.
- Renamed
  [`list_supported_types()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)
  to
  [`list_supported_datasets()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)
  ([\#242](https://github.com/tidy-finance/r-tidyfinance/issues/242)).
  The old name remains exported as a soft-deprecated alias that forwards
  to the new function. Internal helpers were renamed accordingly
  (e.g. `list_supported_types_ff()` -\>
  [`list_supported_datasets_ff()`](https://r.tidy-finance.org/reference/list_supported_datasets_ff.md)).
- [`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md)
  now drops symbols equal to `"-"`.
- Renamed `only_us` parameter in
  [`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md)
  to `only_usd` to reflect that the filter keeps USD-denominated shares
  only. The old name is deprecated and forwards to `only_usd` with a
  warning.
- Removed `arrow`, `glue`, and `stringr` dependencies and added
  `nanoparquet`.

## tidyfinance 0.5.0

CRAN release: 2026-05-12

### New features

- Added
  [`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
  as a convenience wrapper that combines sample construction filtering
  and portfolio return computation into a single call.
- Added
  [`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md)
  to download and process risk-free rate data from FRED, splicing TB3MS
  (pre-2001) with DTB4WK (from 2001 onwards) for monthly data, and using
  DTB3 for daily data. Also accessible via
  `download_data("tidyfinance", "risk_free")`.
- Updated
  [`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md)
  to use
  [`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md)
  (FRED-based) instead of the Kenneth French risk-free rate when
  computing excess returns.
- Added
  [`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md).
- Added `only_us` parameter to
  [`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md).
- Added new parameters for common CRSP transformation tasks
  (`add_ccm_links`, `adjust_volume`) to
  [`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md).
- Added `prc_adj` to `"crsp_monthly"` version `"v1"`.
- Added `adjust_volume` parameter for `"crsp_daily"` version `"v1"` and
  `"v2"` to
  [`download_data_wrds_compustat()`](https://r.tidy-finance.org/reference/download_data_wrds_compustat.md).
- Added
  [`compute_rolling_value()`](https://r.tidy-finance.org/reference/compute_rolling_value.md).
- Added `output` parameter to
  [`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md)
  to also return t-stats or residuals.
- Added
  [`join_lagged_values()`](https://r.tidy-finance.org/reference/join_lagged_values.md).
- Added more indexes to
  [`list_supported_indexes()`](https://r.tidy-finance.org/reference/list_supported_indexes.md).
- Added
  [`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md)
  and
  [`get_available_huggingface_files()`](https://r.tidy-finance.org/reference/get_available_huggingface_files.md).
  and support for `type = "hf_high_frequency_sp500"`.
- Deprecated `type` parameter in favor of `domain` and `dataset`.
- Added `detail` parameter to
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md)
  to include average `n_obs`, `r_squared`, and `adj_r_squared`.
- Removed lower bound of excess returns in
  [`download_data_wrds_crsp()`](https://r.tidy-finance.org/reference/download_data_wrds_crsp.md).
- Removed `add_lag_columns()` in favor of
  [`add_lagged_columns()`](https://r.tidy-finance.org/reference/add_lagged_columns.md).
- Added domain `"tidyfinance"` with datasets `"high_frequency_sp500"`,
  `"factor_library"`, and `"risk_free"`.

### Improvements

- Removed `renv` due to lack of benefits.
- Moved optional dependencies to imports for improved user experience
  (except for `furrr`).

### Bug fixes

- Removed erroneous time zone adjustment in
  [`download_data_wrds_trace_enhanced()`](https://r.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md)
  [\#133](https://github.com/tidy-finance/r-tidyfinance/issues/133).
- Replaced tabs in `list_supported_types_ff()` with underscores
  [\#134](https://github.com/tidy-finance/r-tidyfinance/issues/134).
- [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md)
  and
  [`implement_portfolio_sort()`](https://r.tidy-finance.org/reference/implement_portfolio_sort.md)
  now apply `min_portfolio_size` to the reported portfolio
  cross-section. For bivariate sorts this is the firm count per
  `(main_portfolio, date)` summed across secondary buckets, not per
  `(main, secondary, date)` cell as before. Previously, setting
  `min_portfolio_size` to the number of cells
  (e.g. `n_main * n_secondary`) silently voided every cell. Univariate
  behaviour is unchanged. The default has changed from `0L` to `1L`, so
  each reported portfolio is required to have at least one observation
  by default; pass `min_portfolio_size = 0L` to deactivate the check.
  The param documentation has also been corrected to reflect that small
  portfolios receive `NA` (not zero).
- [`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md)
  no longer errors with `object 'top' not found` when the input panel
  contains only one distinct portfolio (e.g., because
  [`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md)
  collapsed to a single bucket on a constant sorting variable). The
  long-short return is now `NA` on such dates, consistent with “no
  investment, no return”, instead of crashing.

## tidyfinance 0.4.5

CRAN release: 2026-01-08

### Bug fixes

- Updated download of FRED data due to API changes.

## tidyfinance 0.4.4

CRAN release: 2025-05-07

### Bug fixes

- Removed user agent sampling from `download_stock_prices()`because they
  were blocked.

## tidyfinance 0.4.3.

CRAN release: 2024-12-17

### Bug fixes

- `download_constituents()` and `download_stock_prices()` now also fail
  gracefully with informative messages instead of errors or warnings.
- `download_factors()` returns empty data frame with `date` column to
  ensure vignettes are built even if resources are unavailable.

### Improvements

- Unified `start_date` and `end_date` validation across applications.
- Updated tests of `download_*()` functions to cover unavailable or
  broken resources.

## tidyfinance 0.4.2

CRAN release: 2024-12-02

### New features

- Added experimental `add_lag_columns()` function that is more efficient
  than `lag_column()`

### Bug fixes

- `download_macro_predictors()`, `download_factors()`, and
  `download_osap()` now fail gracefully with informative messages
  instead of errors or warnings.

### Improvements

- Updated `ccmxpf_linktable` to the new WRDS default `ccmxpf_lnkhist`.
- Added support for “factors_q5_annual” in `download_factors_q()`
- Optimized
  [`winsorize()`](https://r.tidy-finance.org/reference/winsorize.md) by
  reducing quantile recalculations

## tidyfinance 0.4.1

CRAN release: 2024-09-04

### Bug fixes

- Added missing support of “wrds_trace_enhanced” and “wrds_fisd” support
  to
  [`download_data_wrds()`](https://r.tidy-finance.org/reference/download_data_wrds.md).
- Added intercept to
  [`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md),
  [`estimate_betas()`](https://r.tidy-finance.org/reference/estimate_betas.md),
  and
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md).

### Improvements

- Renamed `download_data_wrds_clean_trace()` to
  [`download_data_wrds_trace_enhanced()`](https://r.tidy-finance.org/reference/download_data_wrds_trace_enhanced.md)
  for improved consistency.
- Added `vcov_options` parameter to
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md).

## tidyfinance 0.4.0

CRAN release: 2024-08-30

### New features

- Added
  [`list_supported_indexes()`](https://r.tidy-finance.org/reference/list_supported_indexes.md)
  and
  [`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md)
  to download index constituents.
- Added
  [`estimate_betas()`](https://r.tidy-finance.org/reference/estimate_betas.md)
  to estimate risk factor betas.
- Added
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md)
  to estimate Fama-MacBeth models.
- Added
  [`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md)
  to download index constituents.
- Added
  [`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md)
  to download data from Open Source Asset Pricing.
- Added
  [`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md)
  to download data from Federal Reserve Economic Data.
- Added
  [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md)
  to implement different portfolio sorting approaches.
- Added
  [`compute_long_short_returns()`](https://r.tidy-finance.org/reference/compute_long_short_returns.md)
  to quickly compute long-short portfolio returns.
- Added
  [`compute_breakpoints()`](https://r.tidy-finance.org/reference/compute_breakpoints.md)
  to make
  [`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md)
  more flexible.
- Added
  [`breakpoint_options()`](https://r.tidy-finance.org/reference/breakpoint_options.md)
  and
  [`data_options()`](https://r.tidy-finance.org/reference/data_options.md)
  to provide more flexibility with respect to column names.

### Bug fixes

- Retained explicit missing values in `mktcap_lag` in monthly CRSP.

### Improvements

- Migrated to `cli` for error messages and warnings.
- Aligned documentation across functions.
- Switched to `NULL` for optional default values.
- Removed dependency from named placeholder that is only available from
  R 4.2 on.
- Removed `readxl` dependency from
  [`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md).
- Removed redundant `check_if_package_installed()` function.
- Updated
  [`estimate_model()`](https://r.tidy-finance.org/reference/estimate_model.md)
  to support both
  [`estimate_betas()`](https://r.tidy-finance.org/reference/estimate_betas.md)
  and
  [`estimate_fama_macbeth()`](https://r.tidy-finance.org/reference/estimate_fama_macbeth.md).
- Updated
  [`assign_portfolio()`](https://r.tidy-finance.org/reference/assign_portfolio.md)
  to support
  [`compute_portfolio_returns()`](https://r.tidy-finance.org/reference/compute_portfolio_returns.md).
- Renamed `download_data_stocks()` to
  [`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md)
  for better naming.

## tidyfinance 0.3.0

CRAN release: 2024-07-23

### New features

- Added support for all available Fama-French datasets (check via
  [`list_supported_types()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)).
  All type names are created from a string cleaning algorithm and are
  hence more consistent. We kept implicit support for legacy type names
  to avoid breaking existing code.
- Added new function to download stock data from Yahoo Finance:
  `download_data_stocks()`.
- Added support for `wrds_compustat_quarterly`.

### Bug fixes

- CRSP monthly data always contains the historically accurate stock
  characteristics instead of the oft misleading most recent information.
- Consistently implemented the `additional_columns` option for CRSP and
  Compustat instead of having the error prone option to pass columns via
  `...`.
- Added replacement of `-999` by NA in Fama-French types, which was
  missing in the initial implementation.

### Improvements

- Refactored the column name cleaning procedure in
  `download_data_factors()` to support all available column names in the
  Fama-French universe.
- Made all `start_date` and `end_date` optional with a message to user
  which dates are used as defaults.
- Introduced automatic checks via GitHub Actions workflows.
- Synchronized `date` column and its references across WRDS types (see
  corresponding vignette for more information).
- Improved handling of imports with `tidyfinance-package.R` file.
- Reformatted DESCRIPTION and roxygen comments for more consistency with
  `tidyverse` style.

## tidyfinance 0.2.1

CRAN release: 2024-07-03

### New features

- Added `domain` and `as_vector` parameters to
  [`list_supported_types()`](https://r.tidy-finance.org/reference/list_supported_datasets.md)

### Bug fixes

- Replaced `...` with `additional_columns` parameter and ensured that
  CRSP and Compustat types consider it correctly
- Removed `mkt_excess` column from type “wrds_crsp_monthly”

### Improvements

- Added `fixed = TRUE` to [`grepl()`](https://rdrr.io/r/base/grep.html)
  calls with fixed strings
- Switched to `NA_real_` instead of `as.double(NA)`
- Switched to [`toString()`](https://rdrr.io/r/base/toString.html)
  instead of [`paste0()`](https://rdrr.io/r/base/paste.html) with
  collapse
- Switched to
  [`dplyr::between()`](https://dplyr.tidyverse.org/reference/between.html)
  instead of unequal signs

## tidyfinance 0.2.0

CRAN release: 2024-05-29

### New features

- Added `vignettes/using-tidyfinance`
- Added
  [`set_wrds_credentials()`](https://r.tidy-finance.org/reference/set_wrds_credentials.md)
  function for a guided tour to store login data
- Added support for `"factors_ff_industry_*"` data types

### Bug fixes

- Removed `hml` and `smb` columns from `"wrds_crsp_monthly"` output
- Fixed stock filters for `"v2"` of `"wrds_crsp_*"` data types

### Improvements

- Relaxed package version requirements as much as possible with the
  current set of packages
- Split up the `download_data*` functions into multiple files for better
  maintenance

## tidyfinance 0.1.0

CRAN release: 2024-03-05

- Initial CRAN submission.
