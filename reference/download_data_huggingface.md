# Download data from a Hugging Face dataset

Downloads data from a supported Hugging Face dataset. For
`"high_frequency_sp500"`, parquet files are filtered by date range and
row-bound. For `"factor_library"` and `"factor_library_costs"`,
portfolio characteristics are selected via
`filter_factor_library_grid()`, the matching returns or trading costs
are downloaded, and the result is filtered to `start_date`/`end_date`
when both are supplied. For `"factor_library_grid"`, the grid itself is
returned via
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md).
For `"factor_library_persistence"`, the persistence of the sorting
variables is returned.

## Usage

``` r
download_data_huggingface(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  ...
)
```

## Arguments

- dataset:

  Character(1). The dataset to download. Supported values are
  `"high_frequency_sp500"`, `"factor_library"`,
  `"factor_library_costs"`, `"factor_library_grid"`, and
  `"factor_library_persistence"`.

- start_date:

  Date or character. Start date (inclusive) in `"YYYY-MM-DD"` format.
  Used for `"high_frequency_sp500"`, `"factor_library"`,
  `"factor_library_costs"`, and `"factor_library_persistence"`. When
  omitted for the factor library, the full history is returned;
  `"high_frequency_sp500"` falls back to a built-in sample window.

- end_date:

  Date or character. End date (inclusive) in `"YYYY-MM-DD"` format. See
  `start_date`.

- type:

  **\[deprecated\]** Use `dataset` instead.

- ...:

  For `dataset = "factor_library"` and `"factor_library_costs"`: either
  named arguments used to filter the portfolio grid, or `ids = <vector>`
  to bypass the grid filter and download specific portfolios directly.
  Filter arguments take the form `column = value`, where `value` may be
  a vector to match multiple levels. Optionally pass `fill_all = TRUE`
  to leave unspecified columns unrestricted (default: `FALSE`, i.e.
  unspecified columns are fixed at the defaults listed below). Passing
  `NULL` for any parameter removes that filter entirely, returning all
  values for that column (e.g., `min_size_quantile = NULL` includes all
  size groups). Passing an unrecognised column name raises an error
  listing the supported names. `ids` cannot be combined with filter
  arguments. See the Details section for supported columns and their
  defaults.

  For all four factor library datasets: `revision` and `grid_revision`
  select the release, see the section Revisions. For
  `"factor_library_grid"`: `refresh`, see
  [`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md).
  For `"factor_library_persistence"`: optionally `sorting_variable`, a
  vector of sorting variables; all are returned without it.

## Value

A tibble with the downloaded data. For `"high_frequency_sp500"`,
contains 5-second aggregated orderbook snapshots filtered to the
requested date range. For `"factor_library"`, contains the columns `id`,
`date`, and `ret` joined with the full grid metadata for the matched
portfolio IDs. For `"factor_library_costs"`, contains the columns `id`,
`date`, `turnover`, `cost`, and `microcap_share` joined with the grid
metadata in the same way. The costs start in January 1994; `turnover`
and `cost` are `0` in months without trades and `microcap_share` is `NA`
in months without holdings. For `"factor_library_persistence"`, contains
the columns `sorting_variable`, `sorting_variable_lag`, `rebalancing`,
`date`, and `persistence`, the Spearman correlation across stocks
between the value of the sorting variable under the lag and rebalancing
and its up-to-date value.

## Details

**Note on `dataset = "factor_library"` defaults:** The defaults below
reflect one common portfolio construction choice, but may not suit every
research question. Always verify that the selected combination matches
your intended design.

Supported columns and their defaults for `...`:

- `sorting_variable`: **Required.** The firm characteristic used to sort
  stocks into portfolios, named like the Open Source Asset Pricing
  signals (e.g., `"size"` for market equity, `"bm"` for book-to-market).
  See
  [`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md)
  for all values. No default is applied.

- `min_size_quantile` (defaults to `0.2`): Fraction of the smallest
  stocks (by market cap) excluded from the portfolio universe. `0.2`
  drops the bottom 20%.

- `exclude_financials` (defaults to `FALSE`): Whether to drop
  financial-sector stocks (SIC 6000-6799) from the universe.

- `exclude_utilities` (default: `FALSE`): Whether to drop utility-sector
  stocks (SIC 4900-4999) from the universe.

- `exclude_negative_earnings` (defaults to `FALSE`): Whether to drop
  firms with negative earnings before sorting.

- `sorting_variable_lag` (defaults to `"6m"`): Lag applied to the
  sorting variable before portfolio assignment: `"1m"` (the timing of
  Open Source Asset Pricing), `"3m"`, `"6m"`, or `"ff"` (Fama-French).

- `rebalancing` (defaults to `"monthly"`): How frequently portfolios are
  reformed: `"monthly"` or `"annual"`.

- `n_portfolios_main` (defaults to `10`): Number of quantile groups
  (e.g., `10` for decile portfolios).

- `sorting_method` (defaults to `"univariate"`): Whether portfolios are
  formed on a single sort (`"univariate"`) or on a double sort with size
  as the second variable (`"bivariate-dependent"` or
  `"bivariate-independent"`).

- `n_portfolios_secondary` (defaults to `NULL`): Number of size groups
  for the secondary sort. Required when `sorting_method` is not
  `"univariate"`.

- `breakpoints_exchanges` (defaults to: `"NYSE"`): Exchange(s) used to
  compute breakpoints. `"NYSE"` uses only NYSE-listed stocks to define
  quantile cutoffs (the conventional Fama-French approach).

- `breakpoints_min_size_threshold` (defaults to `NA`): Minimum size
  quantile of the stocks that set the main breakpoints (e.g., `0.2`).
  `NA` means no minimum-size screen is applied.

- `weighting_scheme` (defaults to `"VW"`): Return weighting within
  portfolios: `"VW"` for value-weighted, `"EW"` for equal-weighted, or
  `"capped VW"` for value-weighted with capped weights.

## Revisions

The factor library is published in two Hugging Face datasets.
`tidy-finance/factor-library` holds the returns and the trading costs.
`tidy-finance/factor-library-grid` holds the grid and the persistence.
Each release is a commit of both datasets. `revision` selects the commit
of `tidy-finance/factor-library`, and `grid_revision` the commit of
`tidy-finance/factor-library-grid`. A revision is a commit hash, a
branch, or a tag, and both default to `"main"`, the latest release. The
grid and the persistence take only `grid_revision`.

To make a download reproducible, pin both to the commits of one release.
The IDs of one release need not match the grid of another.

## Download limits

Hugging Face allows anonymous users 3,000 file downloads per five
minutes and IP address. The returns and the costs are stored in files of
1,000 portfolio IDs, so all portfolios of one sorting variable take
about 27 files of each. The factor library downloads up to 16 files at
once and 8 files per second on average, also across calls, which keeps
it below the limit. If Hugging Face still refuses a download with HTTP
429, e.g. because of other downloads from the same IP address, the
download waits until the limit resets and tries again, twice at most.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_damodaran()`](https://r.tidy-finance.org/reference/download_data_damodaran.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md),
[`download_data_fred_md()`](https://r.tidy-finance.org/reference/download_data_fred_md.md),
[`download_data_jkp()`](https://r.tidy-finance.org/reference/download_data_jkp.md),
[`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md),
[`download_data_pastor_stambaugh()`](https://r.tidy-finance.org/reference/download_data_pastor_stambaugh.md),
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stambaugh_yuan()`](https://r.tidy-finance.org/reference/download_data_stambaugh_yuan.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  download_data_huggingface(
    "high_frequency_sp500", "2007-07-26", "2007-07-27"
  )
  download_data_huggingface(
    "factor_library",
    sorting_variable = "high52",
    rebalancing = "annual"
  )
  download_data_huggingface(
    "factor_library", sorting_variable = "assetgrowth", fill_all = TRUE
  )
  download_data_huggingface(
    "factor_library",
    sorting_variable = "size",
    start_date = "2000-01-01",
    end_date = "2020-12-31"
  )
  download_data_huggingface("factor_library", ids = c(1L, 2L, 3L))

  # All portfolios of one sorting variable from the release of
  # 20 October 2026
  download_data_huggingface(
    "factor_library_costs",
    sorting_variable = "bm",
    fill_all = TRUE,
    revision = "e47bd85accd9b9b168ac7f478f12fe89ba5c2430",
    grid_revision = "a5a74cdf1df0c2c3d1ce3d7bf6403a8386fbf99d"
  )
  download_data_huggingface(
    "factor_library_persistence",
    sorting_variable = "bm",
    grid_revision = "a5a74cdf1df0c2c3d1ce3d7bf6403a8386fbf99d"
  )
} # }
```
