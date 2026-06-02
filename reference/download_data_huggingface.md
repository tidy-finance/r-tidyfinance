# Download data from a Hugging Face dataset

Downloads data from a supported Hugging Face dataset. For
`"high_frequency_sp500"`, parquet files are filtered by date range and
row-bound. For `"factor_library"`, portfolio characteristics are
selected via `filter_factor_library_grid()`, the matching return data is
downloaded, and the result is filtered to `start_date`/`end_date` when
both are supplied. For `"factor_library_grid"`, the grid itself is
returned via
[`download_factor_library_grid()`](https://package.tidy-finance.org/reference/download_factor_library_grid.md).

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
  `"high_frequency_sp500"`, `"factor_library"`, and
  `"factor_library_grid"`.

- start_date:

  Date or character. Start date (inclusive) in `"YYYY-MM-DD"` format.
  Used for `"high_frequency_sp500"` and `"factor_library"`. When omitted
  for `"factor_library"`, the full return history is returned;
  `"high_frequency_sp500"` falls back to a built-in sample window.

- end_date:

  Date or character. End date (inclusive) in `"YYYY-MM-DD"` format. See
  `start_date`.

- type:

  **\[deprecated\]** Use `dataset` instead.

- ...:

  For `dataset = "factor_library"`: either named arguments used to
  filter the portfolio grid, or `ids = <vector>` to bypass the grid
  filter and download specific portfolios directly via
  [`download_factor_library_ids()`](https://package.tidy-finance.org/reference/download_factor_library_ids.md).
  Filter arguments take the form `column = value`, where `value` may be
  a vector to match multiple levels. Optionally pass `fill_all = TRUE`
  to leave unspecified columns unrestricted (default: `FALSE`, i.e.
  unspecified columns are fixed at the defaults listed below). Passing
  `NULL` for any parameter removes that filter entirely, returning all
  values for that column (e.g., `min_size_quantile = NULL` includes all
  size groups). Passing an unrecognised column name raises an error
  listing the supported names. `ids` cannot be combined with filter
  arguments. Ignored when `dataset != "factor_library"`. See the Details
  section for supported columns and their defaults.

## Value

A tibble with the downloaded data. For `"high_frequency_sp500"`,
contains 5-second aggregated orderbook snapshots filtered to the
requested date range. For `"factor_library"`, contains portfolio return
data joined with the full grid metadata for the matched portfolio IDs.

## Details

**Note on `dataset = "factor_library"` defaults:** The defaults below
reflect one common portfolio construction choice, but may not suit every
research question. Always verify that the selected combination matches
your intended design.

Supported columns and their defaults for `...`:

- `sorting_variable`: **Required.** The firm characteristic used to sort
  stocks into portfolios (e.g., `"me"` for market equity, `"bm"` for
  book-to-market). No default is applied.

- `min_size_quantile` (defaults to `0.2`): Fraction of the smallest
  stocks (by market cap) excluded from the portfolio universe. `0.2`
  drops the bottom 20%.

- `exclude_financials` (defaults to `FALSE`): Whether to drop
  financial-sector stocks (SIC 6000-6999) from the universe.

- `exclude_utilities` (default: `FALSE`): Whether to drop utility-sector
  stocks (SIC 4900-4999) from the universe.

- `exclude_negative_earnings` (defaults to `FALSE`): Whether to drop
  firms with negative earnings before sorting.

- `sorting_variable_lag` (defaults to `"6m"`): Lag applied to the
  sorting variable before portfolio assignment (e.g., `"6m"` = 6-month
  lag).

- `rebalancing` (defaults to `"monthly"`): How frequently portfolios are
  reformed: `"monthly"` or `"annual"`.

- `n_portfolios_main` (defaults to `10`): Number of quantile groups
  (e.g., `10` for decile portfolios).

- `sorting_method` (defaults to `"univariate"`): Whether portfolios are
  formed on a single sort (`"univariate"`) or a sequential double sort
  (`"sequential"`).

- `n_portfolios_secondary` (defaults to `NULL`): Number of groups for
  the secondary sort variable. Required when `sorting_method` is not
  `"univariate"`.

- `breakpoints_exchanges` (defaults to: `"NYSE"`): Exchange(s) used to
  compute breakpoints. `"NYSE"` uses only NYSE-listed stocks to define
  quantile cutoffs (the conventional Fama-French approach).

- `breakpoints_min_size_threshold` (defaults to `NULL`): Minimum
  market-cap threshold (in USD) applied when computing breakpoints.
  `NULL` means no minimum-size screen is applied.

- `weighting_scheme` (defaults to `"VW"`): Return weighting within
  portfolios: `"VW"` for value-weighted or `"EW"` for equal-weighted.

## See also

Other download functions:
[`download_data()`](https://package.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://package.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://package.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://package.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://package.tidy-finance.org/reference/download_data_fred.md),
[`download_data_macro_predictors()`](https://package.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_osap()`](https://package.tidy-finance.org/reference/download_data_osap.md),
[`download_data_risk_free()`](https://package.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stock_prices()`](https://package.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://package.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://package.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  download_data_huggingface(
    "high_frequency_sp500", "2007-07-26", "2007-07-27"
  )
  download_data_huggingface(
    "factor_library",
    sorting_variable = "52w",
    rebalancing = "annual"
  )
  download_data_huggingface(
    "factor_library", sorting_variable = "ag", fill_all = TRUE
  )
  download_data_huggingface(
    "factor_library",
    sorting_variable = "me",
    start_date = "2000-01-01",
    end_date = "2020-12-31"
  )
  download_data_huggingface("factor_library", ids = c(1L, 2L, 3L))
} # }
```
