# Download and Process Stambaugh-Yuan Mispricing Factors

Downloads and processes the mispricing factor data of Stambaugh and Yuan
(2017) from [Stambaugh's data
library](https://finance.wharton.upenn.edu/~stambaug/). The four-factor
model (M4) combines the market and size factors with two mispricing
factors, `mgmt` (management) and `perf` (performance). The function
downloads the requested frequency, aligns the date, renames the columns
to the package conventions, and optionally filters the data based on a
provided date range.

## Usage

``` r
download_data_stambaugh_yuan(
  dataset = "monthly",
  start_date = NULL,
  end_date = NULL,
  url = "https://finance.wharton.upenn.edu/~stambaug/"
)
```

## Arguments

- dataset:

  The data frequency to download, either `"monthly"` (the default) or
  `"daily"`.

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the data. If not provided, the full
  dataset is returned.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the data. If not provided, the full
  dataset is returned.

- url:

  The base URL from which to download the dataset files. The file name
  (`M4.csv` or `M4d.csv`) is appended based on `dataset`.

## Value

A tibble with the columns `date` (aligned to the beginning of the month
for monthly data), `mkt_excess` (the market excess return), `smb`
(size), `mgmt` (the management mispricing factor), `perf` (the
performance mispricing factor), and `risk_free` (the risk-free rate).
All returns are plain numeric (decimal) values, filtered by the
specified date range if `start_date` and `end_date` are provided.

## Details

Returns are already expressed as plain numeric (decimal) values in the
source data, so no rescaling is applied. The source files currently end
in December 2016; a requested date range that lies entirely outside the
available data emits a warning and returns an empty tibble.

## References

Stambaugh, R. F., & Yuan, Y. (2017). Mispricing factors. *Review of
Financial Studies*, 30(4), 1270-1315.
[doi:10.1093/rfs/hhw107](https://doi.org/10.1093/rfs/hhw107)

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md),
[`download_data_fred_md()`](https://r.tidy-finance.org/reference/download_data_fred_md.md),
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_jkp()`](https://r.tidy-finance.org/reference/download_data_jkp.md),
[`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md),
[`download_data_pastor_stambaugh()`](https://r.tidy-finance.org/reference/download_data_pastor_stambaugh.md),
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  download_data_stambaugh_yuan(
    start_date = "2015-01-01", end_date = "2016-12-31"
  )
#> Failed to download or process the resource. The resource may not be available,
#> or the URL may have changed. Error message: cannot open the connection to
#> 'https://finance.wharton.upenn.edu/~stambaug/M4.csv'
#> Returning an empty data set due to download failure.
#> # A tibble: 0 × 0
  download_data_stambaugh_yuan(
    dataset = "daily", start_date = "2016-01-01", end_date = "2016-12-31"
  )
#> Failed to download or process the resource. The resource may not be available,
#> or the URL may have changed. Error message: cannot open the connection to
#> 'https://finance.wharton.upenn.edu/~stambaug/M4d.csv'
#> Returning an empty data set due to download failure.
#> # A tibble: 0 × 0
# }
```
