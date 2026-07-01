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
#> # A tibble: 24 × 6
#>    date       mkt_excess      smb     mgmt     perf risk_free
#>    <date>          <dbl>    <dbl>    <dbl>    <dbl>     <dbl>
#>  1 2015-01-01    -0.0311 -0.0271  -0.0227   0.0506          0
#>  2 2015-02-01     0.0613  0.0245  -0.0124  -0.0300          0
#>  3 2015-03-01    -0.0112  0.0214  -0.0192   0.00596         0
#>  4 2015-04-01     0.0059 -0.0260   0.00818 -0.0418          0
#>  5 2015-05-01     0.0136  0.0141  -0.00498  0.0108          0
#>  6 2015-06-01    -0.0153  0.0301  -0.00718  0.00387         0
#>  7 2015-07-01     0.0154 -0.0340   0.00419  0.0706          0
#>  8 2015-08-01    -0.0604 -0.00684  0.00725  0.0133          0
#>  9 2015-09-01    -0.0308 -0.0228   0.0373   0.0790          0
#> 10 2015-10-01     0.0775 -0.0227   0.0240  -0.00612         0
#> # ℹ 14 more rows
  download_data_stambaugh_yuan(
    dataset = "daily", start_date = "2016-01-01", end_date = "2016-12-31"
  )
#> # A tibble: 252 × 6
#>    date       mkt_excess       smb      mgmt     perf risk_free
#>    <date>          <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#>  1 2016-01-04    -0.0159 -0.00858   0.00220  -0.00626         0
#>  2 2016-01-05     0.0012 -0.00460   0.00420   0.00660         0
#>  3 2016-01-06    -0.0135  0.000649  0.00506   0.0297          0
#>  4 2016-01-07    -0.0244 -0.00533   0.00565   0.0202          0
#>  5 2016-01-08    -0.0111 -0.00570  -0.000206 -0.00132         0
#>  6 2016-01-11    -0.0006  0.00272   0.0148    0.0292          0
#>  7 2016-01-12     0.0071 -0.00439   0.00200   0.0162          0
#>  8 2016-01-13    -0.0267 -0.00480   0.0121    0.0174          0
#>  9 2016-01-14     0.0165 -0.00605  -0.0127   -0.0119          0
#> 10 2016-01-15    -0.0214  0.00251   0.00305   0.0139          0
#> # ℹ 242 more rows
# }
```
