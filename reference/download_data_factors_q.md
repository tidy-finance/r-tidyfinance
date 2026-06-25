# Download and Process Global Q Factor Data

Downloads and processes Global Q factor data based on the specified
dataset, date range, and source URL. The processing includes date
conversion, renaming variables to a standardized format, scaling factor
values, and filtering by the specified date range.

## Usage

``` r
download_data_factors_q(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  url = "https://global-q.org/uploads/1/2/2/6/122679606/"
)
```

## Arguments

- dataset:

  The name of the dataset to download (e.g.,
  "q5_factors_daily_2023.csv", "q5_factors_monthly_2023.csv").

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the data. If not provided, the full
  dataset is returned.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the data. If not provided, the full
  dataset is returned.

- type:

  **\[deprecated\]** Use `dataset` instead.

- url:

  The base URL from which to download the dataset files.

## Value

A tibble with processed factor data, including the date, risk-free rate,
market excess return, and other factors, filtered by the specified date
range.

## References

Hou, K., Xue, C., & Zhang, L. (2015). Digesting anomalies: An investment
approach. *Review of Financial Studies*, 28(3), 650-705.
[doi:10.1093/rfs/hhu068](https://doi.org/10.1093/rfs/hhu068)

Hou, K., Mo, H., Xue, C., & Zhang, L. (2019). Which factors? *Review of
Finance*, 23(1), 1-35.
[doi:10.1093/rof/rfy032](https://doi.org/10.1093/rof/rfy032)

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md),
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md),
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
download_data_factors_q("q5_factors_daily_2024", "2020-01-01", "2020-12-31")
#> # A tibble: 253 × 7
#>    date       risk_free mkt_excess       me        ia       roe        eg
#>    <date>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>     <dbl>
#>  1 2020-01-02  0.000055   0.00863  -0.0112  -0.00173   0.000711  0.00340 
#>  2 2020-01-03  0.000055  -0.00673   0.00235 -0.00190  -0.00157   0.000749
#>  3 2020-01-06  0.000055   0.00360  -0.00358 -0.00407  -0.00481   0.000426
#>  4 2020-01-07  0.000055  -0.00192  -0.00138 -0.00322  -0.00512  -0.00224 
#>  5 2020-01-08  0.000055   0.00467  -0.00107 -0.00118   0.00453   0.00635 
#>  6 2020-01-09  0.000055   0.00649  -0.00684 -0.000656  0.00295   0.00510 
#>  7 2020-01-10  0.000055  -0.00335  -0.00237 -0.00206   0.000406  0.00323 
#>  8 2020-01-13  0.000055   0.00730  -0.00200  0.00208   0.00307   0.000787
#>  9 2020-01-14  0.000055  -0.000568  0.00456 -0.00138  -0.00965  -0.00680 
#> 10 2020-01-15  0.000055   0.00164   0.00304 -0.00268  -0.00108   0.00104 
#> # ℹ 243 more rows
download_data_factors_q("q5_factors_annual_2024")
#> No `start_date` or `end_date` provided. Returning the full data set.
#> # A tibble: 58 × 8
#>     date risk_free mkt_excess  year      me       ia      roe      eg
#>    <int>     <dbl>      <dbl> <dbl>   <dbl>    <dbl>    <dbl>   <dbl>
#>  1  1967    0.0415     0.246   19.7  0.406  -0.120    0.202   -0.0366
#>  2  1968    0.0529     0.0882  19.7  0.245   0.151   -0.0205   0.118 
#>  3  1969    0.0659    -0.175   19.7 -0.118   0.00561  0.152    0.128 
#>  4  1970    0.0638    -0.0643  19.7 -0.0837  0.235   -0.00461  0.193 
#>  5  1971    0.0432     0.118   19.7  0.0577  0.0118   0.120    0.0566
#>  6  1972    0.0389     0.129   19.7 -0.0995  0.0454   0.0513   0.149 
#>  7  1973    0.0706    -0.263   19.7 -0.176   0.0801   0.0110   0.173 
#>  8  1974    0.0808    -0.358   19.7  0.0460  0.192    0.117    0.201 
#>  9  1975    0.0582     0.324   19.8  0.172   0.0751  -0.0594   0.112 
#> 10  1976    0.0516     0.218   19.8  0.225   0.0463  -0.0304   0.112 
#> # ℹ 48 more rows
# }
```
