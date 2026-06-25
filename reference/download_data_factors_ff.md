# Download and Process Fama-French Factor Data

Downloads and processes Fama-French factor data based on the specified
dataset name and date range. The data is downloaded directly from
Kenneth French's data library and processed into a structured format,
including date conversion, scaling factor values, and filtering by the
specified date range.

## Usage

``` r
download_data_factors_ff(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated()
)
```

## Arguments

- dataset:

  The name of the Fama-French dataset to download (e.g., "Fama/French 3
  Factors").

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

## Value

A tibble with processed factor data, including the date, risk-free rate,
market excess return, and other factors, filtered by the specified date
range.

## Details

If there are multiple tables in the raw Fama-French data (e.g.,
value-weighted and equal-weighted returns), then the function only
returns the first table because these are the most popular. Download the
source ZIP archive directly if you need less commonly used tables.

## References

Fama, E. F., & French, K. R. (1993). Common risk factors in the returns
on stocks and bonds. *Journal of Financial Economics*, 33(1), 3-56.
[doi:10.1016/0304-405X(93)90023-5](https://doi.org/10.1016/0304-405X%2893%2990023-5)

Fama, E. F., & French, K. R. (2015). A five-factor asset pricing model.
*Journal of Financial Economics*, 116(1), 1-22.
[doi:10.1016/j.jfineco.2014.10.010](https://doi.org/10.1016/j.jfineco.2014.10.010)

Carhart, M. M. (1997). On persistence in mutual fund performance.
*Journal of Finance*, 52(1), 57-82.
[doi:10.1111/j.1540-6261.1997.tb03808.x](https://doi.org/10.1111/j.1540-6261.1997.tb03808.x)

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
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
  download_data_factors_ff(
    "Fama/French 3 Factors", "2000-01-01", "2020-12-31"
  )
#> # A tibble: 252 × 5
#>    date       mkt_excess     smb     hml risk_free
#>    <date>          <dbl>   <dbl>   <dbl>     <dbl>
#>  1 2000-01-01    -0.0474  0.0516 -0.0112    0.0041
#>  2 2000-02-01     0.0246  0.212  -0.0977    0.0043
#>  3 2000-03-01     0.0521 -0.174   0.085     0.0047
#>  4 2000-04-01    -0.0639 -0.06    0.0645    0.0046
#>  5 2000-05-01    -0.0439 -0.0608  0.0459    0.005 
#>  6 2000-06-01     0.0468  0.127  -0.0804    0.004 
#>  7 2000-07-01    -0.0248 -0.0284  0.0806    0.0048
#>  8 2000-08-01     0.0703 -0.005  -0.0137    0.005 
#>  9 2000-09-01    -0.0544 -0.0174  0.0724    0.0051
#> 10 2000-10-01    -0.0278 -0.0385  0.0558    0.0056
#> # ℹ 242 more rows
  download_data_factors_ff(
    "10 Industry Portfolios", "2000-01-01", "2020-12-31"
  )
#> # A tibble: 252 × 11
#>    date         nodur   durbl   manuf   enrgy   hitec   telcm   shops    hlth
#>    <date>       <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 2000-01-01 -0.0472 -0.0133 -0.0866  0.0128 -0.0521 -0.0385 -0.110   0.0763
#>  2 2000-02-01 -0.0627 -0.0881 -0.0374 -0.056   0.180  -0.0369 -0.0395 -0.0251
#>  3 2000-03-01  0.0743  0.109   0.0752  0.119   0.0409  0.0827  0.134  -0.0003
#>  4 2000-04-01 -0.0196  0.0869  0.0146 -0.0231 -0.106  -0.0833 -0.0448  0.0504
#>  5 2000-05-01  0.071  -0.123  -0.0172  0.0939 -0.108  -0.0997 -0.0283  0.0372
#>  6 2000-06-01  0.0218 -0.0899 -0.0128 -0.0505  0.126   0.0399 -0.0221  0.110 
#>  7 2000-07-01  0.0072  0.0473 -0.0145 -0.0268 -0.0515 -0.0674 -0.0122 -0.0651
#>  8 2000-08-01 -0.0208  0.0544  0.0741  0.0875  0.127  -0.0149 -0.024   0.035 
#>  9 2000-09-01  0.0373 -0.0416 -0.057   0.0456 -0.147  -0.0445  0.0266  0.0367
#> 10 2000-10-01  0.0748  0.0255  0.034  -0.0281 -0.0747 -0.0017 -0.0175  0.0206
#> # ℹ 242 more rows
#> # ℹ 2 more variables: utils <dbl>, other <dbl>
# }
```
