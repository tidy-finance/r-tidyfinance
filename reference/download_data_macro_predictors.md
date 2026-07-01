# Download and Process Macro Predictor Data

Downloads and processes macroeconomic predictor data based on the
specified dataset (monthly, quarterly, or annual), date range, and
source URL. The function downloads the data from a Google Sheets export
link. It processes the raw data into a structured format, calculating
additional financial metrics and filtering by the specified date range.

## Usage

``` r
download_data_macro_predictors(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  type = deprecated(),
  sheet_id = "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG"
)
```

## Arguments

- dataset:

  The dataset to download ("monthly", "quarterly", "annual").

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

- sheet_id:

  The Google Sheets ID from which to download the dataset, with the
  default "1bM7vCWd3WOt95Sf9qjLPZjoiafgF_8EG".

## Value

A tibble with processed data, filtered by the specified date range and
including financial metrics.

## References

Welch, I., & Goyal, A. (2008). A comprehensive look at the empirical
performance of equity premium prediction. *Review of Financial Studies*,
21(4), 1455-1508.
[doi:10.1093/rfs/hhm014](https://doi.org/10.1093/rfs/hhm014)

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md),
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_jkp()`](https://r.tidy-finance.org/reference/download_data_jkp.md),
[`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md),
[`download_data_pastor_stambaugh()`](https://r.tidy-finance.org/reference/download_data_pastor_stambaugh.md),
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stambaugh_yuan()`](https://r.tidy-finance.org/reference/download_data_stambaugh_yuan.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  download_data_macro_predictors("monthly")
#> No `start_date` or `end_date` provided. Returning the full data set.
#> # A tibble: 1,164 × 15
#>    date         rp_div    dp    dy    ep     de     svar    bm   ntis    tbl
#>    <date>        <dbl> <dbl> <dbl> <dbl>  <dbl>    <dbl> <dbl>  <dbl>  <dbl>
#>  1 1926-12-01 -0.0220  -2.97 -2.96 -2.39 -0.586 0.000465 0.441 0.0509 0.0307
#>  2 1927-01-01  0.0422  -2.94 -2.96 -2.37 -0.568 0.000470 0.444 0.0508 0.0323
#>  3 1927-02-01  0.00363 -2.98 -2.93 -2.43 -0.549 0.000287 0.429 0.0517 0.0329
#>  4 1927-03-01  0.0142  -2.98 -2.97 -2.45 -0.531 0.000924 0.470 0.0464 0.032 
#>  5 1927-04-01  0.0459  -2.98 -2.97 -2.47 -0.513 0.000603 0.457 0.0505 0.0339
#>  6 1927-05-01 -0.0112  -3.03 -2.98 -2.53 -0.495 0.000392 0.435 0.0553 0.0333
#>  7 1927-06-01  0.0575  -3.01 -3.02 -2.53 -0.476 0.000825 0.452 0.0588 0.0307
#>  8 1927-07-01  0.0392  -3.06 -3.00 -2.60 -0.457 0.000426 0.415 0.0597 0.0296
#>  9 1927-08-01  0.0388  -3.10 -3.05 -2.66 -0.439 0.00128  0.396 0.0545 0.027 
#> 10 1927-09-01 -0.0543  -3.13 -3.09 -2.71 -0.421 0.00112  0.381 0.0946 0.0268
#> # ℹ 1,154 more rows
#> # ℹ 5 more variables: lty <dbl>, ltr <dbl>, tms <dbl>, dfy <dbl>, infl <dbl>
  download_data_macro_predictors("quarterly", "2000-01-01", "2020-12-31")
#> # A tibble: 84 × 15
#>    date         rp_div    dp    dy    ep     de    svar    bm     ntis    tbl
#>    <date>        <dbl> <dbl> <dbl> <dbl>  <dbl>   <dbl> <dbl>    <dbl>  <dbl>
#>  1 2000-01-01 -0.0437  -4.49 -4.47 -3.38 -1.11  0.0149  0.150  0.0183  0.0569
#>  2 2000-04-01 -0.0268  -4.47 -4.50 -3.33 -1.13  0.0155  0.157  0.00712 0.0569
#>  3 2000-07-01 -0.0984  -4.48 -4.49 -3.29 -1.19  0.00470 0.154  0.00447 0.06  
#>  4 2000-10-01 -0.142   -4.40 -4.48 -3.27 -1.12  0.0141  0.152 -0.00226 0.0577
#>  5 2001-01-01  0.0418  -4.29 -4.41 -3.24 -1.05  0.0146  0.133 -0.00521 0.0442
#>  6 2001-04-01 -0.169   -4.36 -4.30 -3.50 -0.852 0.0115  0.125  0.00504 0.0349
#>  7 2001-07-01  0.0900  -4.19 -4.35 -3.60 -0.587 0.0123  0.149  0.00865 0.0264
#>  8 2001-10-01 -0.00483 -4.29 -4.19 -3.84 -0.450 0.00724 0.131  0.0135  0.0169
#>  9 2002-01-01 -0.150   -4.29 -4.29 -3.84 -0.451 0.00677 0.237  0.0138  0.0179
#> 10 2002-04-01 -0.195   -4.12 -4.27 -3.61 -0.511 0.0101  0.267  0.0275  0.017 
#> # ℹ 74 more rows
#> # ℹ 5 more variables: lty <dbl>, ltr <dbl>, tms <dbl>, dfy <dbl>, infl <dbl>
# }
```
