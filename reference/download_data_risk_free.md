# Download Risk-Free Rate Data

Downloads pre-processed risk-free rate data from the
`tidy-finance/risk-free` dataset on HuggingFace. The dataset is updated
monthly via a scheduled GitHub Actions workflow that splices the 3-Month
Treasury Bill Secondary Market Rate (pre-2001) with the 4-Week Treasury
Bill Secondary Market Rate (from 2001 onwards) sourced from FRED. For
monthly data, the monthly TB3MS series is spliced with the daily DTB4WK
series aggregated to month-end. For daily data, the daily DTB3 series is
spliced with the daily DTB4WK series, both at the business-day frequency
provided by FRED.

## Usage

``` r
download_data_risk_free(
  start_date = NULL,
  end_date = NULL,
  frequency = "monthly"
)
```

## Arguments

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the data. If not provided, the full
  dataset is returned.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the data. If not provided, the full
  dataset is returned.

- frequency:

  A character string, either `"monthly"` (default) or `"daily"`,
  specifying the frequency of the returned data. Daily data starts in
  1954-01-04 because of availability of DTB3, while monthly data starts
  in 1934-01-01.

## Value

A tibble with two columns:

- date:

  The date of the observation.

- risk_free:

  The risk-free rate for the period.

## Details

Both series are quoted as annualised bank discount rates on a 360-day
basis. Given an annualised discount rate `d` and a T-bill with `n` days
to maturity, the holding-period return is
`HPR = d * n/360 / (1 - d * n/360)`, which is then converted to the
target period length via `(1 + HPR)^(target/source) - 1`.

The series are spliced at 2001-07-01:

- **Pre-2001**: TB3MS (monthly) or DTB3 (daily), 3-month T-bill with n
  = 90. Monthly conversion uses exponent `1/3`; daily conversion uses
  exponent `1/63` (approx. trading days per quarter).

- **From 2001**: DTB4WK, 4-week T-bill with n = 28. For monthly data,
  the last non-NA observation per calendar month is taken and the
  exponent is `365/(28*12)`. For daily data, observations are used as-is
  and the exponent is `1/20` (approx. trading days per 4-week period).

Business-day gaps in the daily series (e.g. holidays) are handled by
forward-filling the most recent available rate.

Monthly data starts in 1934-01-01 (TB3MS). Daily data starts in
1954-01-04 due to the availability of DTB3.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md),
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  download_data_risk_free("2020-01-01", "2020-12-31")
#> # A data frame: 12 × 2
#>    date       risk_free
#>    <date>         <dbl>
#>  1 2020-01-01 0.00129  
#>  2 2020-02-01 0.00121  
#>  3 2020-03-01 0.0000338
#>  4 2020-04-01 0.0000845
#>  5 2020-05-01 0.000110 
#>  6 2020-06-01 0.000110 
#>  7 2020-07-01 0.0000760
#>  8 2020-08-01 0.0000676
#>  9 2020-09-01 0.0000676
#> 10 2020-10-01 0.0000676
#> 11 2020-11-01 0.0000676
#> 12 2020-12-01 0.0000676
  download_data_risk_free(
    "2020-01-01", "2020-12-31", frequency = "daily"
  )
#> # A data frame: 262 × 2
#>    date       risk_free
#>    <date>         <dbl>
#>  1 2020-01-01 0.0000564
#>  2 2020-01-02 0.0000584
#>  3 2020-01-03 0.0000580
#>  4 2020-01-06 0.0000588
#>  5 2020-01-07 0.0000580
#>  6 2020-01-08 0.0000572
#>  7 2020-01-09 0.0000584
#>  8 2020-01-10 0.0000580
#>  9 2020-01-13 0.0000588
#> 10 2020-01-14 0.0000584
#> # ℹ 252 more rows
# }
```
