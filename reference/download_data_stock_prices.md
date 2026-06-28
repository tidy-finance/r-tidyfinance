# Download Stock Data

Downloads historical stock data from Yahoo Finance for given symbols and
date range.

## Usage

``` r
download_data_stock_prices(symbols, start_date = NULL, end_date = NULL)
```

## Arguments

- symbols:

  A character vector of stock symbols to download data for. At least one
  symbol must be provided.

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the data. If not provided, a one-year
  subset of the dataset is returned (see
  [`validate_dates()`](https://r.tidy-finance.org/reference/validate_dates.md)).

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the data. If not provided, a one-year
  subset of the dataset is returned (see
  [`validate_dates()`](https://r.tidy-finance.org/reference/validate_dates.md)).

## Value

A tibble containing the downloaded stock data with columns: symbol,
date, volume, open, low, high, close, and adjusted_close.

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
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  download_data_stock_prices(c("AAPL", "MSFT"))
#> No `start_date` or `end_date` provided. Using the range 2024-06-28 to
#> 2025-06-28 to avoid downloading large amounts of data.
#> # A tibble: 500 × 8
#>    symbol date         volume  open   low  high close adjusted_close
#>    <chr>  <date>        <dbl> <dbl> <dbl> <dbl> <dbl>          <dbl>
#>  1 AAPL   2024-06-28 82542700  216.  210.  216.  211.           209.
#>  2 AAPL   2024-07-01 60402900  212.  212.  218.  217.           215.
#>  3 AAPL   2024-07-02 58046200  216.  215.  220.  220.           218.
#>  4 AAPL   2024-07-03 37369800  220   219.  222.  222.           220.
#>  5 AAPL   2024-07-05 60412400  222.  222.  226.  226.           224.
#>  6 AAPL   2024-07-08 59085900  227.  223.  228.  228.           226.
#>  7 AAPL   2024-07-09 48076100  228.  226.  229.  229.           227.
#>  8 AAPL   2024-07-10 62627700  229.  229.  233.  233.           231.
#>  9 AAPL   2024-07-11 64710600  231.  226.  232.  228.           226.
#> 10 AAPL   2024-07-12 53046500  229.  229.  233.  231.           229.
#> # ℹ 490 more rows
  download_data_stock_prices("GOOGL", "2021-01-01", "2022-01-01" )
#> # A tibble: 252 × 8
#>    symbol date         volume  open   low  high close adjusted_close
#>    <chr>  <date>        <dbl> <dbl> <dbl> <dbl> <dbl>          <dbl>
#>  1 GOOGL  2021-01-04 37324000  88    85.4  88.1  86.3           85.5
#>  2 GOOGL  2021-01-05 20360000  86.3  85.8  87.3  87.0           86.2
#>  3 GOOGL  2021-01-06 46588000  85.0  84.8  87.2  86.1           85.4
#>  4 GOOGL  2021-01-07 41936000  86.3  86.3  88.9  88.7           87.9
#>  5 GOOGL  2021-01-08 35484000  88.9  88.1  90.0  89.9           89.1
#>  6 GOOGL  2021-01-11 34796000  88.9  87.6  89.2  87.8           87.0
#>  7 GOOGL  2021-01-12 29528000  87.3  85.8  88.4  86.9           86.1
#>  8 GOOGL  2021-01-13 23432000  86.4  86.4  87.8  87.4           86.6
#>  9 GOOGL  2021-01-14 29212000  87.4  86.3  88.4  86.5           85.8
#> 10 GOOGL  2021-01-15 31444000  86.5  85.6  87.4  86.4           85.6
#> # ℹ 242 more rows
# }
```
