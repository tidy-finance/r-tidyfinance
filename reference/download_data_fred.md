# Download and Process Data from FRED

Downloads a specified data series from the Federal Reserve Economic Data
(FRED) website, processes the data, and returns it as a tibble.

## Usage

``` r
download_data_fred(series, start_date = NULL, end_date = NULL)
```

## Arguments

- series:

  A character vector specifying the FRED series ID(s) to download.

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the data. If not provided, the full
  dataset is returned.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the data. If not provided, the full
  dataset is returned.

## Value

A tibble containing the processed data with three columns:

- date:

  The date corresponding to the data point.

- value:

  The value of the data series at that date.

- series:

  The FRED series ID corresponding to the data.

## Details

Constructs the URL based on the provided FRED series ID, performs an
HTTP GET request to download the data in CSV format, and processes it to
a tidy tibble format. The resulting tibble includes the date, value, and
the series ID.

This approach is inspired by `quantmod::getSymbolsFRED()` which uses a
different wrapper around the same FRED download data site. If you want
to systematically download FRED data via API, please consider using the
`fredr` package.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
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
  download_data_fred("CPIAUCNS")
#> No `start_date` or `end_date` provided. Returning the full data set.
#> # A tibble: 1,361 × 3
#>    date       value series  
#>    <date>     <dbl> <chr>   
#>  1 1913-01-01   9.8 CPIAUCNS
#>  2 1913-02-01   9.8 CPIAUCNS
#>  3 1913-03-01   9.8 CPIAUCNS
#>  4 1913-04-01   9.8 CPIAUCNS
#>  5 1913-05-01   9.7 CPIAUCNS
#>  6 1913-06-01   9.8 CPIAUCNS
#>  7 1913-07-01   9.9 CPIAUCNS
#>  8 1913-08-01   9.9 CPIAUCNS
#>  9 1913-09-01  10   CPIAUCNS
#> 10 1913-10-01  10   CPIAUCNS
#> # ℹ 1,351 more rows
  download_data_fred(c("GDP", "CPIAUCNS"), "2010-01-01", "2010-12-31")
#> # A tibble: 16 × 3
#>    date        value series  
#>    <date>      <dbl> <chr>   
#>  1 2010-01-01 14765. GDP     
#>  2 2010-04-01 14980. GDP     
#>  3 2010-07-01 15142. GDP     
#>  4 2010-10-01 15309. GDP     
#>  5 2010-01-01   217. CPIAUCNS
#>  6 2010-02-01   217. CPIAUCNS
#>  7 2010-03-01   218. CPIAUCNS
#>  8 2010-04-01   218. CPIAUCNS
#>  9 2010-05-01   218. CPIAUCNS
#> 10 2010-06-01   218. CPIAUCNS
#> 11 2010-07-01   218. CPIAUCNS
#> 12 2010-08-01   218. CPIAUCNS
#> 13 2010-09-01   218. CPIAUCNS
#> 14 2010-10-01   219. CPIAUCNS
#> 15 2010-11-01   219. CPIAUCNS
#> 16 2010-12-01   219. CPIAUCNS
# }
```
