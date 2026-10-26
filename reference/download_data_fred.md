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

- series:

  The FRED series ID corresponding to the data.

- value:

  The value of the data series at that date.

## Details

Constructs the URL based on the provided FRED series ID, performs an
HTTP GET request to download the data in CSV format, and processes it to
a tidy tibble format. The resulting tibble includes the date, the series
ID, and the value.

This approach is inspired by `quantmod::getSymbolsFRED()` which uses a
different wrapper around the same FRED download data site. If you want
to systematically download FRED data via API, please consider using the
`fredr` package.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_damodaran()`](https://r.tidy-finance.org/reference/download_data_damodaran.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred_md()`](https://r.tidy-finance.org/reference/download_data_fred_md.md),
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md),
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
# \donttest{
  download_data_fred("CPIAUCNS")
#> No `start_date` or `end_date` provided. Returning the full data set.
#> # A tibble: 1,365 × 3
#>    date       series   value
#>    <date>     <chr>    <dbl>
#>  1 1913-01-01 CPIAUCNS   9.8
#>  2 1913-02-01 CPIAUCNS   9.8
#>  3 1913-03-01 CPIAUCNS   9.8
#>  4 1913-04-01 CPIAUCNS   9.8
#>  5 1913-05-01 CPIAUCNS   9.7
#>  6 1913-06-01 CPIAUCNS   9.8
#>  7 1913-07-01 CPIAUCNS   9.9
#>  8 1913-08-01 CPIAUCNS   9.9
#>  9 1913-09-01 CPIAUCNS  10  
#> 10 1913-10-01 CPIAUCNS  10  
#> # ℹ 1,355 more rows
  download_data_fred(c("GDP", "CPIAUCNS"), "2010-01-01", "2010-12-31")
#> # A tibble: 16 × 3
#>    date       series    value
#>    <date>     <chr>     <dbl>
#>  1 2010-01-01 GDP      14765.
#>  2 2010-04-01 GDP      14980.
#>  3 2010-07-01 GDP      15142.
#>  4 2010-10-01 GDP      15309.
#>  5 2010-01-01 CPIAUCNS   217.
#>  6 2010-02-01 CPIAUCNS   217.
#>  7 2010-03-01 CPIAUCNS   218.
#>  8 2010-04-01 CPIAUCNS   218.
#>  9 2010-05-01 CPIAUCNS   218.
#> 10 2010-06-01 CPIAUCNS   218.
#> 11 2010-07-01 CPIAUCNS   218.
#> 12 2010-08-01 CPIAUCNS   218.
#> 13 2010-09-01 CPIAUCNS   218.
#> 14 2010-10-01 CPIAUCNS   219.
#> 15 2010-11-01 CPIAUCNS   219.
#> 16 2010-12-01 CPIAUCNS   219.
# }
```
