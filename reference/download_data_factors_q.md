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
download_data_factors_q("q5_factors_daily_2024", "2020-01-01", "2020-12-31")
#> Error in mutate(raw_data, DATE = ymd(.data$DATE)): ℹ In argument: `DATE = ymd(.data$DATE)`.
#> Caused by error in `.data$DATE`:
#> ! Column `DATE` not found in `.data`.
download_data_factors_q("q5_factors_annual_2024")
#> No `start_date` or `end_date` provided. Returning the full data set.
#> Error in mutate(raw_data, date = .data$year): ℹ In argument: `date = .data$year`.
#> Caused by error in `.data$year`:
#> ! Column `year` not found in `.data`.
# }
```
