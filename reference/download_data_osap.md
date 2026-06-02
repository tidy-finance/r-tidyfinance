# Download and Process Open Source Asset Pricing Data

Downloads the data from [Open Source Asset
Pricing](https://www.openassetpricing.com/data/) from Google Sheets
using a specified sheet ID, processes the data by converting column
names to snake_case, and optionally filters the data based on a provided
date range.

## Usage

``` r
download_data_osap(
  start_date = NULL,
  end_date = NULL,
  sheet_id = "1JyhcF5PRKHcputlioxlu5j5GyLo4JYyY"
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

- sheet_id:

  A character string representing the Google Sheet ID from which to
  download the data. Default is `"1JyhcF5PRKHcputlioxlu5j5GyLo4JYyY"`.

## Value

A tibble containing the processed data. The column names are converted
to snake_case, and the data is filtered by the specified date range if
`start_date` and `end_date` are provided.

## See also

Other download functions:
[`download_data()`](https://package.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://package.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://package.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://package.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://package.tidy-finance.org/reference/download_data_fred.md),
[`download_data_huggingface()`](https://package.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_macro_predictors()`](https://package.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_risk_free()`](https://package.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stock_prices()`](https://package.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://package.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://package.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  osap <- download_data_osap(
    start_date = "2020-01-01", end_date = "2020-06-30"
  )
# }
```
