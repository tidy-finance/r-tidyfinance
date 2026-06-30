# Download and Process Open Source Asset Pricing Data

Downloads the data from [Open Source Asset
Pricing](https://www.openassetpricing.com/data/) from Google Sheets
using a specified sheet ID, processes the data by converting column
names to snake_case, aligning the date to the beginning of the month,
scaling the percentage long-short returns to numeric values, and
optionally filters the data based on a provided date range.

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
to snake_case, the `date` column is aligned to the beginning of the
month, all predictor columns (long-short returns in percent) are divided
by 100 to obtain plain numeric (decimal) returns, and the data is
filtered by the specified date range if `start_date` and `end_date` are
provided.

## Details

The dataset contains monthly long-short returns of the predictor
portfolios. Every column other than `date` is a return expressed in
percent, so all of them are divided by 100 to convert them into plain
numeric (decimal) returns.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md),
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  osap <- download_data_osap(
    start_date = "2020-01-01", end_date = "2020-06-30"
  )
# }
```
