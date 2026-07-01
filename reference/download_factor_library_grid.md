# Download the Factor Library Grid from Hugging Face

Returns the `tidy-finance/factor-library-grid` dataset, which describes
every portfolio construction available in the factor library (one row
per construction, identified by `id`). Use the returned tibble to
discover which `(sorting_variable, weighting_scheme, rebalancing, ...)`
combinations exist before requesting their returns with
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md).

## Usage

``` r
download_factor_library_grid()
```

## Value

A tibble with one row per portfolio construction in the factor library,
including the integer `id` column used by
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md).

## Details

Equivalent to calling
`download_data("Tidy Finance", "factor_library_grid")`.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
[`download_data_fred()`](https://r.tidy-finance.org/reference/download_data_fred.md),
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md),
[`download_data_jkp()`](https://r.tidy-finance.org/reference/download_data_jkp.md),
[`download_data_macro_predictors()`](https://r.tidy-finance.org/reference/download_data_macro_predictors.md),
[`download_data_osap()`](https://r.tidy-finance.org/reference/download_data_osap.md),
[`download_data_pastor_stambaugh()`](https://r.tidy-finance.org/reference/download_data_pastor_stambaugh.md),
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stambaugh_yuan()`](https://r.tidy-finance.org/reference/download_data_stambaugh_yuan.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  download_factor_library_grid()
} # }
```
