# Download factor library returns for a vector of portfolio IDs

Given a vector of portfolio IDs from the
`tidy-finance/factor-library-grid` Hugging Face dataset, downloads the
corresponding return data from the `tidy-finance/factor-library` dataset
on Hugging Face. The function identifies the unique
`(sorting_variable, sorting_variable_lag, sorting_method, n_portfolios_main)`
combinations for the requested IDs, downloads one parquet file per
combination in full, and then inner-joins to retain only the requested
IDs. The grid metadata is joined back onto the result.

## Usage

``` r
download_factor_library_ids(ids)
```

## Arguments

- ids:

  Integer or numeric vector of portfolio IDs to download. IDs correspond
  to rows of the `tidy-finance/factor-library-grid` dataset.

## Value

A tibble of portfolio returns with the grid metadata columns for the
requested IDs appended.

## Details

Use this function when you already know the portfolio IDs you want (for
example, from a previous call to
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md)
with `dataset = "factor_library"`). To resolve IDs from filter criteria
(sorting variable, weighting scheme, breakpoints, etc.) and download in
a single call, use
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md)
instead.

Raises an error if `ids` is empty or contains IDs that cannot be matched
to a parquet file (listing the affected IDs and their key columns).

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
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  download_factor_library_ids(c(1L, 2L, 3L))
} # }
```
