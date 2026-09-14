# Download factor library returns for a vector of portfolio IDs

Given a vector of portfolio IDs from the
`tidy-finance/factor-library-grid` Hugging Face dataset, downloads the
corresponding return data from the `tidy-finance/factor-library` dataset
on Hugging Face. The returns are stored in files of 1,000 consecutive
IDs named after the range they cover (e.g.,
`id_0000001-0001000.parquet`), so the function downloads only the files
that hold the requested IDs. The grid metadata is joined onto the
result.

## Usage

``` r
download_factor_library_ids(ids)
```

## Arguments

- ids:

  Integer or numeric vector of portfolio IDs to download. IDs correspond
  to rows of the `tidy-finance/factor-library-grid` dataset.

## Value

A tibble with the columns `id`, `date`, and `ret` (the monthly
long-short excess return) and the grid metadata columns for the
requested IDs.

## Details

Use this function when you already know the portfolio IDs you want (for
example, from a previous call to
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md)
with `dataset = "factor_library"`). To resolve IDs from filter criteria
(sorting variable, weighting scheme, breakpoints, etc.) and download in
a single call, use
[`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md)
instead.

Raises an error if none of the requested IDs exist in the grid. IDs
whose portfolio sort failed during the construction of the library have
no returns and are absent from the result. Returns are stored in single
precision, and months without a valid long-short return are stored as
`0`.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_factors_ff()`](https://r.tidy-finance.org/reference/download_data_factors_ff.md),
[`download_data_factors_q()`](https://r.tidy-finance.org/reference/download_data_factors_q.md),
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
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  download_factor_library_ids(c(1L, 2L, 3L))
} # }
```
