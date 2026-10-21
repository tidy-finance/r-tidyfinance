# Download the Factor Library Grid from Hugging Face

Returns the `tidy-finance/factor-library-grid` dataset, which describes
every portfolio construction available in the factor library (one row
per construction, identified by `id`). Use the returned tibble to
discover which `(sorting_variable, weighting_scheme, rebalancing, ...)`
combinations exist before requesting their returns with
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md).

## Usage

``` r
download_factor_library_grid(refresh = FALSE, grid_revision = "main")
```

## Arguments

- refresh:

  Logical(1). If `TRUE`, the grid is downloaded again and replaces the
  copy cached in the current session. Defaults to `FALSE`.

- grid_revision:

  Character(1). The revision of `tidy-finance/factor-library-grid` to
  read: a commit hash, a branch, or a tag. Defaults to `"main"`, the
  latest release. Pin it to the release of the returns, see the section
  Revisions of
  [`download_data_huggingface()`](https://r.tidy-finance.org/reference/download_data_huggingface.md).

## Value

A tibble with one row per portfolio construction in the factor library,
including the integer `id` column used by
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md).

## Details

Equivalent to calling
`download_data("Tidy Finance", "factor_library_grid")`.

The grid is downloaded once per R session and revision and cached in
memory, so repeated calls (including those made by
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)
and `download_data("Tidy Finance", "factor_library", ...)`) reuse it.
Set `refresh = TRUE` to download it again. Each revision is cached
separately and takes about 450 MB of memory.

## See also

Other download functions:
[`download_data()`](https://r.tidy-finance.org/reference/download_data.md),
[`download_data_constituents()`](https://r.tidy-finance.org/reference/download_data_constituents.md),
[`download_data_damodaran()`](https://r.tidy-finance.org/reference/download_data_damodaran.md),
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
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  download_factor_library_grid()
  download_factor_library_grid(
    grid_revision = "a5a74cdf1df0c2c3d1ce3d7bf6403a8386fbf99d"
  )
} # }
```
