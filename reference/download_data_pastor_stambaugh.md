# Download and Process Pastor-Stambaugh Liquidity Factors

Downloads and processes the liquidity factor data of Pastor and
Stambaugh (2003) from [Pastor's data
library](https://faculty.chicagobooth.edu/lubos-pastor/data). The source
is a whitespace-delimited text file whose header lines start with a
percent sign. The function reads the three liquidity series, aligns the
monthly date to the beginning of the month, and optionally filters the
data based on a provided date range.

## Usage

``` r
download_data_pastor_stambaugh(
  start_date = NULL,
  end_date = NULL,
  url = paste0("https://faculty.chicagobooth.edu/-/media/faculty/lubos-pastor/data/",
    "liq_data_1962_2025.txt")
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

- url:

  A character string with the URL of the liquidity data file. Because
  the file name embeds the last year of data, the default points to the
  most recent file known at release time; override it when a newer file
  becomes available.

## Value

A tibble with the columns `date` (aligned to the beginning of the
month), `agg_liq` (levels of aggregate liquidity), `innov_liq`
(innovations in aggregate liquidity, the non-traded liquidity factor),
and `traded_liq` (the traded liquidity factor LIQ_V), filtered by the
specified date range if `start_date` and `end_date` are provided.

## Details

The series are already expressed as plain numeric (decimal) values in
the source data, so no rescaling is applied. The traded liquidity factor
is only available from 1968 onward; earlier observations are coded as
`-99` in the source file and are returned as `NA`.

## References

Pastor, L., & Stambaugh, R. F. (2003). Liquidity risk and expected stock
returns. *Journal of Political Economy*, 111(3), 642-685.
[doi:10.1086/374184](https://doi.org/10.1086/374184)

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
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stambaugh_yuan()`](https://r.tidy-finance.org/reference/download_data_stambaugh_yuan.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  pastor_stambaugh <- download_data_pastor_stambaugh(
    start_date = "2020-01-01", end_date = "2020-12-31"
  )
# }
```
