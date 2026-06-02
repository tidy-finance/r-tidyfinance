# Generate Pseudo Compustat Data

Returns pseudo Compustat data with the same column layout as
[`download_data_wrds_compustat()`](https://package.tidy-finance.org/reference/download_data_wrds_compustat.md).
Useful for testing and for reproducing the workflow of analyses that
rely on Compustat without a WRDS subscription. The returned values are
simulated and not suitable for inference.

## Usage

``` r
download_data_pseudo_compustat(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  additional_columns = NULL,
  only_usd = FALSE,
  n_assets = 1000L,
  seed = 1234L
)
```

## Arguments

- dataset:

  A string specifying the dataset to simulate. Supported:
  `"compustat_annual"` and `"compustat_quarterly"`.

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the pseudo panel.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the pseudo panel.

- additional_columns:

  Additional Compustat columns to include. Filled with plausible random
  draws so call sites that pass `additional_columns` continue to work;
  the values themselves are not economically meaningful.

- only_usd:

  Accepted for API compatibility with
  [`download_data_wrds_compustat()`](https://package.tidy-finance.org/reference/download_data_wrds_compustat.md);
  the pseudo universe is treated as USD-denominated, so this argument
  has no effect.

- n_assets:

  Integer. Number of pseudo firms in the universe. Defaults to `1000`.

- seed:

  Integer. Random seed; defaults to `1234`. Identical `(seed, n_assets)`
  produces identical output across calls and matches the identifier
  universe used by
  [`download_data_pseudo_crsp()`](https://package.tidy-finance.org/reference/download_data_pseudo_crsp.md)
  and
  [`download_data_pseudo_ccm_links()`](https://package.tidy-finance.org/reference/download_data_pseudo_ccm_links.md).

## Value

For `"compustat_annual"`, a tibble with columns `gvkey`, `date`,
`datadate`, the financial-statement variables `seq`, `ceq`, `at`, `lt`,
`txditc`, `txdb`, `itcb`, `pstkrv`, `pstkl`, `pstk`, `capx`, `oancf`,
`sale`, `cogs`, `xint`, `xsga`, `ib`, `curcd`, plus the derived `be`,
`op`, `at_lag`, `inv`, and any requested `additional_columns`. For
`"compustat_quarterly"`, a tibble with columns `gvkey`, `date`,
`datadate`, `atq`, `ceqq`, and any requested `additional_columns`.

## Details

Both `"compustat_annual"` and `"compustat_quarterly"` are supported.

## See also

Other pseudo functions:
[`download_data_pseudo_ccm_links()`](https://package.tidy-finance.org/reference/download_data_pseudo_ccm_links.md),
[`download_data_pseudo_crsp()`](https://package.tidy-finance.org/reference/download_data_pseudo_crsp.md)

## Examples

``` r
download_data_pseudo_compustat(
  "compustat_annual",
  start_date = "2020-01-01",
  end_date = "2024-12-31",
  n_assets = 20
)
#> # A tibble: 100 × 25
#>    gvkey  date       datadate      at   seq   ceq    lt txditc  txdb   itcb
#>    <chr>  <date>     <date>     <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>
#>  1 010001 2020-12-01 2020-12-31 154.   64.3  64.2  89.6  4.98  3.58  1.40  
#>  2 010001 2021-12-01 2021-12-31 145.   89.8  86.5  55.2  3.69  2.35  1.34  
#>  3 010001 2022-12-01 2022-12-31 117.   56.8  46.1  59.7  2.10  0.588 1.52  
#>  4 010001 2023-12-01 2023-12-31 138.   93.2  91.6  44.4  6.67  2.71  3.96  
#>  5 010001 2024-12-01 2024-12-31  90.0  57.3  53.5  32.7  4.23  0.611 3.61  
#>  6 010002 2020-12-01 2020-12-31  68.9  35.8  29.5  33.1  2.04  1.69  0.355 
#>  7 010002 2021-12-01 2021-12-31 106.   33.2  33.0  72.6  0.672 0.617 0.0544
#>  8 010002 2022-12-01 2022-12-31 119.   69.5  64.8  49.7  3.67  0.304 3.37  
#>  9 010002 2023-12-01 2023-12-31 103.   57.5  51.6  45.5  2.07  1.60  0.468 
#> 10 010002 2024-12-01 2024-12-31 153.   96.3  93.8  56.9  1.81  0.732 1.08  
#> # ℹ 90 more rows
#> # ℹ 15 more variables: pstkrv <dbl>, pstkl <dbl>, pstk <dbl>, capx <dbl>,
#> #   oancf <dbl>, sale <dbl>, cogs <dbl>, xsga <dbl>, xint <dbl>, ib <dbl>,
#> #   curcd <chr>, be <dbl>, op <dbl>, at_lag <dbl>, inv <dbl>
download_data_pseudo_compustat(
  "compustat_quarterly",
  start_date = "2020-01-01",
  end_date = "2024-12-31",
  n_assets = 20
)
#> # A tibble: 400 × 5
#>    gvkey  date       datadate     atq  ceqq
#>    <chr>  <date>     <date>     <dbl> <dbl>
#>  1 010001 2020-03-01 2020-03-31  94.7  40.7
#>  2 010001 2020-06-01 2020-06-30  71.3  25.2
#>  3 010001 2020-09-01 2020-09-30  63.4  27.8
#>  4 010001 2020-12-01 2020-12-31  83.0  34.4
#>  5 010001 2021-03-01 2021-03-31  96.7  21.4
#>  6 010001 2021-06-01 2021-06-30  82.1  20.1
#>  7 010001 2021-09-01 2021-09-30  72.3  16.8
#>  8 010001 2021-12-01 2021-12-31  73.5  38.7
#>  9 010001 2022-03-01 2022-03-31  84.6  18.4
#> 10 010001 2022-06-01 2022-06-30  81.8  38.6
#> # ℹ 390 more rows
```
