# Generate Pseudo CRSP Data

Returns pseudo CRSP data with the same column layout as
[`download_data_wrds_crsp()`](https://package.tidy-finance.org/reference/download_data_wrds_crsp.md).
Useful for testing and for reproducing the workflow of analyses that
rely on CRSP without a WRDS subscription. The returned values are
simulated and not suitable for inference.

## Usage

``` r
download_data_pseudo_crsp(
  dataset = NULL,
  start_date = NULL,
  end_date = NULL,
  version = "v2",
  additional_columns = NULL,
  add_ccm_links = FALSE,
  adjust_volume = FALSE,
  batch_size = 500,
  n_assets = 1000L,
  seed = 1234L
)
```

## Arguments

- dataset:

  A string specifying the dataset to simulate. Supported:
  `"crsp_monthly"` and `"crsp_daily"`.

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the pseudo panel.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the pseudo panel.

- version:

  Accepted for API compatibility with
  [`download_data_wrds_crsp()`](https://package.tidy-finance.org/reference/download_data_wrds_crsp.md);
  the pseudo schema follows the v2 output.

- additional_columns:

  Additional CRSP columns to include. Filled with plausible random draws
  so call sites that pass `additional_columns` continue to work; the
  values themselves are not economically meaningful.

- add_ccm_links:

  A logical indicating whether CRSP-Compustat links should be appended.
  When `TRUE`, the output gains a `gvkey` column whose values are
  derived from the same pseudo identifier universe used by
  [`download_data_pseudo_ccm_links()`](https://package.tidy-finance.org/reference/download_data_pseudo_ccm_links.md).

- adjust_volume:

  Accepted for API compatibility with
  [`download_data_wrds_crsp()`](https://package.tidy-finance.org/reference/download_data_wrds_crsp.md);
  ignored for pseudo data.

- batch_size:

  Accepted for API compatibility with
  [`download_data_wrds_crsp()`](https://package.tidy-finance.org/reference/download_data_wrds_crsp.md);
  ignored for pseudo data.

- n_assets:

  Integer. Number of pseudo firms in the universe. Defaults to `1000`.

- seed:

  Integer. Random seed; defaults to `1234`. Identical `(seed, n_assets)`
  produces identical output across calls and matches the identifier
  universe used by
  [`download_data_pseudo_compustat()`](https://package.tidy-finance.org/reference/download_data_pseudo_compustat.md)
  and
  [`download_data_pseudo_ccm_links()`](https://package.tidy-finance.org/reference/download_data_pseudo_ccm_links.md).

## Value

For `"crsp_monthly"`, a tibble with columns `permno`, `date`,
`calculation_date`, `ret`, `shrout`, `prc`, `primaryexch`, `siccd`,
`listing_age`, `mktcap`, `mktcap_lag`, `exchange`, `industry`, and
`ret_excess`. For `"crsp_daily"`, a tibble with columns `permno`,
`date`, `ret`, and `ret_excess`. If `add_ccm_links = TRUE`, a `gvkey`
column is appended.

## Details

Both `"crsp_monthly"` and `"crsp_daily"` are supported. The daily panel
uses weekdays (Monday-Friday) only; weekend dates are excluded so the
pseudo calendar approximates a trading-day grid.

## See also

Other pseudo functions:
[`download_data_pseudo_ccm_links()`](https://package.tidy-finance.org/reference/download_data_pseudo_ccm_links.md),
[`download_data_pseudo_compustat()`](https://package.tidy-finance.org/reference/download_data_pseudo_compustat.md)

## Examples

``` r
download_data_pseudo_crsp(
  "crsp_monthly",
  start_date = "2020-01-01",
  end_date = "2024-12-31",
  n_assets = 20
)
#> # A tibble: 1,200 × 14
#>    permno date       calculation_date      ret shrout   prc primaryexch siccd
#>     <int> <date>     <date>              <dbl>  <dbl> <dbl> <chr>       <int>
#>  1      1 2020-01-01 2020-01-31       -0.187   12887. 269.  Q            2118
#>  2      1 2020-02-01 2020-02-29       -0.0861  26252. 202.  Q            2118
#>  3      1 2020-03-01 2020-03-31        0.0599   5872. 346.  Q            2118
#>  4      1 2020-04-01 2020-04-30       -0.0446  45175. 770.  Q            2118
#>  5      1 2020-05-01 2020-05-31       -0.0636  42106. 764.  Q            2118
#>  6      1 2020-06-01 2020-06-30        0.327   21754. 918.  Q            2118
#>  7      1 2020-07-01 2020-07-31        0.243   27680. 836.  Q            2118
#>  8      1 2020-08-01 2020-08-31        0.0724  37520.  27.4 Q            2118
#>  9      1 2020-09-01 2020-09-30       -0.00322 27728. 402.  Q            2118
#> 10      1 2020-10-01 2020-10-31        0.0968  11404. 383.  Q            2118
#> # ℹ 1,190 more rows
#> # ℹ 6 more variables: listing_age <int>, mktcap <dbl>, mktcap_lag <dbl>,
#> #   exchange <chr>, industry <chr>, ret_excess <dbl>
download_data_pseudo_crsp(
  "crsp_daily",
  start_date = "2020-01-01",
  end_date = "2020-03-31",
  n_assets = 20
)
#> # A tibble: 1,300 × 4
#>    permno date            ret ret_excess
#>     <int> <date>        <dbl>      <dbl>
#>  1      1 2020-01-01 -0.0178    -0.0179 
#>  2      1 2020-01-02 -0.00942   -0.00951
#>  3      1 2020-01-03 -0.00485   -0.00492
#>  4      1 2020-01-06  0.0196     0.0195 
#>  5      1 2020-01-07 -0.0328    -0.0330 
#>  6      1 2020-01-08  0.00729    0.00721
#>  7      1 2020-01-09 -0.0241    -0.0241 
#>  8      1 2020-01-10  0.0260     0.0259 
#>  9      1 2020-01-13 -0.0164    -0.0164 
#> 10      1 2020-01-14  0.0120     0.0119 
#> # ℹ 1,290 more rows
```
