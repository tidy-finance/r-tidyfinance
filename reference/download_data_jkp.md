# Download and Process Global Factor Data

Downloads and processes data from [Global Factor
Data](https://jkpfactors.com/data), the public data library accompanying
Jensen, Kelly, and Pedersen (2023). The data are stored as zipped CSV
files (and a few plain CSV reference files) in a public AWS S3 bucket.
For the factor, portfolio, and industry products the function validates
the requested selection against the library's live availability
manifest, then downloads the matching archive, unzips it, aligns monthly
dates to the beginning of the month, and optionally filters by a date
range.

## Usage

``` r
download_data_jkp(
  dataset = "factors",
  region = "usa",
  factors = "all_factors",
  classification = "gics",
  frequency = "monthly",
  weighting = "vw_cap",
  start_date = NULL,
  end_date = NULL
)
```

## Arguments

- dataset:

  The Global Factor Data product to download, one of: `"factors"`
  (characteristic-managed portfolio returns, the default),
  `"portfolios"` (the underlying low/middle/high portfolios that make up
  each long-short factor), `"industry"` (industry returns),
  `"nyse_cutoffs"` (NYSE size breakpoints), or `"return_cutoffs"`
  (return winsorization cutoffs).

- region:

  A character string with the region or country to download, using the
  codes from the availability manifest (e.g., `"usa"`, `"world"`,
  `"developed"`, `"emerging"`, or an ISO-3 country code such as
  `"jpn"`). Defaults to `"usa"`. Ignored for the reference datasets
  `"nyse_cutoffs"` and `"return_cutoffs"`. Call
  [`list_supported_jkp_factors()`](https://r.tidy-finance.org/reference/list_supported_jkp_factors.md)
  to see the available regions.

- factors:

  A character string selecting the factor content for the `"factors"`
  and `"portfolios"` datasets. For `"factors"`: `"mkt"` (the market
  factor), `"all_factors"` (all 153 factors), `"all_themes"` (all 13
  themes), a single theme (e.g., `"value"`, `"momentum"`), or a single
  factor code (e.g., `"be_me"`, `"ret_12_1"`). For `"portfolios"`: a
  single factor code. Defaults to `"all_factors"`. Call
  `list_supported_jkp_factors(region, dataset)` to see the values
  available for a region.

- classification:

  The industry classification for the `"industry"` dataset, either
  `"gics"` (default) or `"ff49"` (Fama-French 49 industries).

- frequency:

  The data frequency, either `"monthly"` (default) or `"daily"`. The
  `"industry"` dataset is only available at monthly frequency. For
  `"return_cutoffs"`, the frequency selects the monthly or daily cutoff
  file.

- weighting:

  The portfolio weighting scheme: `"vw_cap"` (capped value-weighted, the
  default), `"vw"` (value-weighted), or `"ew"` (equal-weighted). Ignored
  for the reference datasets.

- start_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the start date for the data. If not provided, the full
  dataset is returned.

- end_date:

  Optional. A character string or Date object in "YYYY-MM-DD" format
  specifying the end date for the data. If not provided, the full
  dataset is returned.

## Value

A tibble with the processed data. The `date` column is aligned to the
beginning of the month for monthly data, and all returns are plain
numeric (decimal) values. The remaining columns depend on `dataset`: the
`"factors"` data carry `location`, `name`, `freq`, `weighting`,
`direction`, `n_stocks`, `n_stocks_min`, and `ret`; the `"portfolios"`
data add a `pf` portfolio identifier; the `"industry"` data carry the
classification code alongside `ret`; and the reference datasets carry
breakpoint or cutoff columns.

## Details

Returns are already expressed as plain numeric (decimal) values in the
source data, so no rescaling is applied. The data are licensed under CC
BY-NC 4.0 (non-commercial use).

## References

Jensen, T. I., Kelly, B. T., & Pedersen, L. H. (2023). Is there a
replication crisis in finance? *Journal of Finance*, 78(5), 2465-2518.
[doi:10.1111/jofi.13249](https://doi.org/10.1111/jofi.13249)

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
[`download_data_pastor_stambaugh()`](https://r.tidy-finance.org/reference/download_data_pastor_stambaugh.md),
[`download_data_risk_free()`](https://r.tidy-finance.org/reference/download_data_risk_free.md),
[`download_data_stambaugh_yuan()`](https://r.tidy-finance.org/reference/download_data_stambaugh_yuan.md),
[`download_data_stock_prices()`](https://r.tidy-finance.org/reference/download_data_stock_prices.md),
[`download_factor_library_grid()`](https://r.tidy-finance.org/reference/download_factor_library_grid.md),
[`download_factor_library_ids()`](https://r.tidy-finance.org/reference/download_factor_library_ids.md)

## Examples

``` r
# \donttest{
  download_data_jkp(
    region = "usa", factors = "mkt",
    start_date = "2000-01-01", end_date = "2020-12-31"
  )
#> # A tibble: 252 × 9
#>    location name  freq    weighting direction n_stocks n_stocks_min date      
#>    <chr>    <chr> <chr>   <chr>     <lgl>        <int> <lgl>        <date>    
#>  1 usa      mkt   monthly vw_cap    NA            7225 NA           2000-01-01
#>  2 usa      mkt   monthly vw_cap    NA            7179 NA           2000-02-01
#>  3 usa      mkt   monthly vw_cap    NA            7187 NA           2000-03-01
#>  4 usa      mkt   monthly vw_cap    NA            7176 NA           2000-04-01
#>  5 usa      mkt   monthly vw_cap    NA            7193 NA           2000-05-01
#>  6 usa      mkt   monthly vw_cap    NA            7182 NA           2000-06-01
#>  7 usa      mkt   monthly vw_cap    NA            7146 NA           2000-07-01
#>  8 usa      mkt   monthly vw_cap    NA            7159 NA           2000-08-01
#>  9 usa      mkt   monthly vw_cap    NA            7171 NA           2000-09-01
#> 10 usa      mkt   monthly vw_cap    NA            7127 NA           2000-10-01
#> # ℹ 242 more rows
#> # ℹ 1 more variable: ret <dbl>
  download_data_jkp(
    dataset = "portfolios", region = "usa", factors = "be_me",
    start_date = "2000-01-01", end_date = "2020-12-31"
  )
#> # A tibble: 756 × 8
#>    location name     pf     n freq    weighting date            ret
#>    <chr>    <chr> <int> <int> <chr>   <chr>     <date>        <dbl>
#>  1 usa      be_me     3  3469 monthly vw_cap    2000-01-01 -0.0453 
#>  2 usa      be_me     2  1803 monthly vw_cap    2000-01-01 -0.0522 
#>  3 usa      be_me     1  1451 monthly vw_cap    2000-01-01 -0.00878
#>  4 usa      be_me     3  3383 monthly vw_cap    2000-02-01 -0.0614 
#>  5 usa      be_me     2  1879 monthly vw_cap    2000-02-01 -0.00167
#>  6 usa      be_me     1  1495 monthly vw_cap    2000-02-01  0.232  
#>  7 usa      be_me     2  1968 monthly vw_cap    2000-03-01  0.0864 
#>  8 usa      be_me     3  3317 monthly vw_cap    2000-03-01  0.106  
#>  9 usa      be_me     1  1452 monthly vw_cap    2000-03-01 -0.0996 
#> 10 usa      be_me     3  3417 monthly vw_cap    2000-04-01  0.00532
#> # ℹ 746 more rows
  download_data_jkp(
    dataset = "industry", region = "usa", classification = "gics"
  )
#> No `start_date` or `end_date` provided. Returning the full data set.
#> # A tibble: 3,291 × 7
#>     gics date           n location      ret freq    weighting
#>    <int> <date>     <int> <chr>       <dbl> <chr>   <chr>    
#>  1    55 1999-07-01   170 usa      -0.00486 monthly vw_cap   
#>  2    15 1999-07-01   376 usa      -0.0364  monthly vw_cap   
#>  3    20 1999-07-01  1109 usa      -0.0268  monthly vw_cap   
#>  4    10 1999-07-01   278 usa       0.0266  monthly vw_cap   
#>  5    50 1999-07-01   146 usa      -0.00177 monthly vw_cap   
#>  6    30 1999-07-01   305 usa      -0.0241  monthly vw_cap   
#>  7    25 1999-07-01  1305 usa      -0.0499  monthly vw_cap   
#>  8    45 1999-07-01  1430 usa      -0.0199  monthly vw_cap   
#>  9    40 1999-07-01  1185 usa      -0.0537  monthly vw_cap   
#> 10    35 1999-07-01   843 usa      -0.00890 monthly vw_cap   
#> # ℹ 3,281 more rows
  download_data_jkp(dataset = "nyse_cutoffs")
#> No `start_date` or `end_date` provided. Returning the full data set.
#> # A tibble: 1,201 × 6
#>    date           n nyse_p1 nyse_p20 nyse_p50 nyse_p80
#>    <date>     <int>   <dbl>    <dbl>    <dbl>    <dbl>
#>  1 1925-12-01   495   0.075     4.99     15.8     68.8
#>  2 1926-01-01   508   0.116     4.88     16.2     68.1
#>  3 1926-02-01   516   0.112     4.65     14.6     63.1
#>  4 1926-03-01   521   0.123     4.05     13.3     58.2
#>  5 1926-04-01   521   0.129     4.25     13.7     60.1
#>  6 1926-05-01   524   0.118     4.19     13.3     60.7
#>  7 1926-06-01   527   0.188     4.36     14.0     63.1
#>  8 1926-07-01   529   0.15      4.30     13.9     65.5
#>  9 1926-08-01   530   0.169     4.32     14.6     64.6
#> 10 1926-09-01   532   0.184     4.29     13.9     65.0
#> # ℹ 1,191 more rows
# }
```
